# California's power plants, in the style of the week 35 castles map (R/week_35.R) (https://gnoblet.codeberg.page/TidyTuesday/posts/2026/week_35/week_35.html)
# Data: WRI Global Power Plant Database v1.3 (2021)
# Run from the project root: Rscript R/california_power_plants.R

# Packages ----
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)
library(ggtext)
library(ggrepel)
library(showtext)
library(patchwork)
library(sf)
library(rnaturalearth)
library(magick)

out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE)

# Data ----
dat <- read_csv(
  "https://raw.githubusercontent.com/wri/global-power-plant-database/master/output_database/global_power_plant_database.csv",
  col_select = c(country, name, capacity_mw, lat = latitude, lon = longitude, primary_fuel, year = commissioning_year)
)

# Wrangling ----

# Map bbox: room in the Pacific for the LA loupe and the raincloud chart
xmin_ca <- -129
xmax_ca <- -113
ymin_ca <- 31.6
ymax_ca <- 42.6

# Planar geometry is fine at this scale
sf_use_s2(FALSE)

crop_wide <- function(x) {
  st_crop(x, xmin = xmin_ca - 1, xmax = xmax_ca + 1, ymin = ymin_ca - 1, ymax = ymax_ca + 1)
}

# States rather than countries, so California is split from its neighbours
states_crop <- crop_wide(ne_download(scale = 10, type = "admin_1_states_provinces", category = "cultural", returnclass = "sf", load = TRUE))
ca_poly     <- states_crop |> filter(name == "California", admin == "United States of America")
land        <- st_union(states_crop)
coastline   <- st_boundary(land)

# Keep plants inside California. The dataset has no state column, so this is a spatial filter.
plants_usa <- dat |> filter(country == "USA")
inside_ca  <- lengths(st_intersects(st_as_sf(plants_usa, coords = c("lon", "lat"), crs = 4326), ca_poly)) > 0

# Fold the rare fuels into "other" so the palette stays readable
fuel_order <- c("solar", "wind", "gas", "geothermal", "hydro", "other")

plants_ca <- plants_usa[inside_ca, ] |>
  mutate(
    fuel = str_to_lower(primary_fuel),
    fuel = if_else(fuel %in% fuel_order, fuel, "other"),
    # Trim company prefixes and generic suffixes for the labels
    label = name |>
      str_remove("^(AES|Dynegy) ") |>
      str_remove(" (LLC|Power Plant.*|Generating (Plant|Station)|Energy Center.*)$")
  )

# Water lines: rings rippling off all coasts, fading out as they enter the sea.
water_lines <- purrr::map(c(8, 17, 28, 42, 60), function(km) {
  ring <- st_transform(land, 3310) |> st_buffer(km * 1000) |> st_boundary() |> st_transform(4326)
  st_sf(km = km, geometry = ring)
}) |>
  purrr::list_rbind() |>
  st_as_sf() # list_rbind() can drop the sf class depending on sf/vctrs versions

# Major rivers plus Natural Earth's supplementary North American rivers
read_rivers <- function(type, size) {
  r <- ne_download(scale = 10, type = type, category = "physical", returnclass = "sf", load = TRUE)
  st_sf(size = size, geometry = st_geometry(st_make_valid(r)))
}
rivers <- rbind(
  read_rivers("rivers_north_america", "minor"),
  read_rivers("rivers_lake_centerlines", "major")
) |>
  crop_wide()
rivers_ca <- st_intersection(rivers, st_geometry(ca_poly))

# Lake Tahoe, the Salton Sea and friends
lakes <- ne_download(scale = 10, type = "lakes", category = "physical", returnclass = "sf", load = TRUE) |>
  st_make_valid() |>
  crop_wide()

# Biggest plants get a diamond and a label
famous <- plants_ca |>
  arrange(desc(capacity_mw)) |>
  slice_head(n = 12)

# LA loupe. The basin is far too dense to read at state scale
la_lon   <- -118.25
la_lat   <- 34.05
la_r_deg <- 0.45
la_rx    <- la_r_deg / cos(la_lat * pi / 180)

lon_scale <- cos(mean(c(ymin_ca, ymax_ca)) * pi / 180)
loupe_x   <- -126.4
loupe_y   <- 39.4
loupe_r   <- 1.7
mag       <- loupe_r / la_r_deg

to_loupe_x <- function(lon) loupe_x + mag * (lon - la_lon) * cos(la_lat * pi / 180) / lon_scale
to_loupe_y <- function(lat) loupe_y + mag * (lat - la_lat)

in_la <- function(lon, lat) ((lon - la_lon) / la_rx)^2 + ((lat - la_lat) / la_r_deg)^2 <= 1

circle <- function(cx, cy, rx, ry, n = 240) {
  t <- seq(0, 2 * pi, length.out = n)
  tibble(x = cx + rx * cos(t), y = cy + ry * sin(t))
}

la_ring    <- circle(la_lon, la_lat, la_rx, la_r_deg)
loupe_disc <- circle(loupe_x, loupe_y, loupe_r / lon_scale, loupe_r)

plants_la <- plants_ca |>
  filter(in_la(lon, lat)) |>
  mutate(x = to_loupe_x(lon), y = to_loupe_y(lat))

la_labels <- plants_la |>
  arrange(desc(capacity_mw)) |>
  slice_head(n = 6)

# LA's coast matters more than its rivers, so the loupe shows the land inside the ring
la_window <- st_sfc(st_polygon(list(as.matrix(la_ring))), crs = 4326)
to_loupe_sfc <- function(g) {
  g <- (g - c(la_lon, la_lat)) * diag(c(mag * cos(la_lat * pi / 180) / lon_scale, mag)) + c(loupe_x, loupe_y)
  st_set_crs(g, 4326)
}
la_land  <- to_loupe_sfc(st_intersection(st_geometry(land), la_window))
la_coast <- to_loupe_sfc(st_intersection(st_geometry(coastline), la_window))

# Thin leader from the LA ring to the loupe
loupe_link <- tibble(
  x = la_lon - la_rx, y = la_lat,
  xend = loupe_x + loupe_r / lon_scale, yend = loupe_y
)

famous <- famous |> mutate(la = in_la(lon, lat))

# Commissioning years for the raincloud chart. The database records fractional years
# for plants whose units came online at different times; they are kept as they are.
year_dat <- plants_ca |>
  filter(!is.na(year)) |>
  mutate(fuel = stats::reorder(fuel, year, FUN = median))

set.seed(35)
rain_dat <- year_dat |>
  slice_sample(n = 300, by = fuel) |>
  mutate(x_jitter = as.numeric(fuel) - 0.18 + stats::runif(n(), -0.09, 0.09))

medians <- year_dat |> summarise(med = round(median(year)), .by = fuel)
med_of  <- function(f) medians$med[medians$fuel == f]
n_of    <- function(f) sum(plants_ca$fuel == f)

# Map annotations: neighbours in capitals, seas in italics
state_names <- tribble(
  ~label,     ~lon,    ~lat,
  "OREGON",   -121.0,  42.35,
  "NEVADA",   -116.4,  38.6,
  "ARIZONA",  -113.9,  34.6,
  "MEXICO",   -115.0,  31.95
)
sea_names <- tribble(
  ~label,             ~lon,    ~lat,
  "Pacific Ocean",    loupe_x, loupe_y - loupe_r - 0.9
)

# Plot ----

# Fonts
main_font <- 'Libre Franklin'
map_font  <- 'Fira Sans'
sysfonts::font_add_google(main_font)
sysfonts::font_add_google(map_font)
showtext_auto()
showtext_opts(dpi = 300)

# Atlas palette:
page      <- '#FBF8F1'
ink       <- '#1A1A1A'
muted     <- '#6B6B6B'
sea       <- '#D3E4EE'
water     <- '#5F93B3'
neighbour <- '#EEE6D6'
home      <- '#FFFEFA'
border    <- '#C4B7A0'
red       <- '#E3120B'

fuel_palette <- c(
  solar      = red,
  wind       = '#0F3B63',
  gas        = '#5E5548',
  geothermal = '#C46A1F',
  hydro      = '#7E9CBB',
  other      = '#CFC6B4'
)

title <- "Solar is California's youngest power source"
subtitle <- str_glue(
  "California has {format(nrow(plants_ca), big.mark = ',')} plants in WRI's Global Power Plant Database. ",
  "Its hydro dams are the oldest (median build year {med_of('hydro')}), followed by geothermal ",
  "({med_of('geothermal')}), gas ({med_of('gas')}) and wind ({med_of('wind')}). Solar is the newest: ",
  "half of its {n_of('solar')} plants came online after **{med_of('solar')}**. ",
  "Diamonds mark the 12 largest plants"
)

caption <- paste(
  'Source: Global Power Plant Database v1.3, World Resources Institute (2021)',
  'Layout after Guillaume Noblet\'s #TidyTuesday 2026 week 35 castles map',
  sep = '<br>'
)

p_map <- ggplot() +
  # Sea
  geom_sf(data = water_lines, aes(alpha = km), colour = water, linewidth = 0.3) +
  scale_alpha(range = c(0.45, 0.08), guide = 'none') +
  # Land
  geom_sf(data = states_crop, fill = neighbour, colour = border, linewidth = 0.3) +
  geom_sf(data = ca_poly, fill = home, colour = '#8C8170', linewidth = 0.5) +
  geom_sf(data = lakes, fill = sea, colour = water, linewidth = 0.25) +
  # Rivers
  geom_sf(data = rivers |> filter(size == 'major'), colour = water, alpha = 0.35, linewidth = 0.35) +
  geom_sf(data = rivers_ca |> filter(size == 'minor'), colour = water, alpha = 0.8, linewidth = 0.3) +
  geom_sf(data = rivers_ca |> filter(size == 'major'), colour = water, linewidth = 0.55) +
  # Coastline
  geom_sf(data = coastline, colour = water, linewidth = 0.45) +
  # State labels
  geom_text(
    data = state_names, aes(lon, lat, label = label),
    family = map_font, colour = '#948A78', size = 4
  ) +
  # Sea labels
  geom_text(
    data = sea_names, aes(lon, lat, label = label),
    family = map_font, fontface = 'italic', colour = '#4F7F9E', size = 4, lineheight = 0.9
  ) +
  # Plants
  geom_point(
    data = plants_ca |> mutate(fuel = factor(fuel, fuel_order)),
    aes(lon, lat, fill = fuel),
    shape = 21, size = 2.2, colour = 'white', stroke = 0.3
  ) +
  # Largest plants
  geom_point(
    data = famous, aes(lon, lat),
    shape = 23, size = 3.6, fill = ink, colour = 'white', stroke = 0.5
  ) +
  # Keep the state-scale labels off the ring
  geom_text_repel(
    data = bind_rows(
      famous |> filter(!la) |> select(lon, lat, label),
      la_ring |> slice(seq(1, n(), by = 12)) |> mutate(lon = x, lat = y, label = '', .keep = 'none')
    ),
    aes(lon, lat, label = label),
    family = map_font, size = 4, colour = ink,
    box.padding = 0.5, point.padding = 0.3, segment.colour = ink, segment.size = 0.3,
    min.segment.length = 0, max.overlaps = Inf, seed = 35,
    bg.color = 'white', bg.r = 0.15
  ) +
  # LA ring, leader and loupe
  geom_path(data = la_ring, aes(x, y), colour = ink, linewidth = 0.5) +
  geom_segment(
    data = loupe_link, aes(x = x, y = y, xend = xend, yend = yend),
    colour = ink, linewidth = 0.35, linetype = '22'
  ) +
  geom_polygon(data = loupe_disc, aes(x, y), fill = sea, colour = NA) +
  geom_sf(data = la_land, fill = home, colour = NA) +
  geom_sf(data = la_coast, colour = water, linewidth = 0.8) +
  geom_polygon(data = loupe_disc, aes(x, y), fill = NA, colour = ink, linewidth = 0.7) +
  geom_point(
    data = plants_la |> mutate(fuel = factor(fuel, fuel_order)),
    aes(x, y, fill = fuel),
    shape = 21, size = 3.2, colour = 'white', stroke = 0.3
  ) +
  geom_point(
    data = la_labels, aes(x, y),
    shape = 23, size = 3.6, fill = ink, colour = 'white', stroke = 0.5
  ) +
  geom_text_repel(
    data = la_labels, aes(x, y, label = label),
    family = map_font, size = 3.6, colour = ink,
    box.padding = 0.55, point.padding = 0.3, force = 3, segment.colour = ink, segment.size = 0.25,
    min.segment.length = 0, max.overlaps = Inf, seed = 35,
    bg.color = 'white', bg.r = 0.15,
    xlim = loupe_x + c(-1, 1) * 0.92 * loupe_r / lon_scale,
    ylim = loupe_y + c(-1, 1) * 0.92 * loupe_r
  ) +
  annotate(
    'text', x = loupe_x, y = loupe_y - loupe_r - 0.35, label = 'LOS ANGELES',
    family = map_font, fontface = 'bold', size = 4.2, colour = ink
  ) +
  # Legend
  scale_fill_manual(
    values = fuel_palette, breaks = fuel_order,
    labels = str_to_title(fuel_order), name = NULL
  ) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = 5))) +
  # Labels
  labs(title = title, subtitle = subtitle, caption = caption) +
  coord_sf(xlim = c(xmin_ca, xmax_ca), ylim = c(ymin_ca, ymax_ca), expand = FALSE) +
  # Theme
  theme_void(base_family = main_font) +
  theme(
    plot.background       = element_rect(fill = page, colour = NA),
    panel.background      = element_rect(fill = sea, colour = NA),
    plot.margin           = margin(40, 35, 15, 35),
    plot.title.position   = 'plot',
    plot.caption.position = 'plot',
    plot.title = element_textbox_simple(
      family = main_font, face = 'bold', size = 32, colour = ink,
      lineheight = 1.1, margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      family = main_font, size = 16, colour = '#333333', lineheight = 1.35, margin = margin(b = 12)
    ),
    plot.caption = element_textbox_simple(
      family = main_font, size = 13, colour = muted, halign = 0, lineheight = 1.4, margin = margin(t = 10)
    ),
    legend.position      = 'top',
    legend.justification = 'left',
    legend.text          = element_text(family = main_font, size = 14, colour = ink, margin = margin(r = 14)),
    legend.key           = element_rect(fill = NA, colour = NA),
    legend.margin        = margin(b = 6)
  )

# Raincloud chart: half-violins with a narrow boxplot, and the raw years as rain underneath.
# "Other" is too pale to read as text, so its label uses the muted grey
label_palette <- replace(fuel_palette, "other", muted)
fuel_label <- function(x) sprintf("<span style='color:%s'>**%s**</span>", label_palette[x], str_to_title(x))

p_inset <- ggplot(year_dat, aes(x = fuel, y = year)) +
  geom_violin(
    aes(fill = fuel), trim = TRUE, alpha = 0.6, colour = NA,
    position = position_nudge(x = 0.15), width = 0.9
  ) +
  geom_boxplot(
    width = 0.1, outlier.shape = NA, linewidth = 0.4, fill = 'white', colour = ink,
    position = position_nudge(x = 0.15)
  ) +
  geom_point(
    data = rain_dat, aes(x = x_jitter, y = year, colour = fuel),
    inherit.aes = FALSE, size = 0.9, alpha = 0.55
  ) +
  scale_fill_manual(values = fuel_palette, guide = 'none') +
  scale_colour_manual(values = fuel_palette, guide = 'none') +
  scale_x_discrete(labels = fuel_label) +
  coord_flip() +
  labs(
    title = 'When did they come online?',
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13, base_family = main_font) +
  theme(
    plot.background    = element_rect(fill = 'white', colour = '#D8D4CC', linewidth = 0.4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = '#E6E2DA', linewidth = 0.3),
    # coord_flip draws the category axis with axis.text.y.left, which is
    # plain text in the base theme, so the markdown element goes there
    axis.text.y.left   = element_markdown(size = 13, hjust = 0),
    axis.text.x        = element_text(colour = muted, size = 11),
    plot.title         = element_text(face = 'bold', size = 15, colour = ink),
    plot.title.position = 'plot',
    plot.margin        = margin(10, 14, 8, 10)
  )

# Red tab at the top-left
red_tab <- ggplot() +
  annotate('rect', xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = red) +
  theme_void()

p <- p_map +
  inset_element(p_inset, left = 0.015, right = 0.40, bottom = 0.015, top = 0.40, align_to = 'panel') +
  inset_element(red_tab, left = 0, right = 0.09, bottom = 0.993, top = 1, align_to = 'full') +
  plot_annotation(theme = theme(plot.background = element_rect(fill = page, colour = NA)))

# Save ----
out_png   <- file.path(out_dir, 'power_plants_california.png')
out_thumb <- file.path(out_dir, 'power_plants_california_thumb.png')

ggsave(
  out_png,
  plot   = p,
  width  = 13,
  height = 14.2,
  dpi    = 300,
  bg     = page
)

image <- image_read(out_png)
image <- image_scale(image, geometry_size_pixels(width = 500, preserve_aspect = TRUE))
image_write(image, out_thumb, format = 'png')
