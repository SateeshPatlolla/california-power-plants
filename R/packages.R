# Utilize the air package for code formatting by default in Positron
# renv is loaded by Positron if you elect to do so (that was done here)
# renv is initialized by Positron if you elect to do so (that was done here)

## packages ----

# define target packages
required_packages <- c(
'dplyr',
'tidyr',
'readr',
'stringr',
'ggplot2',
'ggtext',
'ggrepel',
'showtext',
'patchwork',
'sf',
'rnaturalearth',
'magick')

# these packages are utilized in this project and must be installed ----
if (any(!required_packages %in% installed.packages())) {
  # missing packages
  missing_packages <- required_packages[
    !required_packages %in% installed.packages()
  ]

  # only install those needed
  # `ask = FALSE`: pak cannot prompt for confirmation while .Rprofile is sourced
  pak::pak(missing_packages, ask = FALSE)

  # print a message to the console reporting that work is done
  print_message <- paste(
    "The following missing packages were attached via `pak::pak()`: ",
    paste(missing_packages, collapse = ", ")
  )
} else {
  # otherwise print a message to the console stating all is well
  message("All required packages are attached.")
}