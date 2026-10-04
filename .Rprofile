source("renv/activate.R")

# install pak ----
if (!"pak" %in% installed.packages()) {
  install.packages("pak")
  message("`pak` is now attached!")
} else {
  message("`pak` was already attached.")
}

# install renv ----
if (!"renv" %in% installed.packages()) {
  pak::pak("renv", ask = FALSE)
  message("`renv` is now attached!")
} else {
  message("`renv` was already attached.")
}

# install cli ----
if (!"cli" %in% installed.packages()) {
  pak::pak("cli", ask = FALSE)
  message("`cli` is now attached!")
} else {
  message("`cli` was already attached.")
}

# messaging about package installation ----
cli::cli_h1(
  "Package Installation"
)

cli::cli_alert_info(
  "Ensure that all required packages are attached."
)


# dynamically attach packages as needed ----
source("./R/packages.R")

# messaging about package installation ----
cli::cli_h1(
  "Run the california_power_plants.R file"
)

source("./R/california_power_plants.R")
