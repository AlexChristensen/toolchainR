#' @title Install DS3100 Packages
#'
#' @description A single function to install all R packages for
#' the DS3100 course
#'
#' @return Returns messages and a numeric value:
#'
#' \item{0}{Successful package installations}
#'
#' \item{1}{Failed package installations}
#'
#' @author Alexander P. Christensen <alexpaulchristensen@gmail.com>
#'
#' @examples
#' \donttest{
#' install_ds3100()}
#'
#' @export
#'
# Updated 13.08.2024
# Installs DS3100 Packages ----
install_ds3100 <- function()
{

  # First, check for toolchain
  toolchain_check <- toolchainR()

  # Second, set list of packages needed to be installed
  ds3100_packages <- c(
    "aricode", "bestNormalize", "car", "caret", "cluster",
    "devtools", "ds4psy", "EGAnet", "factoextra", "faraway",
    "GGally", "ggplot2", "ggpubr", "glmnet", "NbClust",
    "psych", "psychTools", "readxl", "regclass", "relaimpo",
    "reticulate", "rms", "stringr", "tidyverse"
  )

  # Get packages
  needed_packages <- ds3100_packages[
    !ds3100_packages %in% ulapply(.libPaths(), list.files)
  ]

  # Get number needed
  needed_number <- length(needed_packages)

  # Check for whether any are needed
  if(needed_number > 0){

    # Message user
    message(
      paste0(
        "Installing ", needed_number,
        " package(s) and their dependencies for DS3100. ",
        "This could take a while... (~20-30 minutes)"
      )
    )

    # Install packages
    ## Use "--no-lock" to avoid some issues
    install.packages(
      needed_packages,
      dependencies = c("Depends", "Imports")
    )

  }

  # # Third, get GitHub packages
  # remotes::install_github(
  #   "AlexChristensen/simpleRgenius", quiet = TRUE
  # )
  #
  # # Check again for packages in installed packages
  # ds3100_packages <- c(ds3100_packages, "simpleRgenius")

  # Get packages
  needed_packages <- ds3100_packages[
    !ds3100_packages %in% ulapply(.libPaths(), list.files)
  ]

  # Get number needed
  needed_number <- length(needed_packages)

  # Check for remaining packages
  if(needed_number > 0){

    # Send completion message
    message(
      paste0(
        colortext(textsymbol("x"), defaults = "red"),
        " Some package(s) were not able to be installed:\n",
        paste0(needed_packages, collapse = ", ")
      )
    )

    # Return failure
    return(1)

  }else{

    # Send completion message
    message(
      paste0(
        colortext(textsymbol("check mark"), defaults = "green"),
        " All DS3100 packages are successfully installed.\n"
      )
    )

    # Print another message
    message("Remember to update your packages often... Speaking of, update your packages now :)")

    # Return success
    return(0)

  }


}
