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
# Updated 28.11.2023
# Installs DS3100 Packages ----
install_ds3100 <- function()
{
  
  # First, check for toolchain
  toolchain_check <- toolchainR()
  
  # Second, set list of packages needed to be installed
  ds3100_packages <- c(
    "car", "caret", "EGAnet", "GGally", "ggplot2", 
    "ggpubr", "psych", "readxl", "regclass", "rms", 
    "tidyverse"
  )
  
}
