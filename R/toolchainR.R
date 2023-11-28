#' @title Install R Toolchains
#'
#' @description A single function to install R toolchains based on your
#' R version and operating system. Follow the instructions
#' 
#' @return Returns messages and a numeric value:
#'
#' \item{0}{Successful toolchain installation}
#' 
#' \item{1}{Failed toolchain installation}
#' 
#' @author Alexander P. Christensen <alexpaulchristensen@gmail.com>
#'
#' @examples
#' if(interactive()){
#'   toolchainR()
#' }
#' 
#' @export
#'
# Updated 28.11.2023
# Main function ----
toolchainR <- function()
{
  # Get system information
  system_information <- system_check()
  
  # Send message about version < 4.0
  if(as.numeric(substr(system_information$R, 1, 1)) < 4){
    
    # Message user
    message(
      paste0(
        "Your R version (", system_information$R, ") ",
        "is out-of-date. Version >=4.0 is required."
      )
    )
    
    # Check for update
    if(get_response("Would you like to update R? (Y/n): ") == "y"){
      
      # Send message
      message("Running: installr::updateR()")
      
      # Update R
      installr::updateR()
      
      # Send message
      message("Restart R/RStudio and run this function again")
      
      # Return success flag
      return(0)
      
    }else{
      
      # Send message
      message(
        paste0(
          colortext(textsymbol("x"), defaults = "red"),
          " Update to R terminated. It's ",
          styletext("strongly recommended ", defaults = "bold"),
          "that you update R >= 4.0\n\n",
          "User termination"
        )
      )
      
      # Return failure flag
      return(1)
      
    }
    
  }
  
  # Set divergent paths
  if(system_information$OS == "windows"){ # Windows
    
    # Check for Rtools already installed
    if(pkgbuild::has_rtools()){
     
      if(pkgbuild::check_rtools()){
        
        # Send completion message
        message(
          paste0(
            colortext(textsymbol("check mark"), defaults = "green"),
            " Toolchain is already installed.\n\n",
            "You should be able to install packages that compile C, C++, and FORTRAN code."
          )
        )
        
        # Return success flag
        return(0)
        
      }else{
        
        # Send message
        message("Rtools appears to be installed but not properly set up. Setting up Rtools...")
        
        # Set up Rtools
        pkgbuild::setup_rtools()
        
        # Send completion message
        message(
          paste0(
            colortext(textsymbol("check mark"), defaults = "green"),
            " Toolchain is successfully set up.\n\n",
            "You should be able to install packages that compile C, C++, and FORTRAN code."
          )
        )
        
        # Return success flag
        return(0)
      
      }
      
       
    }else{ # Install Rtools
      
      # Get Rtools URL
      Rtools_URL <- switch(
        substr(system_information$R, 1, 3),
        "4.3" = "https://cran.r-project.org/bin/windows/Rtools/rtools43/files/rtools43-5863-5818.exe",
        "4.2" = "https://cran.r-project.org/bin/windows/Rtools/rtools42/files/rtools42-5355-5357.exe",
        "4.1" = "https://github.com/r-windows/rtools-installer/releases/download/2022-02-06/rtools40-x86_64.exe",
        "4.0" = "https://github.com/r-windows/rtools-installer/releases/download/2022-02-06/rtools40-x86_64.exe",
        stop(paste0(
          paste0(
            "\n",
            colortext(textsymbol("x"), defaults = "red"),
            " Install of Rtools terminated. You ",
            styletext("must ", defaults = "bold"),
            " update R >= 4.0\n\n",
            "User termination"
          )
        ), call. = FALSE)
      )
      
      # Message user
      message(
        paste0(
          "Rtools is not installed. Please install Rtools using the following link:\n\n",
          Rtools_URL, "\n\n",
          "After install, restart R/RStudio and run `toolchainR::toolchainR()` again to verify proper set up"
        )
      )
      
      # Return success flag
      return(0)
      
    }
    
    
  }else if(system_information$OS == "linux"){ # Linux
    message("Linux OS is not yet supported.")
    return("toolchainR installer terminated.")
  }else{ # Mac
    
    
    
    # Check for {macrtools}
    if("macrtools" %in% ulapply(.libPaths(), list.files)){
      
      # Send message
      message("Running: remotes::install_github(\"coatless-mac/macrtools\")")
     
      # Actually run
      remotes::install_github("coatless-mac/macrtools")
      
    }
    
    # Send message
    message("Running: macrtools::macos_rtools_install()")
    
    # Actually run
    macrtools::macos_rtools_install()
  
    # Message user
    message(
      "After install, restart R/RStudio and run `toolchainR::toolchainR()` again to verify proper set up"
    )
    
    # Return success
    return(0)
    
  }
  
}

# Helpers ----

#' @noRd
# Unlist `lapply` ----
# Updated 14.07.2023
ulapply <- function(X, FUN, ..., recursive = TRUE)
{
  return(unlist(lapply(X, FUN, ...), recursive = recursive))
}

#' @noRd
# Faster `ifelse` ----
# For single value replacements:
# 1.5x faster with 1 value
# 2.5x faster with 10 values
# >= 18x faster with >= 100 values
# Updated 24.07.2023
swiftelse <- function(condition, true, false)
{
  
  # Get condition length
  condition_length <- length(condition)
  
  # Check for single value
  if(condition_length == 1){
    
    # If TRUE
    if(condition){
      return(true)
    }else{ # Otherwise, FALSE
      return(false)
    }
    
  }
  
  # Initialize result
  result <- vector(mode(true), condition_length)
  
  # Set TRUE condition
  if(length(true) == 1){
    result[condition] <- true
  }else{
    result[condition] <- true[condition]
  }
  
  # Set FALSE condition
  if(length(false) == 1){
    result[!condition] <- false
  }else{
    
    # Get opposite condition (slightly faster than repeated calls)
    opposite_condition <- !condition
    result[opposite_condition] <- false[opposite_condition]
  }
  
  
  # Return result
  return(result)
  
}

#' @noRd
# Get response ---
# Updated 28.11.2023
get_response <- function(prompt = "")
{
 
  # Return response
  return(
    switch(
      tolower(readline(prompt)),
      "y" = "y", "yes" = "y",
      "n" = "n", "no" = "n",
      stop("Unexpected response. User terminated.", call. = FALSE)
    )
  )
  
}

#' @noRd
# OS and System Check ----
# Updated 28.11.2023
system_check <- function()
{
  
  # Get OS usage
  OS <- unname(tolower(Sys.info()["sysname"]))
  
  # Get RStudio usage
  RSTUDIO <- swiftelse(Sys.getenv("RSTUDIO") == "1", TRUE, FALSE)
  
  # Return list
  return(
    list(
      OS = OS,
      R = paste0(R.version$major, ".", R.version$minor),
      RSTUDIO = RSTUDIO,
      TEXT = swiftelse(!RSTUDIO & OS != "linux", FALSE, TRUE)
    )
  )
  
}

#' @noRd
# Colorize text ----
# Updated 28.11.2023
colortext <- function(text, number = NULL, defaults = NULL)
{
  
  # Check for text
  if(system_check()$TEXT){
    
    # Defaults for number (white text)
    if(is.null(number) || number < 0 || number > 231){number <- 15}
    
    # Check for default color
    number <- swiftelse(
      !is.null(defaults) & defaults == "highlight", 208,
      switch(
        defaults, message = 204, red = 9,
        orange = 208, yellow = 11, "light green" = 10,
        green = 34, cyan = 14, blue = 12,
        magenta = 13, pink = 211
      )
    )
    
    # Return text
    return(paste0("\033[38;5;", number, "m", text, "\033[0m"))
    
  }else{return(text)}
  
}

#' @noRd
# Style text ----
# Updated 28.11.2023
styletext <- function(
    text, defaults = c(
      "bold", "italics", "highlight",
      "underline", "strikethrough"
    )
)
{
  
  # Check for text
  if(system_check()$TEXT){
    
    # Set style
    number <- swiftelse(
      missing(defaults), 0,
      switch(
        defaults, bold = 1, italics = 3,
        underline = 4, highlight = 7,
        strikethrough = 9
      )
    )
    
    # Return text
    return(paste("\033[", number, ";m", text, "\033[0m", sep = ""))
    
  }else{return(text)}
  
}

#' @noRd
# Symbols ----
# Updated 26.07.2024
textsymbol <- function(
    symbol = c(
      "alpha", "beta", "chi", "delta",
      "eta", "gamma", "lambda", "omega",
      "phi", "pi", "rho", "sigma", "tau",
      "theta", "square root", "infinity",
      "check mark", "x", "bullet"
    )
)
{
  # Return code
  return(
    switch(
      symbol,
      alpha = "\u03B1", beta = "\u03B2", chi = "\u03C7",
      delta = "\u03B4", eta = "\u03B7", gamma = "\u03B3",
      lambda = "\u03BB,", omega = "\u03C9", phi = "\u03C6",
      pi = "\u03C0", rho = "\u03C1", sigma = "\u03C3", tau = "\u03C4", 
      theta = "\u03B8", "square root" = "\u221A", infinity = "\u221E", 
      "check mark" = "\u2713", x = "\u2717", bullet = "\u2022"
    )
  )
  
}

#' @noRd
# General function to check for packages ----
# Updated 13.07.2023
check_package <- function(packages)
{
  
  # Performs what original `installed.packages()` does
  # but without additional fluff
  installed <- packages %in% ulapply(.libPaths(), list.files)
  
  # Determine which packages are not installed
  not_installed <- packages[!installed]
  
  # Print error with missing packages
  if(length(not_installed) != 0){
    
    # Organize packages error output
    if(length(not_installed) > 1){
      
      # Get missing packages
      missing_packages <- paste0("{", packages , "}", collapse = ", ")
      packages <- paste0("\"", packages, "\"", collapse = ", ")
      
      # Stop and tell user to install package
      stop(
        paste0(
          missing_packages, 
          " are not installed but are required for this function. ",
          "Please run \n\n",
          "`install.packages(c(", packages, "))`",
          "\n\nOnce installed, re-run this function (you may need to restart R/RStudio)."
        ), call. = FALSE
      )
      
    }else{
      
      # Get missing packages
      missing_packages <- paste0("{", packages, "}")
      packages <- paste0("\"", packages, "\"")
      
      # Stop and tell user to install package
      stop(
        paste0(
          missing_packages, 
          " is not installed but is required for this function. ",
          "Please run \n\n",
          "`install.packages(", packages, ")`",
          "\n\nOnce installed, re-run this function (you may need to restart R/RStudio)."
        ), call. = FALSE
      )
      
    }
    
  }
  
}
