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