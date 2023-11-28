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
#'   toolchainR()}
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
            "You should be able to install packages that compile C, C++, and FORTRAN code.\n"
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
            "You should be able to install packages that compile C, C++, and FORTRAN code.\n"
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
          "After install, restart R/RStudio and run `toolchainR::toolchainR()` again to verify proper set up\n"
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
      "After install, restart R/RStudio and run `toolchainR::toolchainR()` again to verify proper set up\n"
    )
    
    # Return success
    return(0)
    
  }
  
}
