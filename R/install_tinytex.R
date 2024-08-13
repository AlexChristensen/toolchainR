#' @title Install \code{\link{tinytex}}
#'
#' @description A single function to install \code{\link{tinytex}}
#'
#' @return Returns messages and a numeric value:
#'
#' \item{0}{Successful \code{\link{tinytex}} installation}
#'
#' \item{1}{Failed \code{\link{tinytex}} installation}
#'
#' @author Alexander P. Christensen <alexpaulchristensen@gmail.com>
#'
#' @examples
#' \donttest{
#' install_tinytex()}
#'
#' @export
#'
# Updated 13.08.2024
# Installs {tinytex} ----
install_tinytex <- function()
{

  # First, check if {tinytex} is installed
  if("tinytex" %in% ulapply(.libPaths(), list.files)){

    # Send completion message
    message(
      paste0(
        "\n", colortext(textsymbol("check mark"), defaults = "green"),
        " {tinytex} is already installed.\n"
      )
    )

  }else{ # Not installed, so install

    # Install {tinytex}
    install.packages("tinytex")

    # Send completion message
    message(
      paste0(
        "\n", colortext(textsymbol("check mark"), defaults = "green"),
        " {tinytex} has successfully installed.\n"
      )
    )

  }

  # Second, check for latex
  if(pkgbuild::has_latex() && !tinytex::is_tinytex()){

    # Send completion message
    message(
      paste0(
        colortext(textsymbol("check mark"), defaults = "green"),
        " LaTeX is installed.\n\n",
        "If you are unable to knit an Rmarkdown, then you'll need to run:\n\n",
        "tinytex::install_tinytex()"
      )
    )

    # Return success
    return(0)

  }else if(tinytex::is_tinytex()){ # Third, check if {tinytex} is usable

    # Send completion message
    message(
      paste0(
        colortext(textsymbol("check mark"), defaults = "green"),
        " {tinytex} is installed and available."
      )
    )

    # Return success
    return(0)


  }else{ # Fourth, {tinytex} is not being used

    # Message user
    message(
      paste0(
        "Installing {tinytex}...\n",
        "This could take a while... (~20-30 minutes)"
      )
    )

    # Install {tinytex}
    tinytex::install_tinytex()

    # Send completion message
    message(
      paste0(
        colortext(textsymbol("check mark"), defaults = "green"),
        " {tinytex} is installed and available."
      )
    )

    # Return success
    return(0)

  }

}
