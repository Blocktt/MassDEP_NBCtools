#' @title run Shiny Example
#'
#' @description Launches Shiny app for MassDEP_NBCtools package.
#'
#' @details The Shiny app based on the R package MassDEP_NBCtools is included in
#' the R package. This function launches that app.
#'
#' The Shiny app is online at:
#' https://tetratech-wtr-wne.shinyapps.io/MassDEP_NBCtools
#'
#' @param shinyappname Shiny appplication name, default = MassDEP_NBCtools
#'
#' @examples
#' \dontrun{
#' # Run Function (full EMVL version, default)
#' runShiny()
#'
#' }
#
#' @export
runShiny <- function(shinyappname = "MassDEP_NBCtools"){##FUNCTION.START
  #
  appDir <- system.file("shiny-examples"
                        , shinyappname
                        , package = "MassDEP_NBCtools")
  #
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `MassDEP_NBCtools`."
         , call. = FALSE)
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}##FUNCTION.END