#' @title run Shiny Example
#'
#' @description Launches Shiny app for MassNBCtools package.
#'
#' @details The Shiny app based on the R package MassNBCtools is included in
#' the R package. This function launches that app.
#'
#' The Shiny app is online at:
#' https://tetratech-wtr-wne.shinyapps.io/MassNBCtools
#'
#' @param shinyappname Shiny appplication name, default = MassNBCtools
#'
#' @examples
#' \dontrun{
#' # Run Function (full EMVL version, default)
#' runShiny()
#'
#' }
#
#' @export
runShiny <- function(shinyappname = "MassNBCtools"){##FUNCTION.START
  #
  appDir <- system.file("shiny-examples"
                        , shinyappname
                        , package = "MassNBCtools")
  #
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `MassNBCtools`."
         , call. = FALSE)
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}##FUNCTION.END