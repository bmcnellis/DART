#' @title WebDART interface
#'
#' @description NA
#'
#' @details NA
#'
#' @author Steve Fick
#' @author Anna Knight
#' @author Samuel Chambers
#' @author Brandon McNellis
#'
#' @name webDART
#' @rdname webDART
NULL
#' @rdname webDART
#' @export
RunWebDART <- function(directory = getwd(), debugMessage = FALSE) {
  require(shiny)
  require(leaflet)
  require(visNetwork)

  # Set up app to run
  appDir <- system.file("shiny-apps", "webDART", package = "DART")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `DART`.", call. = FALSE)
  }

  # Pass parameters
  .GlobalEnv$.debugMessage <- debugMessage
  .GlobalEnv$.directory <- directory

  # Clean up on exit
  on.exit(rm(list = c('.debugMessage', '.directory'), envir = .GlobalEnv))

  # Run app
  shiny::runApp(appDir, display.mode = "normal")
}
