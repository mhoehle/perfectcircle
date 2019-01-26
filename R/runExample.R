#' Shiny app demoing the pkg.
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "perfectcircle", package = "perfectcircle")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `perfectcircle`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
