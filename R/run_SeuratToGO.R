#' Run the package's Shiny App
#'
#' This function runs the package's Shiny App
#'
#' @export

run_SeuratToGO <- function() {
  appDir <- system.file("R",
                        package = "SeuratToGO")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
