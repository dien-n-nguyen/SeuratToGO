#' Run the package's Shiny App
#'
#' This function runs the package's Shiny App
#' @importFrom shiny shinyAppDir
#' @importFrom shiny runApp
#'
#' @export

run_SeuratToGO <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "SeuratToGO")
  shinyObj <- shiny::shinyAppDir(appDir)
  shiny::runApp(shinyObj, display.mode = "normal")
  return()
}
