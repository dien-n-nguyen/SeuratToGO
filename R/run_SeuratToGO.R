#' Run the package's Shiny App
#'
#' This function runs the package's Shiny App
#'
#' @export

run_SeuratToGO <- function() {
  appDir <- system.file("R",
                        package = "SeuratToGO")
  shinyObj <- shiny::shinyAppDir(appDir)
  shiny::runApp(shinyObj, display.mode = "normal")
  return()
}
