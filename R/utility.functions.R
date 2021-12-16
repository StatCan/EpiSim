#' Return the complete path to a demo model Excel workbook
#'
#' @export
#'
#' @param x a character element representing a file name ("demo.model.1.xls", "demo.model.3.xls", "demo.model.4a.xls", "demo.model.4b.xls").
#'
#' @examples
#' path.to.demo.model <- get.path("demo.model.1.xls")
#' 
#' @return a character element representing the complete path to the file. If the file cannot be found, an empty element will be returned.

get.path <- function(x) {
  system.file("extdata", x, package = "EpiSim")
}