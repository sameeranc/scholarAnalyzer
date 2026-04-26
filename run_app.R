# ============================================================
# scholarAnalyzer — Launch Function
# ============================================================

#' Launch the Google Scholar Citation Analyzer
#'
#' Starts the Shiny application in your default web browser.
#'
#' @param ... Additional arguments passed to \code{shiny::shinyApp()},
#'   such as \code{host} or \code{port}.
#'
#' @return Invisibly returns the Shiny app object.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function(...) {
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
