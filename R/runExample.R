#' Call for shiny example where the user can test verifyr package functions
#'
#' \code{runExample} returns simple Shiny App where user can see how the verifyr functions work
#'
#'
#'  @examples
#'verifyr::runExample()
#'
#' @export
runExample <- function() {
  appDir <- system.file("shiny_examples", "app", package = "verifyr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `verifyr`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
