#' Call for shiny example where the user can test verifyr package functions
#'
#' \code{run_example} returns simple Shiny App where user can see how the verifyr functions work
#'
#'
#'  @examples
#'verifyr::run_example()
#'
#' @export
run_example <- function() {
  app_dir <- system.file("shiny_examples", "app", package = "verifyr")
  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing `verifyr`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
