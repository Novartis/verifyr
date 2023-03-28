#' Initial comparison of content of two files
#'
#' \code{initial_comparison} returns basic info about the differences of two files.
#'
#'
#' @param old character, giving the full file path and name of the old file (required)
#' @param new character, giving the full file path and name of the new file (required)
#'
#' @return Returns a vector \code{diff_print} with basic info about the differences in the content of the files
#'
#' @examples
#' verifyr::initial_comparison(old = "./test_outputs/base_files/14-1.01.rtf",
#'                             new = "./test_outputs/compare_files/14-1.01.rtf")
#'
#' @export

initial_comparison <- function(old, new){


  ## do the comparison only if both of the files exist
  if (file.exists({{ old }}) && file.exists({{ new }})) {

    difference <- all.equal(readLines({{ old }}, warn = FALSE), readLines({{ new }}, warn = FALSE))

    ## all.equal returns length 2 vector if the number of rows are not the same in the compared files
    if (length(difference)==2) {
      diff_print <- paste0("Output size has changes")

      ## all.equal returns length 1 vector if the number of rows are the same but there are differences in the content
    } else if (length(difference)==1) {

      ## all.equal returns logical vector if there are no differences
      if (typeof(difference)=="logical"){
        diff_print <- paste0("No changes")

      } else if (typeof(difference)!="logical") {

        lines_diff <- as.numeric(gsub("[^[:digit:].]", "",  difference))


        if(lines_diff == 1) diff_print <- paste0("Only one row has changed")
        if(lines_diff >  1) diff_print <- paste0("Output has changes in ", lines_diff, " places")

      } else {
        print("debug1: something went wrong")
      }
    } else {
      print("debug2: something went wrong")
    }

    ## return an error if both of the files do not exist
  } else if (!file.exists({{ old }}) && !file.exists({{ new }})) {
    stop("Neither of the files exist")

  } else if (!file.exists({{ old }}) || !file.exists({{ new }})) {
    diff_print <- "Unable to compare"

  }
  return(diff_print)
}



