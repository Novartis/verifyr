#' Compare the content of two rtf files
#'
#' \code{full_comparison} returns side by side comparison of two rtf files
#'
#'
#' @param old  character, giving the full file path and name of the old file (required)
#' @param new  character, giving the full file path and name of the new file (required)
#'
#' @return Returns \code{diff_print}, html output of the differences
#'
#' @examples
#' verifyr::full_comparison(old = "./test_outputs/base_files/14-1.01.rtf",
#'                          new = "./test_outputs/compare_files/14-1.01.rtf")
#'
#' @export


full_comparison <- function(old, new){


  ## do the comparison only if both of the files exist
  if (file.exists({{ old }}) && file.exists({{ new }})) {

    ## read in the rtf files
    rtf_old    <-  striprtf::read_rtf(file = file.path({{ old }}))
    rtf_new    <-  striprtf::read_rtf(file = file.path({{ new }}))


  diff_print <- diffobj::diffPrint(rtf_old, rtf_new, color.mode="rgb", format="html", style=list(html.output="diff.w.style"))

  return(diff_print)

  } else print("one or both of the files do not exist")

}
