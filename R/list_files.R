#' List files that exist in two folders
#'
#' \code{list_files} List files that exist in two folders with a specific name pattern
#'
#'
#' @param old character, giving the the full file path and name of the folder where old files are stored (required)
#' @param new character, giving the the full file path and name of the folder where new files are stored (required)
#' @param pattern character, limit the files to be listed to contain a specific pattern (optional)
#'
#' @return Returns a tibble, \code{selected_files} with 3 columns \code{file}, \code{old_path}, \code{new_path}
#' @examples
#' verifyr::list_files(old = paste0(fs::path_package("/extdata/base_files/", package = "verifyr")),
#'                     new = paste0(fs::path_package("/extdata/compare_files/", package = "verifyr")),
#'                     pattern = "14-1")
#'
#' @export

list_files <- function(old, new, pattern = NULL) {

  ## do the comparison only if both of the files exist
  if (file.exists({{ old }}) && file.exists({{ new }})) {

    ## get the info of the old files
    old_files  <- list.files(path = {{ old }}, pattern = {{ pattern }})
    old_paths  <- list.files(path = {{ old }}, pattern = {{ pattern }}, full.names = TRUE)
    old_info   <- tibble::tibble(file = old_files, old_path = old_paths)

    ## get the info of the new files
    new_files  <- list.files(path = {{ new }}, pattern = {{ pattern }})
    new_paths  <- list.files(path = {{ new }}, pattern = {{ pattern }}, full.names = TRUE)
    new_info   <- tibble::tibble(file = new_files, new_path = new_paths)

    selected_files <- dplyr::full_join(old_info, new_info, by = "file") %>%
      dplyr::arrange(file)

    return(selected_files)

  } else {
    print("one or both of the folders do not exist")
  }
}
