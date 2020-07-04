#' Take some ptm numbers and find the photos
#'
#' @param ptm int a ptm number
#' @param results_folder chr destination folder
#'
#' @return
#' @export
#'
#' @examples
#' ### Not run:
#' #get_server_photos(5)
#' ## End(Not run)
get_server_photos <- function(ptm, results_folder = tempdir()){
  path <- "/Volumes/martonelab/Photos"

  stopifnot(dir.exists(path))

  get_ptm <- dplyr::select(ptm, `PTM#`)

  find_photo <- dplyr::mutate(get_ptm, ptm = paste0("PTM",`PTM#`))

  my_files <- purrr::map(find_photo$ptm,
                         ~list.files(path = path,
                           pattern = .,
                           all.files = T,
                           full.names = T))

  # identify the folders
  new_folder <- paste0(here::here(), results_folder)

  # copy the files to the new folder
  my_files <- purrr::map(my_files,
        ~file.copy(., new_folder))
  }
