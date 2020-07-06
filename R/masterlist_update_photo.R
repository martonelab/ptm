#' Update photos column in masterlist
#'
#' @param test logical - if we are using the 
#'
#' @return
#' @export
#'
#' @examples #masterlist_update_photo()
masterlist_update_photo <- function(test = F){
  ptm <- masterlist()
  
  #need to be connected to the server to get photo information
  p <- "/Volumes/martonelab/Photos"
  
  message("looking through server folders")
  my_files <- list.files(
    path = p,
    all.files = T,
    full.names = T,
    recursive = T)
  
  #clean up output
  ptm_photo<- as.data.frame(my_files) %>%
    tidyr::separate(my_files,
             into = c("folder", "m", "p", "ph", "range", "photo"),
             sep = "/",
             extra = "merge") %>%
    tidyr::separate(photo, into = c("ptm", "extension"), sep = "\\.")
  
  img_clean <- ptm_photo %>% 
    dplyr::filter(stringr::str_detect(ptm,"PTM"),
           range != "Quadra Bioblitz 2019") %>% 
    dplyr::mutate(ptm = stringr::str_remove(ptm, "PTM"),
           num = stringr::str_extract(ptm,"[[:digit:]]{2,4}"),
           num = as.numeric(num)) %>% 
    dplyr::group_by(num) %>% 
    dplyr::count(name = "Photos on server NEW")
  
  ptm_p <- dplyr::left_join(ptm, img_clean, by = c(`PTM#` = "num")) %>% 
    dplyr::select(-`Photos on server NEW`)
  
  if(test){
    gs_ptm <- googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/1dI2A0X3Ttvb3PTFNhM1R6bmXXaz_XkJyll2vsw8eAvY/edit#gid=1567287105")
  }else{
    gs_ptm <- googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/1vzVIT5gjQ0yCGwbAyqB4f8pltOkLfWz27TIFTxnm9hk/edit#gid=0")
  }
  save_col <- tibble::as_tibble(ptm_p$`Photos on server`, .name_repair = "minimal")
  names(save_col) <- "Photos on server NEW"
  
  googlesheets4::range_write(gs_ptm,
                             save_col, 
                             sheet = "All PTM data",
                             range = "AK2")
}