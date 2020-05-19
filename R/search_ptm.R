#' Searches for species in PTM masterlist
#' looks in the Final determination and Determination in the field
#'
#'
#' @param name scientific name of seaweed
#'
#' @return dataframe
#' @export
#'
#' @examples search_ptm("Ulva lactuca")
search_ptm <- function(name){
  ptm <- masterlist()

  dplyr::filter(ptm,
                stringr::str_detect(ptm$`Final determination`, name) |
                  stringr::str_detect(ptm$`Determination in the field`, name),
                !is.na(ptm$`Photos on server`))
}
