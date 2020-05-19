#' Gets the higher taxonomy and authorship of the species using the Worms database
#'
#' @param sciname chr or list
#'
#' @return dataframe
#' @export
#'
#' @examples
#' taxonomy("Ulva linza")
#' taxonomy("Mastocarpus")
#'
#' # A list of species
#' taxonomy(c("Ulva linza", "Mastocarpus"))
#'
taxonomy <- function(sciname){
  worms_nm <- worrms::wm_records_names(sciname)
  df_authors <-  purrr::map_dfr(worms_nm,`[`) # puts everyting into a dataframe

  dplyr::mutate(df_authors,
         authorship = stringr::str_remove(authority,
                                 ", [:digit:]{4}")) %>%
  dplyr::select(scientificname, rank, kingdom, phylum, class, order, family, genus, authorship)
}
