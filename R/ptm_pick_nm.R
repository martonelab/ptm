#' Picks a name to use
#' 
#' either the Final determination if there is one of the determindation in the field if there is nothing
#'
#'@param ml data frame the ptm masterlist
#'
#' @return
#' @export
#'
#' @examples #ptm_pick_nm(dataframe)
ptm_pick_nm <- function(ml) {
  ml %>% 
    dplyr::mutate(choice = dplyr::if_else(is.na(ml$`Final determination`),
                                          ml$`Determination in the field`,
                                          ml$`Final determination`),
                  gs = choice) %>% 
    tidyr::separate(gs, into = c("g", "s"), remove = FALSE)
}