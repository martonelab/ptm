#' picks a determination to go with
#' 
#' either the Final determination if there is one of the determindation in the field if there is nothing
#'
#'@param ml data frame the ptm masterlist
#'
#' @return
#' @export
#'
#' @examples
ptm_pick_nm <- function(ml) {
  ml %>% 
    dplyr::mutate(choice = dplyr::if_else(is.na(ml$`Final determination`),
                                          ml$`Determination in the field`,
                                          ml$`Final determination`),
                  gs = choice)
}