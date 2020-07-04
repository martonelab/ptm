#' Opens PTM Masterlist
#'
#' - Pulls from the PTM Masterlist Google Sheet and reads the file
#' - *note* you must have been granted access on your gmail before using
#'
#' @return dataframe
#' @export
#'
#' @examples masterlist()
masterlist <- function(){
  pml <- "https://docs.google.com/spreadsheets/d/1vzVIT5gjQ0yCGwbAyqB4f8pltOkLfWz27TIFTxnm9hk/edit#gid=0"
  suppressWarnings(googlesheets4::read_sheet(pml, sheet = "All PTM data", skip = 1,
                            col_types = "iccccccDcccccccccccccccccccnnccccc-iiccc"))
}
