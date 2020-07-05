#' Download herbarium template from website
#'
#'
#' @return
#' @export
#'
#' @examples  #herbarium_template()
herbarium_template <- function() {
  #get herbarium template
  beaty_link <- "https://beatymuseum.sites.olt.ubc.ca/files/2019/11/Master-Collector-Excel-Templates.xlsx"
  temp_path <- tempfile("herbarium_template.xlsx")
  download.file(beaty_link, temp_path)
  collector_template <- readxl::read_xlsx(temp_path, 
                                          sheet = "Collector Algae Sheet", skip = 4)
}