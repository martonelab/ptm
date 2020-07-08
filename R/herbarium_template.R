#' Download herbarium template from website
#'
#'gets the most recent herbarium template from the website
#'
#' @return
#' @export
#'
#' @examples  #herbarium_template()
herbarium_template <- function() {
  #get herbarium template
  beaty_link <- "https://beatymuseum.sites.olt.ubc.ca/files/2019/11/Master-Collector-Excel-Templates.xlsx"
  temp_path <- tempfile("herbarium_template.xlsx")
  download.file(beaty_link, "templates/herbarium_template.xlsx")
  collector_template <- readxl::read_xlsx(temp_path, 
                                          sheet = "Collector Algae Sheet", skip = 4)
}