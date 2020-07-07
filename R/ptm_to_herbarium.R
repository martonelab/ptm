#' convert PTM master list metadata to UBC herbarium list
#' 
#' please review the file 
#'
#' @param ptm list ptm numbers (character)
#' @param filename name of the file to save to a csv file
#'
#' @return
#' @export
#'
#' @examples ptm_to_herbarium(c("120","230","300"), tempfile("test.csv"))
ptm_to_herbarium <- function(ptm, filename = "herbarium.csv") {
  #turning off warnings - set back to 0 to turn on
  options(warn = -1)
  
  #read in masterlist and filter
  ml <- ptm::masterlist()
  submit <- dplyr::filter(ml, `PTM#` %in% ptm)
  
  #picking whether or not to use the Final Determination
  df_nm <- ptm::ptm_pick_nm(submit)
  
  ### Phylum Class Family
  unique <- df_nm %>% 
    dplyr::distinct(g, .keep_all = TRUE)
  
  higher <- ptm::taxonomy(unique$g)
  df_taxon <- dplyr::left_join(df_nm, higher, by = c("g" = "genus")) 
  
  #get the metadata columns from masterlist
  meta <- df_taxon %>%
    dplyr::mutate(Herbarium = "UBC",
                  LabelQty = 1,
                  Qualifier = "",
                  `Collector Number` = paste0("PTM", `PTM#`),
                  `Originally identified as` = paste(gs, "by", `Determined by`, `Determination Date`),
                  Coralline = dplyr::if_else(`Red/Coralline/Green/Brown` == "Coralline", "Yes", "")) %>% 
    tidyr::separate(gs, into = c("Genus", "Species", "type", "spvar"), remove = F) %>% 
    dplyr::mutate(Variety = dplyr::if_else(stringr::str_detect(type, "var"), spvar, ""),
                  Subspecies = dplyr::if_else(stringr::str_detect(type, "sub|sp"), spvar, "")) %>% 
    dplyr::select(Herbarium, LabelQty, phylum, class, Genus, Qualifier, Species, authorship, 
                  Subspecies, Variety,
                  Country, StateProvince, Locality, Latitude, 
                  Longitude, Habitat, Depth, `Reproductive Status`, `Date Collected`, `Primary Collector`, `Other collectors`,
                  `Collector Number`,`Determined by`, `Determination Date`, `Field Notes`, `Originally identified as`,
                  GenbankNumber, BOLDNumber, Coralline)
  
  collector_template <- ptm::herbarium_template()
  nc <- names(collector_template)
  nm <- names(meta)
  
  #create an empty template
  ct <- data.frame(matrix(NA, nrow=nrow(meta), ncol=length(nc)))
  names(ct) <- nc
  
  df_combine <- dplyr::bind_cols(ct[,which("Database Field" == nc)],
                                 meta[1:which("Subspecies" == nm)],
                                 ct[,which("Subspecies Author" == nc)],
                                 meta[which("Variety" == nm)],
                                 ct[,13:15],
                                 meta[11:which("Latitude" == nm)],
                                 ct[,20:21],
                                 meta[which("Longitude" == nm)],
                                 ct[which("VLongMinute" == nc):which("VCoordinateUncertainty" == nc)],
                                 meta[which("Habitat" == nm)],
                                 ct[which("Microhabitat" == nc)],
                                 meta[which("Depth" == nm):which("Originally identified as" == nm)],
                                 ct[which("Annotation 1" == nc):which("Citations" == nc)],
                                 meta[which("GenbankNumber" == nm):length(nm)],
                                 ct[length(nc)])
  names(df_combine) <- nc
  
  readr::write_csv(df_combine, filename, na = "")
}