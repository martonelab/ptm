#' Converts data from the PTM masterlist to the bold template
#'
#' @param ptm a list of ptm numbers
#' @param filename character - file name
#'
#' @return
#' @export
#'
#' @examples #ptm_to_bold(c("111", "222", "333"), tempfile("savefile"))
ptm_to_bold <- function(ptm, filename) {
  master <- ptm::masterlist()
  
  submit <- master %>% 
    dplyr::filter(master$`PTM#` %in% ptm,
           !is.na(`PTM#`)) %>% 
    ptm::ptm_pick_nm()
  
  #download bold data
  bold_link <- "https://github.com/martonelab/BoldWorkflow/raw/master/01_spreadsheet/SpecimenDataV3Transitional_BOLDTemplate.xls"
  bold_file <- tempfile("bold_template.xls")
  download.file(bold_link, bold_file)
  
  #read in each xls file
  info <- readxl::read_xls(bold_file, skip = 1, sheet = 1)
  e_info <- info[FALSE,]
  tax <- readxl::read_xls(bold_file, skip = 1, sheet = 2)
  e_taxon <- tax[FALSE,]
  spe <- readxl::read_xls(bold_file, skip = 1, sheet = 3)
  e_specimen <- spe[FALSE,]
  coll <- readxl::read_xls(bold_file, skip = 1, sheet = 4)
  e_collect <- coll[FALSE,]
  
  higher <-  taxize::classification(submit$`gs`, db="bold") %>% 
    purrr::map(., ~dplyr::select(.x, -id)) %>% 
    purrr::map_dfr(., ~tidyr::pivot_wider(.x,
                               names_from = rank,
                               values_from = name))
  
  df_taxon <- dplyr::left_join(submit, higher, by = c("gs" = "species")) 
  
  c_info <- df_taxon %>% 
    dplyr::mutate(# Voucher
      Sample_ID = paste0("PTM",`PTM#`),
      Field_ID = Sample_ID,
      Museum_ID = `Accession #`,
      Collection_Code = "",
      Insitution_Storing = "University of British Columbia, Herbarium",
      #Taxon
      Species = `Final determination`,
      Identifier = `Determined by`,
      Identifier_Email = "",
      Identifier_Insitution  = "University of British Columbia",
      `Identification_Method` = "",
      `Taxonomy Notes` = "",
      #collect
      Collectors = paste0(`Primary Collector`,", ",df_taxon$`Other collectors`),
      Collection_Date = `Date Collected`,
      Country_Ocean = Country, 
      State_Province = StateProvince,
      Region = "",
      #Specimen Details
      Sex = "",
      Reproduction = `Reproductive Status`,
      Life_Stage = "",
      Extra_info = "",
      Note = `Field Notes`,
      Voucher_stat = "",
      Tissue_des = "",
      Assoc_tax = "",
      Assoc_spe = "",
      External = ""
      ) %>% #depends on dataset,
    tidyr::separate(Note, c("Site_Code","Habitat"), ";", extra = "drop") %>% 
    tidyr::separate(Locality, c("Sector","Exact_site"),",")
  c_info <- c_info %>% 
    dplyr::mutate(Elevation = "",
           Depth = Depth,
           Elevation_Precision = "",
           Depth_Precision = "",
           GPS_Source	= "",
           Coordinate_Accuracy= "",
           Event_Time = "",
           Collection_Date_Accuracy	= "",
           Habitat = c_info$Habitat,
           Sampling_Protocol = "",
           Collection_Notes = c_info$Habitat,
           Site_Code = c_info$Site_Code,
           Collection_Event_ID = "",
           Note = "")
  
  #selecting the relevant columns
  voucher <- c_info %>%
    dplyr::select(`Sample_ID`,`Field_ID`, `Museum_ID`, `Collection_Code`, `Insitution_Storing`)
  names(voucher) <- names(e_info)
  
  
  ## ------------------------------------------------------------------------------------------------------
  s <- c_info %>%
    dplyr::select(`Sample_ID`)
  t_high <-  df_taxon %>%
    dplyr::select(`phylum`, `class`, `order`, `family`, subfamily, `genus`)
  t_sub <- c_info %>% 
    dplyr::select(`Species`,`Identifier`, `Identifier_Email`, `Identifier_Insitution`, `Identification_Method`, `Taxonomy Notes`)
  taxon <- cbind(s,t_high,t_sub)
  names(taxon) <- names(e_taxon)
  collect <-  c_info %>%
    dplyr::select(`Sample_ID`,`Collectors`, `Collection_Date`, `Country_Ocean`,`State_Province`, `Region`, `Sector`, `Exact_site`, `Latitude`, `Longitude`,`Elevation`, Depth, Elevation_Precision, Depth_Precision, Depth_Precision, GPS_Source, Coordinate_Accuracy, Event_Time, Collection_Date_Accuracy, Habitat, Sampling_Protocol, Collection_Notes, Site_Code, Collection_Event_ID)
  names(collect) <- names(e_collect)
  
  ## ------------------------------------------------------------------------------------------------------
  sd <- c_info %>% 
    dplyr::select(Sample_ID,Sex, Reproduction, Life_Stage, Extra_info, Note, Voucher_stat, Tissue_des, Assoc_tax, Assoc_spe, External)
  names(sd) <- names(e_specimen)
  
  lod <- list("Voucher Info" = voucher, "Taxonomy" = taxon, "Specimen Details" =sd, "Collection Data" = collect )
  openxlsx::write.xlsx(lod, file = filename, append = TRUE)
}

