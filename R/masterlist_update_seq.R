#' Updates the sequence column of the masterlist
#' 
#' searches for the primers: psbar2, psbaf, gwsfn, gwsrx, rbclrevnew, f57 
#' must have access to the lab server and [google sheet](https://docs.google.com/spreadsheets/d/1vzVIT5gjQ0yCGwbAyqB4f8pltOkLfWz27TIFTxnm9hk/edit#gid=0)
#' it takes a while to run because we need to go through the server to find all the sequences
#@param test logical - if we are using the [test sheet](https://docs.google.com/spreadsheets/d/1dI2A0X3Ttvb3PTFNhM1R6bmXXaz_XkJyll2vsw8eAvY/edit#gid=1567287105)
#@return
#@export
#'
#@examples #masterlist_update_seq(test = TRUE)
masterlist_update_seq <- function(test = F){ 
  #turning off warnings - set back to 0 to turn on
  options(warn = -1)
  #read in ptm masterlist from sheet
  ptm <- ptm::masterlist()
  # prep file to be joined
  ptm_s <- ptm %>%
    dplyr::mutate(ptm = paste0("PTM", `PTM#`))
  
  #searching for all the sequences on the server
  #this will take a while to run
  message("looking through server folders")
  seq <- list.files("/Volumes/martonelab/Sequences/0_PTM/",
                    full.names = T,
                    recursive = T) #meaning to look through all folders
  
  #seperate out file names for files NOT in folder
  message("organizing sequence files")
  seq_file <- as.data.frame(seq) %>%
    tidyr::separate(seq, into = c("general_folder","rest"), # deals with the folder information
                    sep = "//", extra = "merge") %>% 
    tidyr::separate(rest, into = c("folder","file","fnm"), # deals with the folder information
                    sep = "/", extra = "merge") %>%
    #filter(!is.na(fnm)) %>%
    tidyr::separate(file, # deals with the file name
                    into = c("ptm","gene","other"),
                    sep = "_") %>%
    dplyr::mutate(gene = stringr::str_to_lower(gene), #standardizes some information
                  gene = stringr::str_remove(gene,".ab"),
                  gene = stringr::str_remove(gene,"1"))
  
  #seperate out file names for files IN FOLDERS
  seq_folder <- as.data.frame(seq) %>%
    tidyr::separate(seq, into = c("general_folder","rest"), # deals with the folder information
                    sep = "//", extra = "merge") %>% 
    tidyr::separate(rest, into = c("folder","file","fnm"), # deals with the folder information
                    sep = "/", extra = "merge") %>%
    tidyr::separate(fnm, # deals with the file name
                    into = c("ptm","gene","other"),
                    sep = "_") %>%
    dplyr::mutate(gene = stringr::str_to_lower(gene), #standardizes some information
                  gene = stringr::str_remove(gene,".ab"),
                  gene = stringr::str_remove(gene,"1"))
  
  #combining the two variations in folder naming
  seq_clean <- dplyr::bind_rows(seq_folder, seq_file)
  
  ## dealing with retrieved files
  #update the below if there are new barcode primers used
  #currently we have to most commonly used sequences for CO1, psbA and rbcl
  genes <-  c("rbclrevnew", "gwsfn", "gwsrx", "psbar2", "psbaf", "F7553", "f|F57", "R1150K")
  
  #inserting the F and R for all the gene names
  # clean up sequence names
  seq_rev <- seq_clean %>%
    dplyr::mutate(gene = stringr::str_to_lower(gene)) %>% # accounts for case differences
    dplyr::filter(gene %in% genes)  # selects on the sequences with primers we are interested in
  # combine meta data to the files
  piv <- dplyr::left_join(ptm_s, seq_clean, by = "ptm") %>%
    dplyr::mutate(
      gene = dplyr::if_else(gene == "", "none", gene),
      gene = dplyr::if_else(is.na(gene), "none", gene),
      present = 1) %>%
    tidyr::pivot_wider(
      names_from = gene, values_from = present,
      values_fn = list(present = length)
    ) %>%
    dplyr::distinct(`PTM#`, .keep_all = TRUE)
  
  combine_FR <- function(forward, reverse){
    fr <- paste0(forward, reverse)
    stringr::str_remove_all(fr,"NA")
  }
  #putting in forward and reverse
  piv_genetics <- piv %>% 
    dplyr::mutate(psbar2 = dplyr::if_else(psbar2 == 1, "R",""),
                  psbaf = dplyr::if_else(psbaf == 1, "F",""),
                  p = combine_FR(psbaf,psbar2),
                  gwsfn = dplyr::if_else(gwsfn == 1, "F",""),
                  gwsrx = dplyr::if_else(gwsrx == 1, "R",""),
                  g = combine_FR(gwsfn, gwsrx),
                  rbclrevnew = dplyr::if_else(rbclrevnew == 1, "R",""),
                  f57 =  dplyr::if_else(f57 == 1, "F",""),
                  r = combine_FR(f57, rbclrevnew))
  molecular_complete <- piv_genetics %>% 
    dplyr::mutate(CO1 = dplyr::if_else(is.na(CO1), g, CO1),
                  psbA = dplyr::if_else(is.na(psbA), p, psbA),
                  rbcL = dplyr::if_else(is.na(rbcL), r, rbcL))
  
  message("saving sheet")
  ## clean up and save
  last_col <- grep("Project", colnames(molecular_complete))
  complete_table <- molecular_complete[1:last_col]
  
  ## note update sheet = "Copy of All PTM data" to all PTM Data once confirmed
  save_col <- cbind(complete_table$CO1, complete_table$psbA, complete_table$rbcL) %>% 
    tibble::as_tibble()
  names(save_col) <- c("CO1 NEW","PSBA NEW", "RBCL NEW")
  
  if(test){
    gs_ptm <- googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/1dI2A0X3Ttvb3PTFNhM1R6bmXXaz_XkJyll2vsw8eAvY/edit#gid=1567287105")
  }else{
    gs_ptm <- googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/1vzVIT5gjQ0yCGwbAyqB4f8pltOkLfWz27TIFTxnm9hk/edit#gid=0")
  }
  
  googlesheets4::range_write(gs_ptm,
                             save_col, 
                             sheet = "All PTM data",
                             range = "AL2") #last column
}