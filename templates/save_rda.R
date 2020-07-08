#make sure the data is in a format readable in package
collector_template <- readxl::read_xlsx("templates/herbarium_template.xlsx", 
                                        sheet = "Collector Algae Sheet", skip = 4)

usethis::use_data(collector_template)

#bold link
bold_file <- "templates/bold_template.xls"

#read in each xls file
info <- readxl::read_xls(bold_file, skip = 1, sheet = 1)
e_info <- info[FALSE,]
tax <- readxl::read_xls(bold_file, skip = 1, sheet = 2)
e_taxon <- tax[FALSE,]
spe <- readxl::read_xls(bold_file, skip = 1, sheet = 3)
e_specimen <- spe[FALSE,]
coll <- readxl::read_xls(bold_file, skip = 1, sheet = 4)
e_collect <- coll[FALSE,]

usethis::use_data(e_info)
usethis::use_data(e_taxon)
usethis::use_data(e_specimen)
usethis::use_data(e_collect)




    