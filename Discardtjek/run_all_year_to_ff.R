# This script is handy when you need to knit multiple instances of kvartalsrapport til ff
library(pacman)
p_load(knitr,rmarkdown,haven)

setwd("M:/123 - FiskeLine/Rapporter/Discardtjek")

filePath <- "Q:/dfad/users/trkj/data/data_fil_ff/"
year<- 2019
quarters <- 1
dateString <- format(Sys.Date(),"%Y%m%d")
# 
# species <- NULL
# areas <- NULL#"Østlige Østersø"
# # logbookPath <- sprintf("Q:/dfad/data/Data/logdata/nylog%s.sas7bdat",year-2000)
# logbookPath <- "Q:/dfad/users/trkj/data/logbook18.rds"
# landingDecPath <- "Q:/dfad/data/Data/afrdata/bms_tabel.sas7bdat"
# 
# # if (!exists("logbook")){logbook <- read_sas(logbookPath)}
# if (!exists("logbook")){logbook <- readRDS(logbookPath)}
# if (!exists("landingDec")){landingDec <- read_sas(landingDecPath)}


for (quarter in quarters){
  
  render(input = "rapport_til_ff_040419.Rmd",output_dir=getwd(),
       output_file = sprintf("%sKvartalsrapport_%s_%s_%s.pdf",filePath,year,quarter,dateString),
       encoding = "UTF-8",output_format = "pdf_document",run_pandoc = T,
       envir = new.env(),params = list(quarter=quarter,year=year)
       )
  
  # render(output_format = "pdf-document")
}
