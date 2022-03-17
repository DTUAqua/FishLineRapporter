
  library(dplyr)
  library(haven)
  library(lubridate)
  library(openxlsx)
  library(readxl)
  Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip")
  
  setwd("Q:/mynd/kibi/ad_hoc/123_ad_hoc")
  
  
  options(scipen = 999)
  
  years <- c(2019:2019)
  
  dfad <- c()
  
  sss
  for (i in years) {
    
    
    dfad_0 <- readRDS(paste("Q:/dfad/data/Data/udvidet_data/dfad_udvidet", i, ".rds", sep = ""))
    names(dfad_0) <- tolower(names(dfad_0))
    dfad_0$hel[is.na(dfad_0$hel)] <- 0
    dfad_0 <- mutate(dfad_0, year = year(ldato), quarter = quarter(ldato), month = month(ldato))
    
    dfad_sum <- summarise(group_by(dfad_0, year, quarter, dfadfvd_ret, square_ret, art), hel = sum(hel, na.rm = T), vrd = sum(vrd, na.rm = T))
    dfad <- bind_rows(dfad, dfad_sum)
    
  }
  

  ### add stock to dfad
  
  species <- rename(select(read_sas("Q:/mynd/SAS Library/Arter/art.sas7bdat"), start, X_3A_CODE, wormsLatin, art), dk_art = art)
  species <- mutate(species, X_3A_CODE = ifelse(start == "HAG", "BOC", 
                                                ifelse(start == "HAK", "CAA", X_3A_CODE)))
  areas <- select(read_sas("Q:/mynd/SAS Library/Farvand/farvand.sas7bdat"), start, faoArea)
  
  dfad <- left_join(dfad, species, by = c("art" = "start"))
  dfad <- left_join(dfad, areas, by = c("dfadfvd_ret" = "start"))
  
  ##Area corrections to DFAD
  dfad <- mutate(dfad, faoArea = ifelse(substr(faoArea, 1, 9) == "27.3.d.28" & X_3A_CODE != "HER", "27.3.d.28", 
                                            ifelse(substr(faoArea, 1, 9) == "27.3.d.28" & X_3A_CODE == "HER", "27.3.d.28.2",
                                                   ifelse(substr(faoArea, 1, 6) == "27.2.b", "27.2.b", 
                                                          ifelse(substr(faoArea, 1, 6) == "27.6.a", "27.6.a.n",
                                                                 ifelse(substr(faoArea, 1, 8) == "27.5.b.1", "27.5.b.1", 
                                                                        ifelse(substr(faoArea, 1, 5) == "27.1." & X_3A_CODE == "PRA", "27.1", faoArea)))))))
  
  dfad <- rename(dfad, sppFAO = X_3A_CODE, area = faoArea, rect = square_ret)
  
  source("Q:/mynd/kibi/functions/GetStock_v1_2.R")
  dfad_1 <- GetStock(dfad)
  
  # Check stock 
  
  check <- summarise(group_by(filter(dfad_1, stock == "no.defined.stock"), year, dfadfvd_ret, sppFAO, art, dk_art, area, wormsLatin), tons = sum(hel, na.rm = T))
  
  # Merge with ICES stock defintion
  
  dc_stocks <- read_xlsx("Q:/mynd/Assessement_discard_and_the_like/data_call/2020/DC_Annex_1 v2.xlsx", sheet = 4)
  names(dc_stocks)
  
  dfad_final <- left_join(dfad_1, dc_stocks, by = c("stock" = "Code"))
  
  
  
  
  # Output
  
  dfad_mean <- mutate(summarise(group_by(dfad_final, dfadfvd_ret, area, rect, sppFAO, wormsLatin, dk_art, stock, Description), 
                         kg_mean = sum(hel, na.rm = T)/length(years), vrd_mean = sum(vrd, na.rm = T)/length(years)), 
                      years = paste(years[1], years[5], sep = "-"))
  
  write.csv(dfad_mean, "landings_per_stock_mean.csv", row.names = F)
  
  
  ## per year
  
  for (i in years) {
    
    
    dfad_year <- summarise(group_by(filter(dfad_final, year == i), year, dfadfvd_ret, area, rect, sppFAO, wormsLatin, dk_art, stock, Description),
    kg = sum(hel, na.rm = T), vrd = sum(vrd, na.rm = T))
    
    write.csv(dfad_year, paste0("landings_per_stock_", i, ".csv"), row.names = F)
    
  }