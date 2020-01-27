
options(scipen = 999)

library(dplyr)
library(haven)
library(tidyr)

dir_in <- "Q:\\mynd\\kibi\\fishLine\\FishLineRapporter\\Biomasseberegninger\\creating_a_new_biomasse_report\\output"
dir_out <- "Q:\\dfad\\users\\kspl\\data\\musling"

species <- "BMS"


orgi <- readRDS("Q:\\mynd\\kibi\\fishLine\\FishLineRapporter\\Biomasseberegninger\\creating_a_new_biomasse_report\\output\\Biomasse_TRU_v0_23Jan2020.rds")
new <- readRDS("Q:\\mynd\\kibi\\fishLine\\FishLineRapporter\\Biomasseberegninger\\creating_a_new_biomasse_report\\output\\Biomasse_TRU_v1_22Jan2020.rds")

#Rename certain columns in new to make comparison
colnames(new)[23:ncol(new)] <- paste("New", colnames(new)[23:ncol(new)], sep = "_")

# Join by as much as possible
comp <- as.data.frame(full_join(orgi, new))

# Replave infinet values with 0

comp[comp == "Inf"] <- 0

# check med falsk
#comp$New_MeanLength_cm_SMÅ <- 1
#tjek <- head(comp)
#tjek$TotalWeight_kg_SMÅ <- ifelse(tjek$TotalWeight_kg_SMÅ == 0, 1, NA)
#tjek$fangst_oes_kg_m2_STO <- ifelse(tjek$fangst_oes_kg_m2_STO == 0, 1, NA)
#comp  <- rbind(tjek, comp)

#Compare the different columns
#TotalWeight_kg_SMÅ
identical(comp[['TotalWeight_kg_SMÅ']],comp[['New_TotalWeight_kg_SMÅ']])

#TotalNumber_SMÅ
identical(comp[['TotalNumber_SMÅ']],comp[['New_TotalNumber_SMÅ']])

#MeanLength_cm_SMÅ
identical(comp[['MeanLength_cm_SMÅ']],comp[['New_MeanLength_cm_SMÅ']])

#Min_Length_cm_SMÅ
identical(comp[['Min_Length_cm_SMÅ']],comp[['New_Min_Length_cm_SMÅ']])

#TotalWeight_kg_STO
identical(comp[['TotalWeight_kg_STO']],comp[['New_TotalWeight_kg_STO']])

#TotalNumber_STO
identical(comp[['TotalNumber_STO']],comp[['New_TotalNumber_STO']])

#MeanLength_cm_STO
identical(comp[['MeanLength_cm_STO']],comp[['New_MeanLength_cm_STO']])

#Min_Length_cm_STO
identical(comp[['Min_Length_cm_STO']],comp[['New_Min_Length_cm_STO']])

#TotalWeight_kg
identical(comp[['TotalWeight_kg']],comp[['New_TotalWeight_kg']])

#TotalNumber
identical(comp[['TotalNumber']],comp[['New_TotalNumber']])

#MeanLength_cm
identical(comp[['MeanLength_cm']],comp[['New_MeanLength_cm']])

#Min_Length_cm
identical(comp[['Min_Length_cm']],comp[['New_Min_Length_cm']])

#Fangst_kg_m2_SMÅ
identical(comp[['Fangst_kg_m2_SMÅ']],comp[['New_Fangst_kg_m2_SMÅ']])

#Fangst_kg_m2_STO
identical(comp[['Fangst_kg_m2_STO']],comp[['New_Fangst_kg_m2_STO']])

#Fangst_kg_m2
identical(comp[['Fangst_kg_m2']],comp[['New_Fangst_kg_m2']])

#fangst_oes_kg_m2_SMÅ
identical(comp[['fangst_oes_kg_m2_SMÅ']],comp[['New_fangst_oes_kg_m2_SMÅ']])

#fangst_oes_kg_m2_STO
identical(comp[['fangst_oes_kg_m2_STO']],comp[['New_fangst_oes_kg_m2_STO']])

#fangst_oes_kg_m2
identical(comp[['fangst_oes_kg_m2']],comp[['New_fangst_oes_kg_m2']])

#Korrigeret_oes_kg_m2
identical(comp[['Korrigeret_oes_kg_m2']],comp[['New_Korrigeret_oes_kg_m2']])


#==================================================================
# Direct comparison

comp_red <- comp[c(1,3:8,21:60)]

comp_red$diff_TotalWeight_kg_SMÅ <- comp_red$TotalWeight_kg_SMÅ-comp_red$New_TotalWeight_kg_SMÅ
comp_red$diff_TotalNumber_SMÅ <- comp_red$TotalNumber_SMÅ-comp_red$New_TotalNumber_SMÅ
comp_red$diff_MeanLength_cm_SMÅ <- comp_red$MeanLength_cm_SMÅ-comp_red$New_MeanLength_cm_SMÅ
comp_red$diff_Min_Length_cm_SMÅ <- comp_red$Min_Length_cm_SMÅ-comp_red$New_Min_Length_cm_SMÅ

comp_red$diff_TotalWeight_kg_STO <- comp_red$TotalWeight_kg_STO-comp_red$New_TotalWeight_kg_STO
comp_red$diff_TotalNumber_STO <- comp_red$TotalNumber_STO-comp_red$New_TotalNumber_STO
comp_red$diff_MeanLength_cm_STO <- comp_red$MeanLength_cm_STO-comp_red$New_MeanLength_cm_STO
comp_red$diff_Min_Length_cm_STO <- comp_red$Min_Length_cm_STO-comp_red$New_Min_Length_cm_STO

comp_red$diff_TotalWeight_kg <- comp_red$TotalWeight_kg-comp_red$New_TotalWeight_kg
comp_red$diff_TotalNumber <- comp_red$TotalNumber-comp_red$New_TotalNumber
comp_red$diff_MeanLength_cm <- comp_red$MeanLength_cm-comp_red$New_MeanLength_cm
comp_red$diff_Min_Length_cm <- comp_red$Min_Length_cm-comp_red$New_Min_Length_cm

comp_red$diff_Fangst_kg_m2_SMÅ <- comp_red$Fangst_kg_m2_SMÅ-comp_red$New_Fangst_kg_m2_SMÅ
comp_red$diff_Fangst_kg_m2_STO <- comp_red$Fangst_kg_m2_STO-comp_red$New_Fangst_kg_m2_STO
comp_red$diff_Fangst_kg_m2 <- comp_red$Fangst_kg_m2-comp_red$New_Fangst_kg_m2

# comp_red$diff_fangst_oes_kg_m2_SMÅ <- comp_red$fangst_oes_kg_m2_SMÅ-comp_red$New_fangst_oes_kg_m2_SMÅ
# comp_red$diff_fangst_oes_kg_m2_STO <- comp_red$fangst_oes_kg_m2_STO-comp_red$New_fangst_oes_kg_m2_STO
# comp_red$diff_fangst_oes_kg_m2 <- comp_red$fangst_oes_kg_m2-comp_red$New_fangst_oes_kg_m2
# comp_red$diff_Korrigeret_oes_kg_m2 <- comp_red$Korrigeret_oes_kg_m2-comp_red$New_Korrigeret_oes_kg_m2





#Subset for which ones got FALSE

Issue <- subset(comp_red, diff_TotalWeight_kg_SMÅ != 0 |
                  diff_TotalNumber_SMÅ != 0 | 
                  diff_MeanLength_cm_SMÅ != 0 |
                  diff_Min_Length_cm_SMÅ != 0 | 
                  diff_TotalWeight_kg_STO != 0 |
                  diff_TotalNumber_STO != 0 | 
                  diff_MeanLength_cm_STO != 0 |
                  diff_Min_Length_cm_STO != 0 | 
                  diff_TotalWeight_kg != 0 |
                  diff_TotalNumber != 0 | 
                  diff_MeanLength_cm != 0 |
                  diff_Min_Length_cm != 0 | 
                  diff_Fangst_kg_m2_SMÅ != 0 |
                  diff_Fangst_kg_m2_STO != 0 | 
                  diff_Fangst_kg_m2 != 0)

Issue_2 <- subset(comp_red, diff_TotalWeight_kg_SMÅ != 0 | is.na(diff_TotalWeight_kg_SMÅ) |
                  diff_TotalNumber_SMÅ != 0 | is.na(diff_TotalNumber_SMÅ)  | 
                  diff_MeanLength_cm_SMÅ != 0 | is.na(diff_MeanLength_cm_SMÅ)  |
                  diff_Min_Length_cm_SMÅ != 0 | is.na(diff_Min_Length_cm_SMÅ)  | 
                  diff_TotalWeight_kg_STO != 0 | is.na(diff_TotalWeight_kg_STO)  |
                  diff_TotalNumber_STO != 0 | is.na(diff_TotalNumber_STO)  | 
                  diff_MeanLength_cm_STO != 0 | is.na(diff_MeanLength_cm_STO)  |
                  diff_Min_Length_cm_STO != 0 | is.na(diff_Min_Length_cm_STO)  | 
                  diff_TotalWeight_kg != 0 | is.na(diff_TotalWeight_kg)  |
                  diff_TotalNumber != 0 | is.na(diff_TotalNumber)  | 
                  diff_MeanLength_cm != 0 | is.na(diff_MeanLength_cm)  |
                  diff_Min_Length_cm != 0 | is.na(diff_Min_Length_cm)  | 
                  diff_Fangst_kg_m2_SMÅ != 0 | is.na(diff_Fangst_kg_m2_SMÅ)  |
                  diff_Fangst_kg_m2_STO != 0 | is.na(diff_Fangst_kg_m2_STO)  | 
                  diff_Fangst_kg_m2 != 0 | is.na(diff_Fangst_kg_m2))

# 
#| diff_fangst_oes_kg_m2_SMÅ != 0 | diff_fangst_oes_kg_m2_STO != 0
#| diff_fangst_oes_kg_m2 != 0 | diff_Korrigeret_oes_kg_m2 != 0
