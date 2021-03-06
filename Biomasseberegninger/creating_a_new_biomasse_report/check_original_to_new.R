
options(scipen = 999)

library(dplyr)
library(haven)
library(tidyr)

dir_in <- "Q:\\mynd\\kibi\\fishLine\\FishLineRapporter\\Biomasseberegninger\\creating_a_new_biomasse_report\\output"
dir_out <- "Q:\\dfad\\users\\kspl\\data\\musling"




orgi <- readRDS("Q:\\mynd\\kibi\\fishLine\\FishLineRapporter\\Biomasseberegninger\\creating_a_new_biomasse_report\\output\\Biomasse_OES_orginal_06Dec2019.rds")
new <- readRDS("Q:\\mynd\\kibi\\fishLine\\FishLineRapporter\\Biomasseberegninger\\creating_a_new_biomasse_report\\output\\Biomasse_OES_Compare_to_original_06Dec2019.rds")

#Rename certain columns in new to make comparison
colnames(new)[23:ncol(new)] <- paste("New", colnames(new)[23:ncol(new)], sep = "_")


# Join by as much as possible
comp <- left_join(orgi, new)

# check med falsk
#comp$New_MeanLength_cm_SM� <- 1
#tjek <- head(comp)
#tjek$TotalWeight_kg_SM� <- ifelse(tjek$TotalWeight_kg_SM� == 0, 1, NA)
#tjek$fangst_oes_kg_m2_STO <- ifelse(tjek$fangst_oes_kg_m2_STO == 0, 1, NA)
#comp  <- rbind(tjek, comp)

#Compare the different columns
#TotalWeight_kg_SM�
identical(comp[['TotalWeight_kg_SM�']],comp[['New_TotalWeight_kg_SM�']])

#TotalNumber_SM�
identical(comp[['TotalNumber_SM�']],comp[['New_TotalNumber_SM�']])

#MeanLength_cm_SM�
identical(comp[['MeanLength_cm_SM�']],comp[['New_MeanLength_cm_SM�']])

#Min_Length_cm_SM�
identical(comp[['Min_Length_cm_SM�']],comp[['New_Min_Length_cm_SM�']])

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

#Fangst_kg_m2_SM�
identical(comp[['Fangst_kg_m2_SM�']],comp[['New_Fangst_kg_m2_SM�']])

#Fangst_kg_m2_STO
identical(comp[['Fangst_kg_m2_STO']],comp[['New_Fangst_kg_m2_STO']])

#Fangst_kg_m2
identical(comp[['Fangst_kg_m2']],comp[['New_Fangst_kg_m2']])

#fangst_oes_kg_m2_SM�
identical(comp[['fangst_oes_kg_m2_SM�']],comp[['New_fangst_oes_kg_m2_SM�']])

#fangst_oes_kg_m2_STO
identical(comp[['fangst_oes_kg_m2_STO']],comp[['New_fangst_oes_kg_m2_STO']])

#fangst_oes_kg_m2
identical(comp[['fangst_oes_kg_m2']],comp[['New_fangst_oes_kg_m2']])

#Korrigeret_oes_kg_m2
identical(comp[['Korrigeret_oes_kg_m2']],comp[['New_Korrigeret_oes_kg_m2']])


#==================================================================
# Direct comparison

comp_red <- comp[c(1,3:8,21:60)]

comp_red$diff_TotalWeight_kg_SM� <- comp_red$TotalWeight_kg_SM�-comp_red$New_TotalWeight_kg_SM�
comp_red$diff_TotalNumber_SM� <- comp_red$TotalNumber_SM�-comp_red$New_TotalNumber_SM�
comp_red$diff_MeanLength_cm_SM� <- comp_red$MeanLength_cm_SM�-comp_red$New_MeanLength_cm_SM�
comp_red$diff_Min_Length_cm_SM� <- comp_red$Min_Length_cm_SM�-comp_red$New_Min_Length_cm_SM�

comp_red$diff_TotalWeight_kg_STO <- comp_red$TotalWeight_kg_STO-comp_red$New_TotalWeight_kg_STO
comp_red$diff_TotalNumber_STO <- comp_red$TotalNumber_STO-comp_red$New_TotalNumber_STO
comp_red$diff_MeanLength_cm_STO <- comp_red$MeanLength_cm_STO-comp_red$New_MeanLength_cm_STO
comp_red$diff_Min_Length_cm_STO <- comp_red$Min_Length_cm_STO-comp_red$New_Min_Length_cm_STO

comp_red$diff_TotalWeight_kg <- comp_red$TotalWeight_kg-comp_red$New_TotalWeight_kg
comp_red$diff_TotalNumber <- comp_red$TotalNumber-comp_red$New_TotalNumber
comp_red$diff_MeanLength_cm <- comp_red$MeanLength_cm-comp_red$New_MeanLength_cm
comp_red$diff_Min_Length_cm <- comp_red$Min_Length_cm-comp_red$New_Min_Length_cm

comp_red$diff_Fangst_kg_m2_SM� <- comp_red$Fangst_kg_m2_SM�-comp_red$New_Fangst_kg_m2_SM�
comp_red$diff_Fangst_kg_m2_STO <- comp_red$Fangst_kg_m2_STO-comp_red$New_Fangst_kg_m2_STO
comp_red$diff_Fangst_kg_m2 <- comp_red$Fangst_kg_m2-comp_red$New_Fangst_kg_m2

comp_red$diff_fangst_oes_kg_m2_SM� <- comp_red$fangst_oes_kg_m2_SM�-comp_red$New_fangst_oes_kg_m2_SM�
comp_red$diff_fangst_oes_kg_m2_STO <- comp_red$fangst_oes_kg_m2_STO-comp_red$New_fangst_oes_kg_m2_STO
comp_red$diff_fangst_oes_kg_m2 <- comp_red$fangst_oes_kg_m2-comp_red$New_fangst_oes_kg_m2
comp_red$diff_Korrigeret_oes_kg_m2 <- comp_red$Korrigeret_oes_kg_m2-comp_red$New_Korrigeret_oes_kg_m2





#Subset for which ones got FALSE

Issue <- subset(comp_red, diff_TotalWeight_kg_SM� != 0 | diff_TotalNumber_SM� != 0 | diff_MeanLength_cm_SM� != 0
                | diff_Min_Length_cm_SM� != 0 | diff_TotalWeight_kg_STO != 0
                | diff_TotalNumber_STO != 0 | diff_MeanLength_cm_STO != 0
                | diff_Min_Length_cm_STO != 0 | diff_TotalWeight_kg != 0
                | diff_TotalNumber != 0 | diff_MeanLength_cm != 0
                | diff_Min_Length_cm != 0 | diff_Fangst_kg_m2_SM� != 0
                | diff_Fangst_kg_m2_STO != 0 | diff_Fangst_kg_m2 != 0
                | diff_fangst_oes_kg_m2_SM� != 0 | diff_fangst_oes_kg_m2_STO != 0
                | diff_fangst_oes_kg_m2 != 0 | diff_Korrigeret_oes_kg_m2 != 0)

