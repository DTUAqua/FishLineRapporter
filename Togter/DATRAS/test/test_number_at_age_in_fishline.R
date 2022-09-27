

# Compare number of ages in Fishline with output from DATRAS-udtræk-new report


library(RODBC)
library(dplyr)
library(ggplot2)

path_output <- "Q:/mynd/kibi/fishLine/FishLineRapporter/Togter/DATRAS/test/"


channel <- odbcConnect("FishLineDW")
age <- sqlQuery(
  channel,
  paste0(
    "SELECT     ageId, animalId, year, cruise, trip, tripType, station, dateGearStart, quarterGearStart, dfuArea, statisticalRectangle, gearQuality, gearType, meshSize, speciesCode, landingCategory, dfuBase_Category, sizeSortingEU, sizeSortingDFU, ovigorous, cuticulaHardness, treatment, 
                  speciesList_sexCode, sexCode, representative, individNum, number, length, age, agePlusGroup, otolithWeight, edgeStructure, otolithReadingRemark, hatchMonth, hatchMonthRemark, ageReadId, ageReadName, hatchMonthReaderId, hatchMonthReaderName, remark, genetics, 
                  visualStock, geneticStock
FROM        Age
WHERE     (cruise IN ('E349', 'E562', 'L151', 'R500', 'L610', 'Reyklanes', 'SALLING', 'Reykjanes', 'LONNY HEDVIG')) OR
                  (cruise LIKE 'TOBISSKRABETOGT%') "
  )
)
close(channel)

age_null <- subset(age, is.na(age))

age_1 <- subset(age, !is.na(age))

age_sum <- summarise(group_by(age_1, year, speciesCode), no_age_number = sum(number, na.rm = T))


ggplot(age_sum, aes(x = year, y = no_age_number, group = speciesCode)) +
  geom_bar(stat = "identity")

write.csv(age_sum, paste0(path_output, "test_number_at_age_in_fishline_tobis.csv"), row.names = F)
