# Create R_StockSpeciesSpaceTime

Existing relation: ./Lookup and relational tables/R_StockSpeciesSpaceTime/oldStuff/GetStock_v1_2.R
Using the Existing relation: ./Lookup and relational tables/R_StockSpeciesSpaceTime/oldStuff/landings_per_stock.R

## Check and update existing lookups
1.	L_DFUArea - areaICES on all DFUArea
2.	L_Species - icesCode on all speciesCodes &| speciesFAO on all speciescodes - only check relevant species (Are there eny difference between ICES speciescodes in the stock and FAO species codes?)

## Update the existing relation
3.	Include StatisticalRectangle for relevant stocks
4.	Include quarter for relevant stocks
5.	Add DFUArea and speciesCode &| id's

## Checks
1.	Check that the existing relation is up to date //vocab.ices.dk/?ref=357
2.	Check the new relation on newer data in FishLine
3.	…….

## Questions to AnchorLab
1.	How do we maintain the relation? – stocks change sometimes
2.	What info would you prefer in the relation in respect to DFUArea and speciesCode?
