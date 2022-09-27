


# Draft relation - correct columns, but maybe not correct content :-)

# The draft relation is based on a reference table developed under fishPi2

library(dplyr)

stockref_path = "Q:/mynd/SAS Library/Stock/"

stock_ref <- read.csv(paste0(stockref_path, "stock.csv"))

names(stock_ref)

# Add rect when needed ----

## Sandeel ----


san_1 <- data.frame(
  sppFAO = "SAN",
  stock = "san.sa.1r",
  rect = c(
    "31F0",
    "31F1",
    "31F2",
    "31F3",
    "32F0",
    "32F1",
    "32F2",
    "32F3",
    "33F1",
    "33F2",
    "33F3",
    "33F4",
    "34F0",
    "34F1",
    "34F2",
    "34F3",
    "34F4",
    "35F0",
    "35F1",
    "35F2",
    "35F3",
    "35F4",
    "35F5",
    "36E9",
    "36F0",
    "36F1",
    "36F2",
    "36F3",
    "36F4",
    "36F5",
    "36F6",
    "37E9",
    "37F0",
    "37F1",
    "37F2",
    "37F3",
    "37F4",
    "37F5",
    "37F6",
    "38F0",
    "38F1",
    "38F2",
    "38F3",
    "38F4",
    "38F5",
    "39F0",
    "39F1",
    "39F2",
    "39F3",
    "39F4",
    "39F5",
    "40F0",
    "40F1",
    "40F2",
    "40F3",
    "40F4",
    "40F5",
    "41F4",
    "41F5"
  )
)

san_2 <- data.frame(
  sppFAO = "SAN",
  stock = "san.sa.2r",
  rect = c(
    "35F7",
    "35F8",
    "36F7",
    "36F8",
    "36F9",
    "37F7",
    "37F8",
    "38F6",
    "38F7",
    "38F8",
    "39F6",
    "39F7",
    "39F8",
    "40F6",
    "40F7",
    "40F8",
    "41F6",
    "41F7",
    "41F8",
    "42F6",
    "42F7",
    "42F8",
    "43F7",
    "43F8",
    "43F9",
    "44F9",
    "44G0",
    "45G0",
    "45G1",
    "46G1"
  )
)

san_3 <- data.frame(
  sppFAO = "SAN",
  stock = "san.sa.3r",
  rect = c(
    "41F1",
    "41F2",
    "41F3",
    "42F1",
    "42F2",
    "42F3",
    "42F4",
    "42F5",
    "43F1",
    "43F2",
    "43F3",
    "43F4",
    "43F5",
    "43F6",
    "44F1",
    "44F2",
    "44F3",
    "44F4",
    "44F5",
    "44F6",
    "44F7",
    "44F8",
    "45F1",
    "45F2",
    "45F3",
    "45F4",
    "45F5",
    "45F6",
    "45F7",
    "45F8",
    "45F9",
    "46F1",
    "46F2",
    "46F3",
    "46F4",
    "46F5",
    "46F9",
    "46G0" ,
    "47G0"
  )
)

san_4 <- data.frame(
  sppFAO = "SAN",
  stock = "san.sa.4",
  rect = c(
    "38E8",
    "38E9",
    "39E8",
    "39E9",
    "40E7",
    "40E8",
    "40E9",
    "41E6",
    "41E7",
    "41E8",
    "41E9",
    "41F0",
    "42E7",
    "42E8",
    "42E9",
    "42F0",
    "43E7",
    "43E8",
    "43E9",
    "43F0",
    "44E5",
    "44E6",
    "44E7",
    "44E8",
    "44E9",
    "44F0",
    "45E6",
    "45E7",
    "45E8",
    "45E9",
    "45F0",
    "46E6",
    "46E7",
    "46E8",
    "46E9",
    "46F0"
  )
)

san_5 <- data.frame(
  sppFAO = "SAN",
  stock = "san.sa.5r",
  rect =  c(
    "47F1",
    "47F2",
    "47F3",
    "47F4",
    "47F5",
    "47F6",
    "48F1",
    "48F2",
    "48F3",
    "48F4",
    "48F5",
    "48F6",
    "49F1",
    "49F2",
    "49F3",
    "49F4",
    "49F5",
    "49F6",
    "50F1",
    "50F2",
    "50F3",
    "50F4",
    "50F5",
    "51F1",
    "51F2",
    "51F3",
    "51F4",
    "51F5",
    "52F1",
    "52F2",
    "52F3",
    "52F4",
    "52F5"
  )
)

san_6 <- data.frame(
  sppFAO = "SAN",
  stock = "san.sa.6",
  rect = c(
    "41G0",
    "41G1",
    "41G2",
    "42G0",
    "42G1",
    "42G2",
    "43G0",
    "43G1",
    "43G2",
    "44G1"
  )
)

san_7 <- data.frame(
  sppFAO = "SAN",
  stock = "san.sa.7r",
  rect = c(
    "47E6",
    "47E7",
    "47E8",
    "47E9",
    "47F0",
    "48E6",
    "48E7",
    "48E8",
    "48E9",
    "48F0",
    "49E6",
    "49E7",
    "49E8",
    "49E9",
    "49F0",
    "50E6",
    "50E7",
    "50E8",
    "50E9",
    "50F0",
    "51E6",
    "51E7",
    "51E8",
    "51E9",
    "51F0",
    "52E6",
    "52E7",
    "52E8",
    "52E9",
    "52F0"
  )
)

san <- rbind(san_1, san_2, san_3, san_4, san_5, san_6, san_7)

## Add rect to stock_ref ----

stock_ref <- merge(stock_ref, san, all.x = T)

# Add quarter when needed ----

## Horse mackerel ----

hom <- subset(stock_ref, sppFAO == "HOM")

hom$quarter <- NA

hom_w <- data.frame(stock = "hom.27.2a4a5b6a7a-ce-k8", quarter = c(3, 3, 3, 3, 4, 4, 4, 4),  area = c("27.4.a", "27.3.a", "27.3.a.20", "27.3.a.21"))
          

hom_ns <- data.frame(stock = "hom.27.3a4bc7d", quarter = c(1, 1, 1, 1, 2, 2, 2, 2), area = c("27.4.a", "27.3.a", "27.3.a.20", "27.3.a.21"))

hom <- rbind(hom_w, hom_ns)

## Add quarter to stock_ref ----

stock_ref <- merge(stock_ref, hom, all.x = T)

## clean up relation

stock_ref <- rename(stock_ref, "speciesCode" = "sppFAO", "statisticalRectangle" = "rect")

stock_ref$updated <- Sys.Date()

write.csv(stock_ref, paste0("./Lookup and relational tables/R_StockSpeciesSpaceTime/stock_relation_draft_", Sys.Date(), ".csv"), row.names = F)
