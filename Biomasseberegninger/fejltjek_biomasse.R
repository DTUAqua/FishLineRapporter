library(pacman)
p_load(RODBC)


channel <- odbcConnect("FishLineDW")

bvc <- sqlQuery(channel,
                "select year,cruise from cruise where cruise like 'bv%' and year <= 2019",
                stringsAsFactors=F)




bv <- sqlQuery(channel,
               paste0("select * from errorLog
               where origin like ",
                      paste0("'",bvc$year,"->",bvc$cruise,"%'",collapse=" or origin like ")),
               stringsAsFactors=F)

close(channel)