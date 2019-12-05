

#Functions

##     Cleaning REKREA data 
##     get_rekrea_fucntions.R

###############################################################################

##     Defining functions

###############################################################################


## 1) Simple functions

round2 = function(x) trunc(x+0.5);
sumNA = function(x) sum(x,na.rm=T);

#------------------------------------------------------------------------------;

## 2) Simple functions

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 2)
  h1 <-hcl(h = hues, l = 65, c = 100)[1:n]
  #if("#C49A00" %in% h1) {h2 <- h1[h1 != "#C49A00"]}
  #else 
  {h1}
}

# overwrite missing values in x with values from y ----
coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
    joined <- join(x, y, by = by, suffix = suffix, ...)
    # names of desired output
    cols <- union(names(x), names(y))
    
    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    # remove suffixes and deduplicate
    to_coalesce <- unique(substr(
        to_coalesce, 
        1, 
        nchar(to_coalesce) - nchar(suffix_used)
    ))
    
    coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
        joined[[paste0(.x, suffix[1])]], 
        joined[[paste0(.x, suffix[2])]]
    ))
    names(coalesced) <- to_coalesce
    
    dplyr::bind_cols(joined, coalesced)[cols]
}

#------------------------------------------------------------------------------;

## 3) extract legend

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


#------------------------------------------------------------------------------;

## 4) Blank theme for plotting

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

#------------------------------------------------------------------------------;


## 5) Rename columns 1


rename1 <- function(dat, x, y, N){
  colnames(dat)[grepl(x, names(dat) ) ] <- paste0(y,str_sub(colnames(
    dat[,grepl(x, names(dat))]),N))
  return(dat)
}

#------------------------------------------------------------------------------;

## 6) Rename columns 2


renameOLS1 <- function(x, y){
  colnames(OLS)[grepl(x, names(OLS)) ] <- paste0(y, stri_extract_first_regex(
    colnames(OLS[ ,grepl(x, names(OLS)) ]), "[0-9]+"),
    str_sub(colnames(OLS[ ,grepl(x, names(OLS)) ]),-7))
  return(OLS)
}

#------------------------------------------------------------------------------;

## 6) Rename offline


rename_offline <- function(OFS){
  
  OFS <- rename1(OFS, "Antal.dage", "days", 11)
  OFS <- rename1(OFS, "Nægter", "refuse", 16)
  OFS <- rename1(OFS, "Ved.ikke", "unknown", 9)
  
  OFS <- rename1(OFS, "Artskode", "species", 9)
  OFS <- rename1(OFS, "Antal", "number", 6)
  OFS <- rename1(OFS, "Længde", "length", 12)
  OFS <- rename1(OFS, "Vægt", "weight", 10)
  OFS <- rename1(OFS, "Kode", "code", 5)
  OFS <- rename1(OFS, "Øresten", "otolith", 17)
  
  return(OFS)
  
}

#------------------------------------------------------------------------------;

## 6) Rename online


rename_online <- function(OLS){
  
  OLS <- rename1(OLS, "Antal.dage","days",11)
  OLS <- rename1(OLS, "Nægter","refuse",16)
  OLS <- rename1(OLS, "Ved.ikke","unknown",9)
  
  OLS <- renameOLS1("Artskode.spm", "species.")
  OLS <- renameOLS1("Antal.spm", "number.")
  OLS <- renameOLS1("Længde", "length.")
  OLS <- renameOLS1("Vægt", "weight.")
  OLS <- renameOLS1("Kode.spm", "code.")
  OLS <- renameOLS1("Øresten", "otolith.")
  
  return(OLS)
  
}


#------------------------------------------------------------------------------;

## 7) Updating data to aacount for the different data versions

create_survey_data <- function(SUR,TRIP){

  
  #small of corrections
  SUR[SUR=="" | SUR==" " ]<-NA
  # TRIP$c.spm1_1 <- paste(TRIP$a.spm1_1, TRIP$a.spm1_2, TRIP$spm1_3, sep="-")

  SUR1 <- SUR
  
  #Combine trip information with respondenet data
  nm <- c("a.spm1_1", "a.spm1_2", "b.spm1_2", "spm1_3" , "a.spm1_4", 
          "b.spm1_4","c.spm1_4" ,"a.spm1_5", "b.spm1_5" ,"c.spm1_5","d.spm1_5")
  nm2 <- c("a.spm3_2","b.spm3_2", "a.spm3_3", "b.spm3_3", "Timer.NOE.spm3_4",
           "Minutter.NOE.spm3_4", "Timer.MOE.spm3_4", "Minutter.MOE.spm3_4",
           "Timer.SOE.spm3_4","Minutter.SOE.spm3_4")

  # SUR1[!is.na(SUR1$c.spm1_1),nm] <- lapply(nm, function(x)
  #     TRIP[[x]][match(SUR1[!is.na(SUR1$c.spm1_1),]$c.spm1_1, TRIP$c.spm1_1)])
  # 
  # SUR1[!is.na(SUR1$f.spm1_1),c(nm)] <- lapply(c(nm), function(x)
  #     TRIP[[x]][match(SUR1[!is.na(SUR1$f.spm1_1),]$f.spm1_1, TRIP$f.spm1_1)])
  # 
  # SUR1[is.na(SUR1$a.spm3_2),nm2] <- lapply(nm2, function(x)
  #     TRIP[[x]][match(SUR1[is.na(SUR1$a.spm3_2),]$c.spm1_1, TRIP$c.spm1_1)])
  # SUR1[is.na(SUR1$a.spm3_2),nm2] <- lapply(nm2, function(x)
  #     TRIP[[x]][match(SUR1[is.na(SUR1$a.spm3_2),]$f.spm1_1, TRIP$f.spm1_1)])

  coalesce2<-function(data,support){
      if (class(data)=="logical"){return(coalesce(rep(NA,nrow(data)),support))}
      else if (class(support)=="logical"){return(data)} 
      else{return(coalesce(data,support))}
  }
  
  SUR15 <- left_join(SUR1 %>% select(-nm),
                     TRIP %>% 
                       select(c("f.spm1_1":"tripSubtrip")) %>% 
                       select(-URL.Redirect),
                     by=c("tripSubtrip","f.spm1_1","c.spm1_1")
                     ) #%>%
  
  for (col in nm2){
    print(col)
      SUR15[[col]]<-coalesce2(SUR15[[paste0(col,".x")]],
                              SUR15[[paste0(col,".y")]])
  }
    
  
# SUR2.0 <- merge(SUR1, TRIP[TRIP$d.spm1_5!0 & !is.na(TRIP$d.spm1_5),c("c.spm1_1","spm1_6")],all=TRUE)
  
  
  SUR2 <- merge(SUR15,
                select(TRIP[TRIP$d.spm1_5==0 & !is.na(TRIP$d.spm1_5),],-spm1_6),
                all=TRUE)
  
  #Adding trip ID to trips made after trip numbers where added
  # SUR2[!is.na(SUR2$f.spm1_1) | is.na(SUR2$c.spm1_1) ,"c.spm1_1"] <- 
  #   paste(SUR2[!is.na(SUR2$f.spm1_1) | is.na(SUR2$c.spm1_1),"a.spm1_1"],
  #         SUR2[!is.na(SUR2$f.spm1_1) | is.na(SUR2$c.spm1_1),"a.spm1_2"],
  #         SUR2[!is.na(SUR2$f.spm1_1) | is.na(SUR2$c.spm1_1),"spm1_3"],sep="-")
  SUR2[!is.na(SUR2$f.spm1_1) | is.na(SUR2$c.spm1_1) ,"c.spm1_1"] <- 
      paste(SUR2[!is.na(SUR2$f.spm1_1) | is.na(SUR2$c.spm1_1),"f.spm1_1"],
            SUR2[!is.na(SUR2$f.spm1_1) | is.na(SUR2$c.spm1_1),"a.spm1_1"],
            sep="_")
 
  # # create dateGearStart and dateGearEnd
  SUR2$dateGearStart <- as.numeric(
      strptime(paste(as.Date(SUR2$spm1_3,"%d/%m/%Y"),
                     paste(as.character(
                         ifelse(is.na(SUR2$a.spm3_2),"08",SUR2$a.spm3_2)),
                         as.character(
                             ifelse(is.na(SUR2$b.spm3_2),"00",SUR2$b.spm3_2)),
                         sep=":"),
                     sep=" "),
               "%Y-%m-%d %H:%M"))

  SUR2$dateGearEnd <- 
      as.numeric(ifelse(is.na(SUR2$a.spm3_3),
             as.POSIXct(strptime(SUR2$dateGearStart,"%Y-%m-%d %H:%M")),
             as.POSIXct(strptime(paste(as.Date(SUR2$spm1_3,"%d/%m/%Y"),
                                       paste(as.character(SUR2$a.spm3_3),
                                             as.character(ifelse(is.na(SUR2$b.spm3_3),
                                                                 "00",
                                                                 SUR2$b.spm3_3)),
                                                      sep=":"),
                     sep=" "),
               "%Y-%m-%d %H:%M"))))#,origin="1970-01-01")
      
  SUR2$year <- format(strptime(SUR2$dateGearStart,"%Y-%m-%d %H:%M"),"%Y")
  SUR2$month <- as.numeric(format(strptime(SUR2$dateGearStart,"%Y-%m-%d %H:%M"),"%m"))
  SUR2$quarter <- ceiling(SUR2$month/3)
  #Create intercept ID from intercept number and trip ID
  SUR2$intID <- paste(SUR2$b.spm1_1,SUR2$c.spm1_1,sep="-")

  SUR_out <- merge(SUR2,TRIP[,c("c.spm1_1","spm1_6")],all=TRUE)
  
 TRIP$sgTRid <- TRIP$Response.ID
 SUR_out2 <- merge(SUR_out,TRIP[,c("c.spm1_1","sgTRid")],all=TRUE)
  
 # SUR_out2$c.spm1_1 <- SUR_out2$tripSubtrip
  
  #For use in report
  return(SUR_out2)

}

#------------------------------------------------------------------------------;

## 8) Trip data

get_trip_data <- function(SUR1,TRIP){
  
  #Summarize respondentes with positive participant status per trip
  
  trip_vars <- c("sgTRid","f.spm1_1","a.spm1_1", "a.spm1_2", "b.spm1_2", "spm1_3", 
                 "a.spm1_4", "b.spm1_4", "c.spm1_4", "a.spm1_5", "b.spm1_5",
                 "c.spm1_5", "c.spm1_1","spm1_6")
  
  # iTurY <- ddply(SUR1[SUR1$spm2_1 == "Ja",], 
  #                trip_vars, summarize, respYes = sum(!is.na(spm1_3)))
  iTurY <- SUR1 %>% 
      dplyr::filter(spm2_1 == "Ja" & !is.na(SUR1$spm2_1)) %>% 
      dplyr::group_by(sgTRid,f.spm1_1,a.spm1_1, a.spm1_2, b.spm1_2, spm1_3, 
                      a.spm1_4, b.spm1_4, c.spm1_4, a.spm1_5, b.spm1_5,
                      c.spm1_5, c.spm1_1,spm1_6) %>% 
      dplyr::summarise(respYes = sum(!is.na(spm1_3)))
  
  #Summarize respondents with negative participant status per trip
  # iTurN <- ddply(SUR1[SUR1$spm2_1 == "Nej" & !is.na(SUR1$spm2_1),], 
  #                trip_vars, summarize, respNo = sum(!is.na(spm1_3)))
  iTurN <- SUR1 %>% 
      dplyr::filter(spm2_1 == "Nej" & !is.na(SUR1$spm2_1)) %>% 
      dplyr::group_by(sgTRid,f.spm1_1,a.spm1_1, a.spm1_2, b.spm1_2, spm1_3, 
                      a.spm1_4, b.spm1_4, c.spm1_4, a.spm1_5, b.spm1_5,
                      c.spm1_5, c.spm1_1,spm1_6) %>% 
      dplyr::summarise(respNo = sum(!is.na(spm1_3)))
  
  #Merge trip data
  iTur0 <- merge(iTurY,iTurN,all=TRUE)
  iTur0$respYes <- ifelse(is.na(iTur0$respYes),0,iTur0$respYes)
  iTur0$respNo <- ifelse(is.na(iTur0$respNo),0,iTur0$respNo)
  
  # noResp <- SUR1[SUR1$d.spm1_5==0 & !is.na(SUR1$d.spm1_5),trip_vars]
  noResp <- TRIP %>% 
      dplyr::filter(!(Response.ID %in% iTur0$sgTRid)) %>% 
      dplyr::rename(sgTRid = Response.ID) %>% 
      dplyr::select(sgTRid,f.spm1_1,a.spm1_1, a.spm1_2, b.spm1_2, spm1_3, 
             a.spm1_4, b.spm1_4, c.spm1_4, a.spm1_5, b.spm1_5,
             c.spm1_5, c.spm1_1,spm1_6)
  noResp$respYes <- 0; noResp$respNo <- 0
  
  iTur <- rbind(as.data.frame(iTur0),
                as.data.frame(noResp))
  
  iTur$respTot <- rowSums(iTur[,c("respYes","respNo")],na.rm=T)
  
  #Add month, quarter and year
  iTur$month <- as.numeric(str_sub(iTur$spm1_3,4,5))
  iTur$quarter <- ceiling(as.numeric(iTur$month)/3)
  iTur$year <- as.numeric(str_sub(iTur$spm1_3,7,10))
  
  #If other picked as interviewer
  iTur[iTur$a.spm1_2=="Anden" & !is.na(iTur$b.spm1_2),"a.spm1_2"] <- 
    iTur[iTur$a.spm1_2=="Anden" & !is.na(iTur$b.spm1_2),"b.spm1_2"]
  iTur$b.spm1_2 <- NULL #And drop col b.spm1_2

  iTur$caseStudy <- "Torsk"
  
  iTur_out <- unique(iTur[,c("sgTRid","caseStudy","c.spm1_1","f.spm1_1","spm1_3",
                      "month","quarter","year","a.spm1_5","b.spm1_5","a.spm1_1","spm1_6",
                      "a.spm1_2","b.spm1_4","a.spm1_4","c.spm1_4","c.spm1_5",
                      "respYes","respNo","respTot")])
  
  return(iTur_out)
  
}


#------------------------------------------------------------------------------;

## 9) Respondent data
get_resp_data <- function(SUR1){
  
  # respW <- select(SUR1,-c(species.1.spm6_1:code.15.spm7_2))
  respW <- select(SUR1, 
                  -c(grep(pattern = "^species|^number|^length|^weight|^otolith|^code",
                       colnames(SUR1))))

  
  respNew <- respW[c("Response.ID", "c.spm1_1","f.spm1_1", "spm1_3", "month", "quarter",
                     "year",  "a.spm1_5","spm1_6", "a.spm1_2", "b.spm1_5",
                     "b.spm1_1")]
  colnames(respNew) <- c("sgId", "tripId","tripNum", "date", "month", "quarter", "year",
                          "tripType","weekdayWeekend", "interviewPerson", "place",
                         "respNum")
  
  #Status and time for ending interview
  
  respNew$statusInterview <- respW$spm10_3
  respNew[respNew$statusInterview %in% c("answered","Spørgskema udfyldt") & 
            !is.na(respNew$statusInterview),"statusInterview"] <- "A"
  respW$a.spm10_2[is.na(respW$a.spm10_2)] <- "00"
  respW$b.spm10_2[is.na(respW$b.spm10_2)] <- "00"
  respNew$timeInterviewEnd <- paste(str_pad(respW$a.spm10_2, 2, pad = "0"),
                                    str_pad(respW$b.spm10_2, 2, pad = "0"),
                                    sep=":")
  
  respNew$participateSurvey <- respW$spm2_1
  
  #Participation before
  
  respNew$participatedBefore <- respW$a.spm3_1
  respNew[is.na(respNew$participatedBefore),]$participatedBefore <- "Nej"
  respNew$participatedWhen <- respW$b.spm3_1
  
  # Trip time and place
  
  respW[is.na(respW$a.spm3_2),]$a.spm3_2 <- 0
  respW[is.na(respW$b.spm3_2),]$b.spm3_2 <- 0
  respW[is.na(respW$a.spm3_3),]$a.spm3_3 <- 0
  respW[is.na(respW$b.spm3_3),]$b.spm3_3 <- 0
  respNew$tripStart <- paste(sprintf("%02d", respW$a.spm3_2),
                             sprintf("%02d", respW$b.spm3_2),sep=":")
  respNew$tripEnd <- paste(sprintf("%02d", respW$a.spm3_3),
                           sprintf("%02d", respW$b.spm3_3),sep=":")
  
  respNew$noeHours <- respW$Timer.NOE.spm3_4+(respW$Minutter.NOE.spm3_4/60)
  respNew$coeHours <- respW$Timer.MOE.spm3_4+(respW$Minutter.MOE.spm3_4/60)
  respNew$soeHours <- respW$Timer.SOE.spm3_4+(respW$Minutter.SOE.spm3_4/60)
  
  
  #License 
  
  respNew$license <- respW$spm3_5
  respNew$licenseType <- respW$spm3_5.2
  respNew$noLicense <- respW$spm3_5.3
  respNew[respNew$license=="Nej" & !is.na(respNew$license),]$licenseType <- NA
  
  #Target species
  
  respNew$target1 <- gsub(" ", "", toupper(respW$a.b.spm3_6), fixed = TRUE)
  respNew$target2 <- gsub(" ", "", toupper(respW$b.b.spm3_6), fixed = TRUE)
  respNew$target3 <- gsub(" ", "", toupper(respW$c.b.spm3_6), fixed = TRUE)
  
  
  tar_col <- c("target1","target2","target3")
  
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("TORSK"), "TOR") )
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("HORNFISK","HORN"), "HOF") )
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("SKRU","SKRUBBE"), "SKR") )
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("SEJ"), "MSJ") )
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("ØRRED","HAVØRRED"), "ORD") )
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("ORD"), "ORD") )
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("SILD"), "SIL") )
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("RØDSPÆTTE","RØDS"), "RSP") )
  respNew[tar_col] <- lapply(respNew[tar_col], function(x) replace(x,x %in% c("MAKREL"), "MAK") )

  
  # CatchReg
  
  respNew$catchRegistartion <- respW$spm5_1
  
  
  # Effort/Fisketid
  
  respNew$tripsCharter12 <- as.character(apply(respW[,c("unknown.a.spm4_1","refuse.a.spm4_1","days.a.spm4_1.1")],1,function(x) x[!is.na(x)]))
  respNew$tripsCharter3 <- as.character(apply(respW[,c("unknown.b.spm4_1","refuse.b.spm4_1","days.b.spm4_1.1")],1,function(x) x[!is.na(x)]))
  respNew$tripsPrivate12 <- as.character(apply(respW[,c("unknown.a.spm4_2","refuse.a.spm4_2","days.a.spm4_2.1")],1,function(x) x[!is.na(x)]))
  respNew$tripsPrivate3 <- as.character(apply(respW[,c("unknown.b.spm4_2","refuse.b.spm4_2","days.b.spm4_2.1")],1,function(x) x[!is.na(x)]))
  respNew$tripsPier12 <- as.character(apply(respW[,c("unknown.a.spm4_3","refuse.a.spm4_3","days.a.spm4_3.1")],1,function(x) x[!is.na(x)]))
  respNew$tripsPier3 <- as.character(apply(respW[,c("unknown.b.spm4_3","refuse.b.spm4_3","days.b.spm4_3.1")],1,function(x) x[!is.na(x)]))
  
  
 # respNew[respNew$sgId=="OF63",]$tripsCharter3 <- 0
  
  #For harbour/pier sampling (are only asked about effort for the past 1 month in private boat)
  respNew$tripsPrivate1H <- as.character(apply(respW[,c("unknown.c.spm4_2","refuse.c.spm4_2","days.c.spm4_2.1")],1,function(x) x[!is.na(x)]))
  respNew[respNew$tripsPrivate1H=="integer(0)",]$tripsPrivate1H <- NA
  
  respNew$tripsFreshSalt12 <- respW$a.spm4_4
  respNew$tripsFreshSalt3 <- respW$b.spm4_4
  
  respNew$tripsLakeRiverPT12 <- respW$a.spm4_5
  respNew$tripsLakeRiverPT3 <- respW$b.spm4_5
  
  respNew[respNew=="character(0)"] <- NA

  
  # Address
  
  
  #If either postal code, street, cirty or country is informed
  respW[(!is.na(respW$b.spm8_1) | !is.na(respW$a.spm8_1) | !is.na(respW$c.spm8_1) | !is.na(respW$d.spm8_1)),"addressStatus"] <- "OK"
  
  respW[is.na(respW$addressStatus) & !is.na(respW$unknown.e.spm8_1),"addressStatus"] <- respW[is.na(respW$addressStatus) & !is.na(respW$unknown.e.spm8_1),]$unknown.e.spm8_1 
  respW[is.na(respW$addressStatus) & !is.na(respW$refuse.e.spm8_1),"addressStatus"] <- respW[is.na(respW$addressStatus) & !is.na(respW$refuse.e.spm8_1),]$refuse.e.spm8_1 
  
  respNew$addressStatus <- respW$addressStatus
  
  respNew$addressPostalCode <- respW$a.spm8_1
  respNew$addressCity <- respW$b.spm8_1
  respNew$addressStreet <- respW$c.spm8_1
  respNew$addressCountry <- respW$d.spm8_1
  
  
  # Kontact info
  
  #If email or phone informed
  respW[!is.na(respW$b.spm8_2) | !is.na(respW$e.spm8_2) ,"contactStatus"] <- "OK"
  respW[is.na(respW$contactStatus) & !is.na(respW$d.spm8_2),"contactStatus"] <- respW[is.na(respW$contactStatus) & !is.na(respW$d.spm8_2),]$d.spm8_2 
  
  respNew$contactStatus <- respW$contactStatus
  
  respNew$contactName <- respW$a.spm8_2
  respNew$contactPhone <- respW$b.spm8_2
  respNew$contactEmail <- respW$e.spm8_2
  respNew$contactTime <- respW$c.spm8_2
  respNew$contactAge <- respW$spm8_3
  respNew$contactSex <- respW$spm8_4
  
  
  # Choise Experiment
  
  
  #subset data w. participate status O (=OK)
  respNew$ceParticipate <- respW$spm10_1
  respNew[respNew$ceParticipate=="Udfyldes online" & !is.na(respNew$ceParticipate),"ceParticipate"] <- "O"
  respNew[respNew$ceParticipate=="Ønsker ikke at deltage" & !is.na(respNew$ceParticipate),"ceParticipate"] <- "N"
  
  respNew$ceEmail <- respW$spm10_1.2
  respNew[is.na(respNew$ceEmail) & !is.na(respNew$contactEmail),]$ceEmail <- 
    respNew[is.na(respNew$ceEmail) & !is.na(respNew$contactEmail),]$contactEmail
  
  #If CE mail empty or "Samme" then overwrite w. first email adress
 # CE_data2[is.na(CE_data2$ceEmail) | CE_data2$ceEmail == "Samme" | CE_data2$ceEmail == "","ceEmail"] <- 
#    CE_data2[is.na(CE_data2$ceEmail)  | CE_data2$ceEmail == "Samme" | CE_data2$ceEmail == "","anglerEmail"]
  
  #Motivation and expectaitons
  
  respNew$expertise <- respW$spm8_5
  respNew$motivations <- respW$spm8_6
  respNew$rateTrip <- respW$spm8_7
  respNew$fiskepleje <- respW$spm9_1
  respNew$fangstjournalen <- respW$spm9_2
  respNew$regFangstjournalen <- respW$spm9_2.2
  respNew$hobby <- respW$a.spm9_3
  respNew$catchExpectations <- respW$b.spm9_3
  respNew$amountSatisfaction <- respW$c.spm9_3
  respNew$unitAmountSatisfaction <- respW$d.spm9_3
  respNew$sizeSatisfaction <- respW$e.spm9_3
  respNew$unitSizeSatisfaction <- respW$f.spm9_3
  
  
  #Transportation
  
  respNew$transportMethod <- respW$a.spm9_4
  respNew$startHomeAdress <- respW$b.spm9_4
  respNew$transportLength <- respW$c.spm9_4
  respNew$numberInCar <- respW$d.spm9_4
  respNew$otherPurposes <- respW$e.spm9_4
  
  #
  respNew$transportHours <- NA #round(respW$Timer.e.spm9_4+(respW$Minutter.e.spm9_4/60),1)
  
  
  #Expenses
  
  respNew$expensePublicTransport <- respW[,grep("publicTransport", names(respW))]
  respNew$expensePermits <- respW[,grep("permitsShipHarbour", names(respW))]
  respNew$expenseSmallGear <- respW[,grep("smallGear", names(respW))]
  respNew$expenseLargeGear <- respW[,grep("largeGear", names(respW))]
  respNew$expenseFood <- respW[,grep("food", colnames(respW))]
  respNew$expenseOtherEquipment <- respW[,grep(".otherEquipment.", names(respW))]
  # respNew$expenseAccommodation <- respW[,grep(".accommodation.", names(respW))]
  respNew$expenseAccommodation <- respW[,grep(".Overnatning.", names(respW))]
  respNew$expenseOther <- respW[,grep(".other.spm", names(respW))]
  

 # respNew[respNew %in% c("vedikke","unknown")] <- "Unknown"
#  respNew[respNew %in% c("Nægter at svare","refuse","naegter")] <- "Refuse"
  
  
  respNewOut <- respNew[substr(respNew$sgId,1,2) != "TR",]
  
  return(respNewOut)
}


#------------------------------------------------------------------------------;

## 10) Catch data

get_catch_data <- function(SUR1){
    
    
    #Getting ang cleaning all catch data from survey output
    catch <- select(SUR1,
                    c(Response.ID,intID,a.spm1_4,dateGearStart,dateGearEnd,
                      species.1.spm6_1:code.15.spm7_2))
    catch[,"spm6_2"] <- NULL; 
    catch[,"spm6_4"] <- NULL; 
    catch[,"spm7_1"] <- NULL
    
    ######## Available catch ########
    catch2 <- select(catch,
                     c(Response.ID,intID,a.spm1_4,dateGearStart,dateGearEnd,
                       species.1.spm6_1:otolith.45.spm6_5,
                       code.1.spm6_1:code.45.spm6_5))
    
    colnames(catch2) <- c("sgId","intID","dfuArea","dateGearStart","dateGearEnd",
                          paste(str_sub(colnames(catch2)[-c(1:5)],1,-8),
                                str_sub(colnames(catch2)[-c(1:5)],-6),sep=":"))
    
    catch3<-reshape(catch2, varying=c(6:(ncol(catch2))), direction="long", 
                    idvar=paste0("sgId","intID","dfuArea",
                                 "dateGearStart","dateGearEnd"), 
                    sep=".")
    
    #Small corrections to data
    catch3[catch3==''|catch3==' ' | catch3=='-'] <- NA
    catch3[!is.na(catch3$weight),"weight"] <- 
      as.numeric(gsub(",",".",catch3[!is.na(catch3$weight),]$weight))
    
    
    catch4 <- catch3[rowSums(is.na(catch3[,c("species", "number", "length",
                                             "weight", "code", "otolith")])) != 6, ]
    catch4$catchCategory <- "C"
    
    catch5 <- catch4[!is.na(catch4$species),]
    
    ######## Unavailable catch ########
    nCatch <- select(catch,
                     c(Response.ID,intID,a.spm1_4,dateGearStart,dateGearEnd,
                       species.1.spm7_2:number.15.spm7_2,
                       code.1.spm7_2:code.15.spm7_2))
    colnames(nCatch) <- c("sgId","intID","dfuArea","dateGearStart","dateGearEnd",
                          paste(str_sub(colnames(nCatch)[-c(1:5)],1,-8),
                                str_sub(colnames(nCatch)[-c(1:5)],-6),sep=":"))
    
    nCatch2 <-reshape(nCatch, varying=c(6:(ncol(nCatch))), direction="long", 
                      idvar=paste0("sgId","intID","dfuArea",
                                   "dateGearStart","dateGearEnd"), 
                      sep=".")
    nCatch2[nCatch2==''|nCatch2==' ']<-NA
    
    nCatch3 <- nCatch2[rowSums(is.na(nCatch2[,
                                             c("species","number","code")])) != 3, ]
    nCatch3$catchCategory <- "R"
    nCatch3$otolith <- "N"
    
    nCatch4 <- nCatch3[!is.na(nCatch3$species),]
    
    ######## Merge all catch data  ###########
    
    catchA <- merge(catch5,nCatch4,all=TRUE)
    catchA$species <- toupper(catchA$species)
    catchA[tolower(catchA$otolith)=="ja" & 
             !is.na(catchA$otolith),"otolith"] <- "Y"
    catchA[tolower(catchA$otolith)=="nej" & 
             !is.na(catchA$otolith),"otolith"] <- "N"
    catchA[is.na(catchA$otolith) & 
             catchA$species!="TOR" & 
             !is.na(catchA$species),"otolith"] <- "N"
    catchA$catchNum <- sub(':.*$','', catchA$time)
    catchA$code <- as.numeric(gsub("([0-9]+).*$", "\\1", catchA$code))
    
    catchA$tripId <-  sub(".+?-", "", catchA$intID)
    catchA$respNum <- sub("-.*", "", catchA$intID)
    
    catchB <- catchA[,c("sgId","tripId","dfuArea","respNum", "catchNum", "species",
                        "catchCategory", "number", "length", "weight",
                        "code", "otolith","dateGearStart","dateGearEnd")]
    
    catchB[catchB$species=="HORN"  & !is.na(catchB$species),"species"] <- "HOF"
    catchB[catchB$species %in% c("MMSJ","SEJ")  & !
             is.na(catchB$species),"species"] <- "MSJ"
    catchB[catchB$species=="SILD"  & !is.na(catchB$species),"species"] <- "SIL"
    catchB$species <- gsub(" ", "", catchB$species)
    
    #For use in report
    catchW <- catchB
    
  return(catchW)  
  
}


#------------------------------------------------------------------------------;

## 10) Catch data

clean_seatr_data <- function(dat_in){
  
  sea_1 <- dat_in[order(as.integer(dat_in$spm1_3)),] 
  sea_1$respNum <- 1:nrow(sea_1)
  sea_1 <- ddply(sea_1,c("spm1_3","c.spm1_4"),transform,resp_nr_trip=seq_along(spm1_3))
  sea_1$trip_no <- id(sea_1[c("spm1_3", "c.spm1_4")], drop = TRUE)
    
  return(sea_1)

}

## 11) Catch data seatrout

get_catch_data_seatr <- function(sea_1){
  
  
  #Getting ang cleaning all catch data from survey output
  catch <- select(sea_1,c(respNum,Response.ID,ID.1.spm6_1:age.45.spm6_5,species.1.spm7_2:reason.15.spm7_2))
  catch[,"spm6_2"] <- NULL; catch[,"spm6_4"] <- NULL; catch[,"spm7_1"] <- NULL
  
  ######## Available catch ########
  catch2 <- select(catch,c(respNum,Response.ID,ID.1.spm6_1:age.45.spm6_5))
  
  colnames(catch2)[-c(1,2)] <- paste(str_sub(colnames(catch2)[-c(1,2)],1,-8),
                                str_sub(colnames(catch2)[-c(1,2)],-6),sep=":")
  
  catch3<-reshape(catch2, varying=c(3:(ncol(catch2))), direction="long", 
                  idvar=paste0("respNum","Response.ID","time"), sep=".")
  
  #Small corrections to data
  catch3$weight <- as.numeric(gsub(",",".",catch3$weight))
  catch3[catch3==''|catch3==' ']<-NA
  
  catch4 <- catch3[rowSums(is.na(catch3[,c("ID","species", "number", "length",
                                           "weight", "code","sample","age")])) != 8, ]
  catch4$catchCategory <- "C"
  
  ######## Unavailable catch ########
  nCatch <- select(catch,c(respNum,Response.ID,species.1.spm7_2:reason.15.spm7_2))
  colnames(nCatch)[-c(1,2)] <- paste(str_sub(colnames(nCatch)[-c(1,2)],1,-8),
                                str_sub(colnames(nCatch)[-c(1,2)],-6),sep=":")
  
  nCatch2 <-reshape(nCatch, varying=c(3:(ncol(nCatch))), direction="long", 
                    idvar=paste0("respNum","Response.ID","time"), sep=".")
  nCatch2[nCatch2==''|nCatch2==' ']<-NA
  
  nCatch3 <- nCatch2[rowSums(is.na(nCatch2[,
                                           c("species","number","code","reason")])) != 4, ]
  nCatch3$catchCategory <- "R"
  
  
  ######## Merge all catch data  ###########
  
  catchA <- merge(catch4,nCatch3,all=TRUE)
 # catchA$species <- toupper(catchA$species)
 # catchA[tolower(catchA$otolith)=="ja" & 
#           !is.na(catchA$otolith),"otolith"] <- "Y"
 # catchA[tolower(catchA$otolith)=="nej" & 
#           !is.na(catchA$otolith),"otolith"] <- "N"
 # catchA$catchNum <- sub(':.*$','', catchA$time)
  #catchA$code <- as.numeric(gsub("([0-9]+).*$", "\\1", catchA$code))
  
#  catchA$tripId <-  sub(".+?-", "", catchA$intID) 
#  catchA$respNum <- sub("-.*", "", catchA$intID)
  
  catchA$sgId <- catchA$Response.ID
  
  catchB <- catchA[,c("respNum","sgId","ID","species","catchCategory", "number", "length", "weight",
                      "sample","age","code","reason")]
  
  
  
  return(catchB)
  
}


#------------------------------------------------------------------------------;

## 12) Respondent data

get_resp_data_seat <- function(dat_in){
  
  resp_rek <- select(dat_in,-c(X1.b,X2.b,X3.b,X4.b,X5.b,
                               days.a.spm4_2:b.spm4_6,
                               ID.1.spm6_1:age.45.spm6_5,
                               species.1.spm7_2:reason.15.spm7_2))
  resp_rek[resp_rek =='' | resp_rek == ' '] <- NA
  
  
  respNew <- data.frame(tripDate=resp_rek$spm1_3,placeCode=resp_rek$c.spm1_4, 
                        tripId=resp_rek$trip_no,respNum =resp_rek$respNum,
                        respNumTrip =resp_rek$resp_nr_trip,
                        sgId =resp_rek$Response.ID,
                        interviewerID =resp_rek$a.spm1_2)
  
  
  
  #Participation before
  
  respNew$StatusParticipate <- resp_rek$spm2_1
  respNew$participatedBefore <- resp_rek$a.spm3_1
  respNew$participatedWhen <- resp_rek$b.spm3_1
  #Status and time for ending interview
  
  respNew$statusInterview <- resp_rek$spm10_3
  resp_rek[is.na(resp_rek$a.spm10_2),"a.spm10_2"] <- "00"
  resp_rek[is.na(resp_rek$b.spm10_2),"b.spm10_2"] <- "00"
  respNew$timeInterviewEnd <- paste(str_pad(resp_rek$a.spm10_2, 2, pad = "0"),str_pad(resp_rek$b.spm10_2, 2, pad = "0"),sep=":")
  
  respNew$onlinePart <- resp_rek$spm10_1
  respNew$emailPart <- resp_rek$spm10_1.2
  
  respNew$orderInterview <- resp_rek$ha_spm0
  
  # Trip time and place
  resp_rek[is.na(resp_rek$a.ha_spm3_2) & is.na(resp_rek$b.ha_spm3_2),"fishedTest"] <- "NO"
  resp_rek[is.na(resp_rek$a.ha_spm3_3) & is.na(resp_rek$b.ha_spm3_3),"contFishingTest"] <- "NO"
  resp_rek[is.na(resp_rek$a.ha_spm3_2),"a.ha_spm3_2"] <- 0
  resp_rek[is.na(resp_rek$b.ha_spm3_2),"b.ha_spm3_2"] <- 0
  resp_rek[is.na(resp_rek$a.ha_spm3_3),"a.ha_spm3_3"] <- 0
  resp_rek[is.na(resp_rek$b.ha_spm3_3),"b.ha_spm3_3"] <- 0

  respNew$hoursFished <- round(resp_rek$a.ha_spm3_2+(resp_rek$b.ha_spm3_2/60),1)#paste(sprintf("%02s", resp_rek$a.ha_spm3_2),sprintf("%02s", resp_rek$b.ha_spm3_2),sep=":")
  respNew$hoursContFishing <- round(resp_rek$a.ha_spm3_3+(resp_rek$b.ha_spm3_3/60),1)
  
  respNew[resp_rek$fishedTest=="NO" & !is.na(resp_rek$fishedTest),"hoursFished"] <- NA
  respNew[resp_rek$contFishingTest=="NO" & !is.na(resp_rek$contFishingTest),"hoursContFishing"] <- NA
  
  # Fisher typo
  
  respNew$fisherType <- resp_rek$ha_a.spm1_1
  respNew[is.na(respNew$fisherType),]$fisherType <- resp_rek[is.na(respNew$fisherType),]$ha_a2.spm1_1  
  respNew$specialisingClothes <- resp_rek$ha_spm1  
  respNew$specialisingGear <- resp_rek$ha_spm2
  
  respNew$otherFishermen <- resp_rek$ha_spm3
  respNew$weatherWater <- resp_rek$ha_spm4
  respNew$weatherWindDir <- resp_rek$ha_spm5
  respNew$weatherWindForce <- resp_rek$ha_spm6
  
    
  #License 

  
  respNew$license <- resp_rek$spm3_5
  respNew$licenseType <- resp_rek$spm3_5.2
  respNew$noLicense <- resp_rek$spm3_5.3
  
  #Target species
  
  respNew$target1 <- resp_rek$a.b.spm3_6
  respNew$target2 <- resp_rek$b.b.spm3_6
  respNew$target3 <- resp_rek$c.b.spm3_6
  
  
  # CatchReg
  
  respNew$anyCatch <- resp_rek$spm5_0
  respNew$catchRegistartion <- resp_rek$spm5_1
  
  
  # Effort/Fisketid
  
  resp_rek[!is.na(resp_rek$days.a.ha_spm4_1.1) & !is.na(resp_rek$unknown.a.ha_spm4_1),"unknown.a.ha_spm4_1"] <- NA
  resp_rek[!is.na(resp_rek$days.b.ha_spm4_1.1) & !is.na(resp_rek$unknown.b.ha_spm4_1),"unknown.b.ha_spm4_1"] <- NA
  resp_rek[!is.na(resp_rek$days.a.spm4_4.1) & !is.na(resp_rek$unknown.a.spm4_4),"unknown.a.spm4_4"] <- NA
  resp_rek[!is.na(resp_rek$days.b.spm4_4.1) & !is.na(resp_rek$unknown.b.spm4_4),"unknown.b.spm4_4"] <- NA
  
  resp_rek[!is.na(resp_rek$days.a.ha_spm4_1.1) & !is.na(resp_rek$refuse.a.ha_spm4_1),"refuse.a.ha_spm4_1"] <- NA
  resp_rek[!is.na(resp_rek$days.b.ha_spm4_1.1) & !is.na(resp_rek$refuse.b.ha_spm4_1),"refuse.b.ha_spm4_1"] <- NA
  resp_rek[!is.na(resp_rek$days.a.spm4_4.1) & !is.na(resp_rek$refuse.a.spm4_4),"refuse.a.spm4_4"] <- NA
  resp_rek[!is.na(resp_rek$days.b.spm4_4.1) & !is.na(resp_rek$refuse.b.spm4_4),"refuse.b.spm4_4"] <- NA
  
  respNew$tripsPier12 <- as.character(apply(resp_rek[,c("unknown.a.ha_spm4_1","refuse.a.ha_spm4_1","days.a.ha_spm4_1.1")],1,function(x) x[!is.na(x)]))
  respNew$tripsPier3 <- as.character(apply(resp_rek[,c("unknown.b.ha_spm4_1","refuse.b.ha_spm4_1","days.b.ha_spm4_1.1")],1,function(x) x[!is.na(x)]))
  
  respNew$tripsAll12 <- as.character(apply(resp_rek[,c("unknown.a.spm4_4","refuse.a.spm4_4","days.a.spm4_4.1")],1,function(x) x[!is.na(x)]))
  respNew$tripsAll3 <- as.character(apply(resp_rek[,c("unknown.b.spm4_4","refuse.b.spm4_4","days.b.spm4_4.1")],1,function(x) x[!is.na(x)]))
  
  respNew[respNew=="character(0)"] <- NA
  
  # Address
  
  
  #If either postal code, street, city or country is informed
  resp_rek[(!is.na(resp_rek$b.spm8_1) | !is.na(resp_rek$a.spm8_1) | 
           !is.na(resp_rek$c.spm8_1) | !is.na(resp_rek$d.spm8_1)),"addressStatus"] <- "OK"
  
  resp_rek[is.na(resp_rek$addressStatus) & !is.na(resp_rek$unknown.e.spm8_1),
        "addressStatus"] <- resp_rek[is.na(resp_rek$addressStatus) & !is.na(resp_rek$unknown.e.spm8_1),]$unknown.e.spm8_1 
  resp_rek[is.na(resp_rek$addressStatus) & !is.na(resp_rek$refuse.e.spm8_1),
        "addressStatus"] <- resp_rek[is.na(resp_rek$addressStatus) & !is.na(resp_rek$refuse.e.spm8_1),]$refuse.e.spm8_1 
  
  respNew$addressStatus <- resp_rek$addressStatus
  
  respNew$addressPostalCode <- resp_rek$a.spm8_1
  respNew$addressCity <- resp_rek$b.spm8_1
  respNew$addressStreet <- resp_rek$c.spm8_1
  respNew$addressCountry <- resp_rek$d.spm8_1
  
  
  # Kontact info
  
  #If email or phone informed
  resp_rek[!is.na(resp_rek$e.spm8_2) ,"contactStatus"] <- "OK"
  resp_rek[is.na(resp_rek$contactStatus) & !is.na(resp_rek$d.spm8_2)
           ,"contactStatus"] <- resp_rek[is.na(resp_rek$contactStatus) & !is.na(resp_rek$d.spm8_2),]$d.spm8_2 
  
  respNew$contactStatus <- resp_rek$contactStatus
  
  respNew$contactName <- resp_rek$a.spm8_2
  respNew$contactEmail <- tolower(resp_rek$e.spm8_2)
  respNew$contactAge <- resp_rek$spm8_3
  respNew$contactSex <- resp_rek$spm8_4
  
  
  #If emailPart mail empty or "Samme" then overwrite w. contact email adress
  respNew[is.na(respNew$emailPart) | respNew$emailPart %in% c("Samme","før") | respNew$emailPart == "","emailPart"] <- 
    respNew[is.na(respNew$emailPart)  | respNew$emailPart %in% c("Samme","før") | respNew$emailPart == "før" | respNew$emailPart == "","contactEmail"]
  
  
  
  #Motivation and expectaitons
  
  respNew$expertise <- resp_rek$spm8_5 #ok
  respNew$hobby <- resp_rek$a.spm9_3 #ok
  
  #respNew$motivations <- respW$spm8_6
  respNew$rateTrip <- resp_rek$spm8_7 #ok
  respNew$rateCatch <- resp_rek$spm8_7a #ok
  
  respNew$amountSatisfaction <- resp_rek$c.spm9_3
  respNew$unitAmountSatisfaction <- resp_rek$d.spm9_3
  respNew$lastAmountSatisfaction <- resp_rek$d2.spm9_3
  respNew$sizeSatisfaction <- resp_rek$e.spm9_3
  respNew$unitSizeSatisfaction <- resp_rek$f.spm9_3
  respNew$lastSizeSatisfaction <- resp_rek$f2.spm9_3

    
  respNew$fiskepleje <- resp_rek$spm9_1
  respNew$fangstjourn <- resp_rek$spm9_2
  respNew$regFangstjourn <- resp_rek$spm9_2.2

 # respNew$catchExpectations <- respW$b.spm9_3

  
  
  #Transportation
  
  respNew$transportMethod <- resp_rek$a.spm9_4
  respNew$carBrand <- resp_rek$ha_a.spm9_4
  respNew$carYear <- resp_rek$ha_b.spm9_4
  respNew$carFuel <- resp_rek$ha_c.spm9_4
  
  respNew$numberInCar <- resp_rek$d.spm9_4
  respNew$otherPurposeTransp <- resp_rek$f.spm9_4
  respNew$transportExpenses <- resp_rek$ha_d.spm9_4
    
  respNew$startHomeAdress <- resp_rek$b.spm9_4
  
  respNew$otherStartStreet <- resp_rek$Gade
  respNew$otherStartPostCode <- resp_rek$Postnummer
  respNew$otherStartCity <- resp_rek$By
  
  respNew$transpLengthKM <- resp_rek$c.spm9_4
  respNew$transportHours <- round(resp_rek$a.e.spm9_3+(resp_rek$b.e.spm9_3/60),1)
 # respNew$transportHours <- round(resp_rek$Timer.e.spm9_3+(resp_rek$Minutter.e.spm9_3/60),1)

  #Expenses
  
  respNew$expensePublicTransportDKK <- resp_rek$expense1
  respNew$expensePermitsDKK <- resp_rek$expense3
  respNew$expenseFishingGuiderDKK <- resp_rek$expense5
  respNew$expenseSmallGearDKK <- resp_rek$expense7
  respNew$expenseLargeGearDKK <- resp_rek$expense9
  respNew$expenseFoodDKK <- resp_rek$expense11
  respNew$expenseOtherEquipmentDKK <- resp_rek$expense13
  respNew$expenseAccomodationDKK <- resp_rek$expense15
  respNew$expenseOtherDKK <- resp_rek$expense17
  
  respNew$expensePublicTransportEUR <- resp_rek$expense2
  respNew$expensePermitsEUR <- resp_rek$expense4
  respNew$expenseFishingGuiderEUR <- resp_rek$expense6
  respNew$expenseSmallGearEUR <- resp_rek$expense8
  respNew$expenseLargeGearEUR <- resp_rek$expense10
  respNew$expenseFoodEUR <- resp_rek$expense12
  respNew$expenseOtherEquipmentEUR <- resp_rek$expense14
  respNew$expenseAccomodationEUR <- resp_rek$expense16
  respNew$expenseOtherEUR <- resp_rek$expense18
  
  respNew[respNew =='Yes' ] <- "Ja"
  respNew[respNew =='No' | respNew =='Nein'] <- "Nej"

  return(respNew)  
}

#------------------------------------------------------------------------------;

## 13 Rename columns 2


rename_onl_survey <- function(data,x, y){
  colnames(data)[grepl(x, names(data)) ] <- paste0(y, stri_extract_first_regex(
    colnames(data[ ,grepl(x, names(data)) ]), "[0-9]+"),
    str_sub(colnames(data[ ,grepl(x, names(data)) ]),-7))
  return(data)
}

#------------------------------------------------------------------------------;

## 14 clean ce data


create_choise_exp <- function(CE_data0){
  
  CE_data <- CE_data0
    CE_data[CE_data$Response.ID=="CE187","Group"] <- "OF163"
    CE_data[CE_data$Response.ID=="CE200","Group"] <- "OF209"
    
    colnames(CE_data)[grep("situation", names(CE_data))] <- c("Situation1","Situation2","Situation3",
                                                              "Situation4","Situation5","Situation6")
    
    for (i in 1:6) {
      CE_data[stri_detect_fixed(CE_data[[paste0('Situation',i)]],"Fiskemulighed 1"),paste0("Situation",i)] <- 1
      CE_data[stri_detect_fixed(CE_data[[paste0('Situation',i)]],"Fiskemulighed 2"),paste0("Situation",i)] <- 2
      CE_data[stri_detect_fixed(CE_data[[paste0('Situation',i)]],"Fiskemulighed 3"),paste0("Situation",i)] <- 3
      CE_data[stri_detect_fixed(CE_data[[paste0('Situation',i)]],"Jeg bliver"),paste0("Situation",i)] <- 4
    }
    
    CE_data$respNum <- gsub("(.*?)(-.*)", "\\1",CE_data$survey_ID)
    CE_data$tripId <- gsub("^([0-9]).-*", "\\", CE_data$survey_ID)
    
    CE_data2 <- CE_data[,c("Response.ID","Group","Date.Submitted","Email","tripId","respNum","name.spm6_2","email.spm6_2","phone.spm6_2",
                           "Situation1","Situation2","Situation3","Situation4","Situation5","Situation6")]
    
    colnames(CE_data2) <- c("sgCEId","sgId","dateSubmitted","ceEmail","tripId","respNum","name","contactEmailCE","phone",
                            "situation1","situation2","situation3","situation4","situation5","situation6")
    
    CE_data2$dateSubmitted <- gsub(',', '', CE_data2$dateSubmitted)

  #CE_data2[CE_data2$ceEmail=="","ceEmail"] <-  CE_data2[CE_data2$ceEmail=="","contactEmail"]
  
  return(CE_data2)
}

#------------------------------------------------------------------------------;

## 14 combine ce data to respondent data

# ce_dat <- ce_data
combine_choise_exp <- function(ce_dat, resp_data){
  
    ce_dat$email <- ce_dat$contactEmailCE
    # ce_dat$date <- 
    resp_data$email <- resp_data$contactEmail
  
  resp_up1 <- merge(resp_data,ce_dat[ce_dat$tripId != "",-c(2,4)], by=c("tripId","respNum"),all.y = TRUE)
  # resp_up2 <- merge(resp_data,ce_dat[!is.na(ce_dat$sgId),-c(4:6)], by="sgId",all.y = TRUE)
  resp_up2 <- merge(resp_data,ce_dat[startsWith(ce_dat$sgId,"O")==T,-c(4:6)], by="email",all.y = TRUE)
  resp_up3 <- merge(resp_data,ce_dat[startsWith(ce_dat$sgId,"O")==F & is.na(ce_dat$sgId),-c(2,5:6)], by="email",all.y = TRUE)
  
  # resp_up1 <- resp_up1[,c(3,1,4:12,2,13:90)]
  # resp_up2 <- resp_up2
  # resp_up3 <- resp_up3[,c(2:54,1,55:90)]
  # 
  # row.names(resp_up1)<-NULL
  # row.names(resp_up2)<-as.numeric(rownames(resp_up2))+length(row.names(resp_up1))
  # row.names(resp_up3)<-as.numeric(rownames(resp_up3))+length(row.names(resp_up1))+length(row.names(resp_up2))
  
  ce_res <- rbind(resp_up1,resp_up2,resp_up3)
  
  resp_ce <- merge(resp_data,ce_res,all=TRUE)
  
  return(resp_ce)
  
  
}

# Additional functions for merging data -----

# Ensuring the same column classes across datasets
columnAdjust <- function(df){
    for (col in colnames(df)){
        if(str_detect(col,"code")){
            df[[col]]<- substr(df[[col]],0,1)
            df[[col]] <- as.numeric(df[[col]])
        }
        if(str_detect(col,"length|weight")){
            df[[col]] <- str_replace_all(df[[col]],"%2C",",")
            df[[col]] <- str_replace_all(df[[col]],",",".")
            df[[col]] <- as.numeric(df[[col]])
            # df[[col]] <- as.numeric(sprintf("%s.%s",temp[1],temp[2]))
        }
        if(str_detect(col,"number")){
            df[[col]] <- as.numeric(df[[col]])
        }
        if (str_detect(col, "otolith|species")){
            df[[col]][df[[col]]==""]<-NA
            df[[col]]<- as.character(df[[col]])
        }
    }
    return(df)
}


# functions to be used in the salmon survey
priority <- function(x){y = ifelse(x == "Vigtigste<br /> grund", 1,
                                   ifelse(x == "Anden vigtigste<br /> grund",2,
                                          ifelse(x == "Tredje vigtigste<br /> grund",3, NA)))
return(y)
}

priority2 <- function(x){y = ifelse(x == "Vigtigste<br />hobby", 1,
                                    ifelse(x == "Næstvigtigste<br />hobby",2,
                                           ifelse(x == "3. vigtigste<br />hobby",3,
                                                  ifelse(x == "En hobby blandt<br />mange",4,NA))))
return(y)
}

priority3 <- function(x){y = ifelse(x == "Meget<br />bedre", 2,
                                    ifelse(x == "Noget<br />bedre",1,
                                           ifelse(x == "Som<br />gennemsnittet",0,
                                                  ifelse(x == "Noget<br />dårligere",-1,
                                                         ifelse(x == "Meget<br />dårligere",-2,NA)))))
return(y)
}


agreeing <- function(x){y = ifelse(x == "Fuldstændig<br />enig", 2,
                                   ifelse(x == "Overvejende<br />enig",1,
                                          ifelse(x == "Hverken<br />eller",0,
                                                 ifelse(x == "Overvejende<br />uenig",-1,
                                                        ifelse(x == "Fuldstændig<br />uenig",-2,NA)))))
return(y)
}

agreeing2 <- function(x){y = ifelse(x == "Mange<br />flere ture", 3,
                                    ifelse(x == "Flere ture",2,
                                           ifelse(x == "Lidt flere ture",1,
                                                  ifelse(x == "Ca. det samme",0,
                                                         ifelse(x == "Lidt færre ture",-1,
                                                                ifelse(x == "Færre ture",-2,
                                                                       ifelse(x == "Meget færre ture",-3,NA)))))))
return(y)
}


timeMaker <- function(start,end){
  return(paste0(trimws(str_pad(as.character(start),width = 2,pad = "0")),
                trimws(str_pad(as.character(end),width = 2,pad = "0"))))
}
