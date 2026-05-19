getDatrasExchange_kelly <- function(existing_dat, survey, years, strict = TRUE, species)
{
  if(!is.null(existing_dat) == TRUE){
    ca <- existing_dat[["ca"]]
    if (identical(ca, FALSE)) {
      stop()}
    
    hl <- existing_dat[["hl"]]
    hh <- existing_dat[["hh"]]
    
  }else{
    ca <- icesDatras::getDATRAS("CA", survey = survey, years = years)
    if (identical(ca, FALSE)) {
      stop()
    }
    
    hh <- icesDatras::getDATRAS("HH", survey = survey, years = years)
    hl <- icesDatras::getDATRAS("HL", survey = survey, years = years)
  }
  
  
  d <- list(CA = ca, HH = hh, HL = hl)
  cat("Classes of the variables\n")
  print(lapply(d, sapply, class))
  for (i in 1:3) d[[i]] <- renameDATRAS(d[[i]])
  d <- minus9toNA(d)
  if (is.null(d$CA$StatRec))
    d$CA$StatRec <- d$CA$AreaCode
  d <- addExtraVariables_kelly(d)
  d <- DATRAS:::fixMissingHaulIds(d, strict = strict)
  for (i in 1:3) {
    for (k in 1:ncol(d[[i]])) {
      if (is.character(d[[i]][, k]))
        d[[i]][, k] <- factor(d[[i]][, k])
    }
  }
  class(d) <- "DATRASraw"
  d
}




addExtraVariables_kelly <- function(IBTS)
{
  d1 <- IBTS[[1]]
  d2 <- IBTS[[2]]
  d3 <- IBTS[[3]]
  # d1<-ca[ca$Year%in% 1995,]
  # d2<-hh[hh$Year%in% 1995,]
  # d3<-hl[hl$Year%in% 1995,]
  #LngtCode2cm <- c(. = 0.1, `0` = 0.1, `1` = 1, `2` = 1, `5` = 1)
  file <- system.file("SpeciesTable.csv", package = "DATRAS")
  specdat <- read.csv(file, skip = 3, strip.white = TRUE, stringsAsFactors = TRUE)
  SpecCode2species <- structure(as.character(specdat$Species),
                                names = specdat$TSN.code)
  file <- system.file("WoRMSTable.csv", package = "DATRAS")
  specdat <- read.csv(file, stringsAsFactors = TRUE)
  WSpecCode2species <- structure(as.character(specdat$ScientificName_WoRMS),
                                 names = specdat$WoRMS_AphiaID)

  haul.id <- quote(factor(paste(Year, Country, Ship,
                                Gear, HaulID, sep = ":")))
  d1$haul.id <- eval(haul.id, d1)
  d2$haul.id <- eval(haul.id, d2)
  d3$haul.id <- eval(haul.id, d3)
  d3 <- merge(d3, d2[c("haul.id","Length", "Number", "SubFactor")],
              by = "haul.id", all.x = TRUE, sort = FALSE)
  #multiplier <- ifelse(d3$DataType == "C", d3$HaulDur/60, d3$SubFactor)
  d3$Count <- d3$Number * d3$SubFactor
  #d2$abstime <- local(Year + (Month - 1) * 1/12 + (Day - 1)/365,
  #                    d2)
  #d2$timeOfYear <- local((Month - 1) * 1/12 + (Day - 1)/365,
  #                       d2)
  #d2$TimeShotHour = as.integer(d2$TimeShot/100) + (d2$TimeShot%%100)/60
  d2 <- transform(d2, lon = StartLongitude, lat = StartLatitude)
  file <- system.file("roundfish.csv", package = "DATRAS")
  rf <- read.table(file, stringsAsFactors = TRUE)
  d2$Roundfish = NA
  for (r in 1:nrow(rf)) d2$Roundfish[d2$StatRec == as.character(rf[r,2])] = rf[r, 1]
  d2$Roundfish = as.factor(d2$Roundfish)
  diff <- setdiff(levels(d2$haul.id), levels(d3$haul.id))
  if (length(diff) > 0) {
    cat("========= WARNING: ============\n")
    cat("Hauls without length info will be interpreted as empty hauls:\n")
    print(diff)
  }
  
  print(unique(d3$Year))
  print(unique(d2$Year))
  
  d1$haul.id <- factor(d1$haul.id, levels = levels(d2$haul.id))
  d3$haul.id <- factor(d3$haul.id, levels = levels(d2$haul.id))
  
  if(sum(which(duplicated(d2$haul.id)))==0){
    d2<-d2}else{
      d2<-d2[-which(duplicated(d2$haul.id)),]
    }
  
  stopifnot(nlevels(d3$haul.id) == nrow(d2))
  cat("Consistency check passed\n")
  IBTS[[1]] <- d1
  IBTS[[2]] <- d2
  IBTS[[3]] <- d3
  for (i in 1:3) {
    IBTS[[i]]$Year <- factor(IBTS[[i]]$Year)
    IBTS[[i]]$Quarter <- factor(IBTS[[i]]$Quarter)
  }
  IBTS
}


