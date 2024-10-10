path1 <- "C:/Users/chin008/OneDrive - Wageningen University & Research/git/eggsandlarvae_bberges/eggsandlarvae"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)
library(tidyr)
library(sp)
library(sf)

# load DRS data
DATRAS_eggsLarvae <- readICES(file.path('./data','DATRAS_eggsLarvae_all.csv'), strict = TRUE)

DATRAS_eggsLarvae[[1]] #CA data
DATRAS_eggsLarvae[[2]] #HH data
DATRAS_eggsLarvae[[3]] #HL data
DATRAS_eggsLarvae[[3]]$Count <- DATRAS_eggsLarvae[[3]]$SubFactor * DATRAS_eggsLarvae[[3]]$TotalNo

dAll <- addSpectrum(DATRAS_eggsLarvae, cm.breaks=seq(0,40,by=1))
names(dAll[[1]])
summary(dAll[[1]])
plot(dAll[[2]]$lon, dAll[[2]]$lat, pch = 1)

summary(dAll[[3]])
dAll[[3]]$Count <- ifelse(is.na(dAll[[3]]$Count), 0, dAll[[3]]$Count)
dAll[[3]]$LngtClas <- ifelse(is.na(dAll[[3]]$LngtClas), 0, dAll[[3]]$LngtClas)
dAll[[3]]$LngtCm <- ifelse(is.na(dAll[[3]]$LngtCm), 0, dAll[[3]]$LngtCm)
table(dAll$Ship)
table(dAll$Gear)
dAll$Gear <- "MRN2"
dAll$ShipG = factor(paste(dAll$Ship,
                          dAll$Gear, sep = ":"))
table(dAll$ShipG)
dAll$dum = 1

# Abundance
# number at length
NoAtLngt = aggregate(Count ~ haul.id + LngtClas, data = dAll[[3]], FUN = sum)
dAll$NL = t(t(dAll$N)*1)
view(dAll[[2]])
dAll$Abundance <- rowSums(dAll$NL[, 1:ncol(dAll$NL)])
dAll$CPUE <- dAll$Abundance/dAll[[2]]$SweepLngt

dAll$Nage <- matrix(dAll$Abundance, ncol = 1)
colnames(dAll$Nage) <- "1"

## Remove levels of Gear, ShipG, StatRec with only zero observations
dd <- removeZeroClusters(dAll, response="Abundance", factors=c("Gear","Ship"))
dd <- subset(dd,!is.na(Depth))

simplegrid = getGrid(dd, nLon = 25)
plot(simplegrid)

# grid as data.frame
grid.df = subset(dd, haul.id %in% simplegrid[[3]])[[2]]
table(dd[[2]]$Ship)
table(dd[[3]]$Gear)
dd[[1]]$Gear <- "MRN2"
dd[[3]]$Gear <- "MRN2"
str(dd[[2]])
summary(dd[[1]])
dd[[1]]$Age <- 0
table(dd[[1]]$Year)


# Save DRS data
#save(dd, file = "./data/DRS2DATRAS_eggsLarvae.RData")
load(paste0(getwd(), "./data/DRS2DATRAS_eggsLarvae.RData"))

###################################
#  delta log-normal distribution  #
###################################
load(paste0(getwd(), "./data/DRS2DATRAS_eggsLarvae.RData"))
simplegrid = getGrid(dd, nLon = 25)
plot(simplegrid)

# grid as data.frame
grid.df = subset(dd, haul.id %in% simplegrid[[3]])[[2]]

# summary of output variables
summary(dd$Abundance)
summary(dd$CPUE)
hist(dd$CPUE)
hist(dd$Abundance)

# Abundance as the dependent variable
## delta lognormal model requires two formulas

### model 1 ###
model = "Year + s(sqrt(Depth), bs = 'cr') + Ship + DayNight + s(lon, lat, bs = 'ds') + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"
modelZ = "Year + s(sqrt(Depth), bs = 'cr') + Ship + DayNight + s(lon, lat, bs = 'ds') + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"

system.time(dln1 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln1 = AIC.surveyIdx(dln1)
aic.dln1 # 2302
# QQ plot
qqnorm(residuals(dln1), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln1))); abline(0,1)
# index
surveyIdxPlots(dln1,dd,select="index",par=list(mfrow=c(1,1),mar=c(4,3,3,3)),main="Index")

### model 2 ###
model = "Year + s(sqrt(Depth), bs = 'cr') + Ship + DayNight + s(lon, lat, bs = 'ds', k = 20) + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"
modelZ = "Year + s(sqrt(Depth), bs = 'cr') + Ship + DayNight + s(lon, lat, bs = 'ds', k = 20) + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"

system.time(dln2 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln2 = AIC.surveyIdx(dln2)
aic.dln2 # 2304
qqnorm(residuals(dln2), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln2))); abline(0,1)

### model 3 ###
# adjust the smoother
model = "Year + s(sqrt(Depth), bs = 'ds') + Ship + DayNight + s(lon, lat, bs = 'ds', k = 20) + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"
modelZ = "Year + s(sqrt(Depth), bs = 'ds') + Ship + DayNight + s(lon, lat, bs = 'ds', k = 20) + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"

system.time(dln3 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln3 = AIC.surveyIdx(dln3)
aic.dln3 # 2308

### model 4 ###
# remove Ship
model = "Year + s(sqrt(Depth), bs = 'cr') + DayNight + s(lon, lat, bs = 'ds', k = 20) + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"
modelZ = "Year + s(sqrt(Depth), bs = 'cr') + DayNight + s(lon, lat, bs = 'ds', k = 20) + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"

system.time(dln4 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln4 = AIC.surveyIdx(dln4)
aic.dln4 # 2298
qqnorm(residuals(dln4), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln4))); abline(0,1)

### model 5 ###
# remove DayNight
model = "Year + s(sqrt(Depth), bs = 'cr') + s(lon, lat, bs = 'ds', k = 10) + s(lon, lat, bs = 'ds', by = Year, m = c(0, 0.5)) + offset(log(SweepLngt))"
modelZ = "Year + s(sqrt(Depth), bs = 'cr') + s(lon, lat, bs = 'ds', k = 10) + s(lon, lat, bs = 'ds', by = Year, m = c(0, 0.5)) + offset(log(SweepLngt))"

system.time(dln5 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln5 = AIC.surveyIdx(dln5)
aic.dln5 # 2281
qqnorm(residuals(dln5), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln5))); abline(0,1)

# no need to have DayNight nor Ship effect
# index
surveyIdxPlots(dln5,dd,select="index",par=list(mfrow=c(1,1),mar=c(4,3,3,3)),main="DRS Index")
surveyIdxPlots(dln5, dd, myids=NULL, predD=grid.df, select="absolutemap", year=dd[[1]]$Year, 
               colors=rev(heat.colors(6)), par=list(mfrow=n2mfrow(nlevels(dd$Year)), 
                                                    mar=c(0,0,2,0), cex=0.6), legend=TRUE, legend.signif=1, map.cex=1)

### model 6 ###
# remove depth
model = "Year + s(lon, lat, bs = 'ds', k = 20) + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', k = 20) + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"

system.time(dln6 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln6 = AIC.surveyIdx(dln6)
aic.dln6 # 2307

### model 7 ###
# remove depth, add DayNight
model = "Year + s(lon, lat, bs = 'ds', k = 20) + DayNight + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', k = 20) + DayNight + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"

system.time(dln7 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln7 = AIC.surveyIdx(dln7)
aic.dln7 # 2303

### model 8 ###
# upgrade model 4, without k = 20
model = "Year + s(sqrt(Depth), bs = 'cr') + DayNight + s(lon, lat, bs = 'ds') + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"
modelZ = "Year + s(sqrt(Depth), bs = 'cr') + DayNight + s(lon, lat, bs = 'ds') + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"

system.time(dln8 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln8 = AIC.surveyIdx(dln8)
aic.dln8 # 2296
qqnorm(residuals(dln8), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln8))); abline(0,1)

### model 9 ###
# remove DayNight, without k = 20
model = "Year + s(sqrt(Depth), bs = 'cr') + s(lon, lat, bs = 'ds') + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"
modelZ = "Year + s(sqrt(Depth), bs = 'cr') + s(lon, lat, bs = 'ds') + s(lon, lat, bs = 'ds', by = Year) + offset(log(SweepLngt))"

system.time(dln9 <- getSurveyIdx(dd, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln9 = AIC.surveyIdx(dln9)
aic.dln9 # 2295
qqnorm(residuals(dln9), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln9))); abline(0,1)
## model 5 is still the best for now

#########################
##   Save DRS models   ##
#########################
resDRS <- list(DRS1 = dln1,
               DRS2 = dln2,
               DRS3 = dln3,
               DRS4 = dln4,
               DRS5 = dln5,
               DRS6 = dln6,
               DRS7 = dln7,
               DRS8 = dln8,
               DRS9 = dln9)
save(resDRS, file = "DRS_LogNormalAbundance.RData")


#########################
###   Load MIK Data   ###
#########################
MIK2DATRAS_eggsLarvae <- readICES(file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'), strict = TRUE)
MIK2DATRAS_eggsLarvae  <- addSpatialData(MIK2DATRAS_eggsLarvae,"./shapefiles/ICES_areas.shp")

MIK2DATRAS_eggsLarvae  <- subset(MIK2DATRAS_eggsLarvae,
                                 ICES_area %in% as.character(c("IVa","IVb","IVc")))

MIK2DATRAS_eggsLarvae[[3]]$Count <- MIK2DATRAS_eggsLarvae[[3]]$TotalNo * MIK2DATRAS_eggsLarvae[[3]]$SubFactor
table(MIK2DATRAS_eggsLarvae$Ship)
table(MIK2DATRAS_eggsLarvae$Gear)

# make a new variable, combine Ship and Gear
MIK2DATRAS_eggsLarvae$ShipG = factor(paste(MIK2DATRAS_eggsLarvae$Ship,
                                           MIK2DATRAS_eggsLarvae$Gear, sep = ":"))
table(MIK2DATRAS_eggsLarvae$ShipG)
MIK2DATRAS_eggsLarvae$dum = 1 
summary(MIK2DATRAS_eggsLarvae)

### Check the valid haul and the explanation of HaulDur 
#dd.mik <- subset(MIK2DATRAS_eggsLarvae,HaulVal=="V" & HaulDur>0) ## not applicable.
#' Validity of hauls has been checked during the data format editing process.
#' Only ELHaulFlag == 'U' means invalid hauls. Missing information could be due to the old data from 19xx.
#' Haul duration 0 or NAs having flowmeter revolutions and they should be considered as valid hauls.

dd.mik <- addSpectrum(MIK2DATRAS_eggsLarvae, cm.breaks = seq(0, 40, by = 1))
names(dd.mik[[1]])
summary(dd.mik[[1]])
summary(dd.mik[[2]])
summary(dd.mik[[3]])
plot(dd.mik[[2]]$lon, dd.mik[[2]]$lat, pch = 1)

# number at length
NoAtLngt = aggregate(Count ~ haul.id + LngtClas, data = dd.mik[[3]], FUN = sum)
dd.mik$NL = dd.mik$N
#view(dd.mik[[2]])

# Abundance
dd.mik$Abundance <- rowSums(dd.mik$NL[, 1:ncol(dd.mik$NL)])
dd.mik$CPUE <- dd.mik$Abundance/dd.mik[[2]]$SweepLngt
dd.mik$Nage <- matrix(dd.mik$Abundance, ncol = 1)
colnames(dd.mik$Nage) <- "1"

## Remove levels of Gear, ShipG, StatRec with only zero observations
dd.m <- removeZeroClusters(dd.mik, response="Abundance", factors=c("Gear","Ship"))
dd.m <- subset(dd.m,!is.na(Depth))

plot(dd.m$Year,dd.m$Depth)

simplegrid = getGrid(dd.m, nLon = 40)
plot(simplegrid)

# grid as data.frame
grid.df = subset(dd.m, haul.id %in% simplegrid[[3]])[[2]]

table(dd.m[[2]]$Ship)
table(dd.m[[2]]$Gear)
str(dd.m[[2]])
summary(dd.m[[1]])
summary(dd.m[[2]])
dd.m[[1]]$Age <- 0

# Save MIK data
#save(dd.m, file = "./data/MIK_eggsLarvae.RData")
load(paste0(getwd(), "./data/MIK_eggsLarvae.RData"))

##### Delta lognormal modelling #####
###  model 1  ###
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln1 <- getSurveyIdx(dd.m, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln1 = AIC.surveyIdx(dln1)
aic.dln1 # 75096
# QQ plot
qqnorm(residuals(dln1), main = paste0("Delta lognormal, MIK aic = ", round(aic.dln1))); abline(0,1)
# index
surveyIdxPlots(dln1,dd.m, select="index", par = list(mfrow=c(1, 1), mar=c(4,3,3,3)), main="Index")

###  model 2  ### 
## remove Country
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln2 <- getSurveyIdx(dd.m, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln2 = AIC.surveyIdx(dln2)
aic.dln2 # 75168, not good 
# QQ plot
qqnorm(residuals(dln2), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln2))); abline(0,1)
# index
surveyIdxPlots(dln2, dd.m, select="index", par = list(mfrow = c(1, 1), mar = c(4,3,3,3)), main="Index")

###  model 3  ###
## Remove ShipG
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln3 <- getSurveyIdx(dd.m, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln3 = AIC.surveyIdx(dln3)
aic.dln3 # 75376
# QQ plot 
qqnorm(residuals(dln3), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln3))); abline(0,1)
# index
surveyIdxPlots(dln3, dd.m, select="index", par = list(mfrow = c(1, 1), mar = c(4,3,3,3)), main="Index")

###  model 4  ###
##
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln4 <- getSurveyIdx(dd.m, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln4 = AIC.surveyIdx(dln4)
aic.dln4 # 75253

# QQ plot 
qqnorm(residuals(dln4), main = paste0("Delta lognormal, DRS aic = ", round(aic.dln4))); abline(0,1)
# index
surveyIdxPlots(dln4, dd.m, select="index", par = list(mfrow = c(1, 1), mar = c(4,3,3,3)), main="Index")

###  model 5  ###
## adjust k
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 100) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 100) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln5 <- getSurveyIdx(dd.m, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln5 = AIC.surveyIdx(dln5)
aic.dln5 # 75044
# QQ plot
qqnorm(residuals(dln5), main = paste0("Delta lognormal, MIK aic = ", round(aic.dln5))); abline(0,1)
# index
surveyIdxPlots(dln5,dd.m, select="index", par = list(mfrow=c(1, 1), mar=c(4,3,3,3)), main="Index")

#########################
###  Save MIK models  ###
#########################
resMIK <- list(MIK1 = dln1,
               MIK2 = dln2,
               MIK3 = dln3,
               MIK4 = dln4,
               MIK5 = dln5)
save(resMIK, file = "MIK_LogNormalAbundance.RData")

#########################
##  Combine DRS + MIK  ##
#########################
# load DRS data
load(paste0(getwd(), "./data/DRS2DATRAS_eggsLarvae.RData"))
dd$Survey <- "DRS"
# load MIK data
load(paste0(getwd(), "./data/MIK_eggsLarvae.RData"))
dd.m$Survey <- "MIK"

dd.both <- c(dd, dd.m)
summary(dd.both)
summary(dd.both[[1]])
summary(dd.both$Abundance)
summary(dd.both[[2]])

simplegrid = getGrid(dd.both, nLon = 40)
plot(simplegrid)

# grid as data.frame
grid.df = subset(dd.both, haul.id %in% simplegrid[[3]])[[2]]

# summary of output variables
summary(dd.both$Abundance)
summary(dd.both$CPUE)
hist(dd.both$CPUE)
table(dd.both$Survey)

###  model 1  ###
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + Country + DayNight + Survey + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + Country + DayNight + Survey + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln1 <- getSurveyIdx(dd.both, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln1 = AIC.surveyIdx(dln1)
aic.dln1 # 77582
# QQ plot
qqnorm(residuals(dln1), main = paste0("Delta lognormal, MIK + DRS aic = ", round(aic.dln1))); abline(0,1)
# index
surveyIdxPlots(dln1,dd.m, select="index", par = list(mfrow=c(1, 1), mar=c(4,3,3,3)), main="Index")

###  model 2  ###
## remove DayNight
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + Country + Survey + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + Country + Survey + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln2 <- getSurveyIdx(dd.both, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln2 = AIC.surveyIdx(dln2)
aic.dln2 # 77630, not a good idea
# QQ plot
qqnorm(residuals(dln2), main = paste0("Delta lognormal, MIK + DRS aic = ", round(aic.dln2))); abline(0,1)


###  model 3  ###
## remove Survey
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + DayNight + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + DayNight + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln3 <- getSurveyIdx(dd.both, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln3 = AIC.surveyIdx(dln3)
aic.dln3 # 77585, not bad, Survey does not need to be in the model.
# QQ plot
qqnorm(residuals(dln3), main = paste0("Delta lognormal, MIK + DRS aic = ", round(aic.dln3))); abline(0,1)


###  model 4  ###
## remove ShipG
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + DayNight + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 80) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + DayNight + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln4 <- getSurveyIdx(dd.both, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
# AIC
aic.dln4 = AIC.surveyIdx(dln4)
aic.dln4 # 77839


###  model 5  ###
## adjust k
model = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 100) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + DayNight + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"
modelZ = "Year + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 100) + s(lon, lat, bs='ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + DayNight + Country + s(sqrt(Depth), bs = 'cr') + offset(log(SweepLngt))"

system.time(dln5 <- getSurveyIdx(dd.both, ages = 1, predD = grid.df, fam = "LogNormal",
                                 modelP = model, modelZ = modelZ, gamma = 1, cutoff = 0.1,
                                 control = list(trace = TRUE, maxit = 20)))
aic.dln5 = AIC.surveyIdx(dln5)
aic.dln5 # 77538, this is good
# QQ plot
qqnorm(residuals(dln5), main = paste0("Delta lognormal, MIK + DRS aic = ", round(aic.dln5))); abline(0,1)


resBoth <-list(BOTH1 = dln1,
               BOTH2 = dln2,
               BOTH3 = dln3,
               BOTH4 = dln4,
               BOTH5 = dln5)
save(resBoth, file = "MIKDRS_LogNormalAbundance.RData")
# index
surveyIdxPlots(dln5,dd.both, select="index", par = list(mfrow=c(1, 1), mar=c(4,3,3,3)), main="Index")
