rm(list=(ls()))

path1 <- "//wurnet.nl/dfs-root/IMARES/IJmuiden/WOT/WOT surveys Zout - Haringlarven/Down's recruitment survey/New index calculation/"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)
library(tidyr)

DATRAS_eggsLarvae <- readICES(file.path('./data','DATRAS_eggsLarvae_all.csv'), strict = TRUE)

# DATRAS_eggsLarvae  <- subset(DATRAS_eggsLarvae,Species==species,Quarter == 1,Year %in% yearlist,HaulVal=="V",StdSpecRecCode==1)
#DATRAS_eggsLarvae  <- addSpatialData(DATRAS_eggsLarvae,"./data/shapefiles/ICES_areas.shp")
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

#####################
# merge the data
ca <- dAll[[1]]
hh <- dAll[[2]]
hl <- dAll[[3]]

hl.exp <- hl %>%
  tidyr::uncount(as.integer(Count))

hist(hl.exp$LngtCm)
hist(hl$Count)
table(hl$Count)

ca.hh <- merge(ca[, c("StNo", "HaulNo", "Year", "SpecCode", "LngtClas", "FishID",
                      "Valid_Aphia", "LngtCm", "haul.id")], 
               hh, by = c("HaulNo", "Year", "StNo", "haul.id"),
               all.x = TRUE)
ca.hh.hl <- merge(ca.hh,
                  hl)

#######################
# abundance
# number at length
NoAtLngt = aggregate(Count ~ haul.id + LngtClas, data = dAll[[3]], FUN = sum)
dAll$NL = t(t(dAll$N)*1)
view(dAll[[2]])

dAll$Abundance <- rowSums(dAll$NL[, 1:ncol(dAll$NL)])
dAll$Nage <- matrix(dAll$Abundance, ncol = 1)
colnames(dAll$Nage) <- "1"

## Remove levels of Gear, ShipG, StatRec with only zero observations
dd <- removeZeroClusters(dAll, response="Abundance", factors=c("Gear","Ship"))
dd <- subset(dd,!is.na(Depth))

simplegrid = getGrid(dd, nLon = 30)
plot(simplegrid)

# grid as data.frame
grid.df = subset(dd, haul.id %in% simplegrid[[3]])[[2]]
table(dd[[2]]$Ship)
str(dd[[2]])
summary(dd[[1]])
dd[[1]]$Age <- 0
table(dd[[1]]$Year)

## model - tweedie
model1 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=40) + DayNight + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + s(Ship, bs = 're') + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(TW1 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model1, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw1 = AIC.surveyIdx(TW1) # 2489.16
qqnorm(residuals(TW1), main = paste0("Tweedie, DRS AIC = ", round(aic.tw1))); abline(0,1)

# remove depth
model2 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=40) + DayNight + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + s(Ship, bs = 're') + offset(log(SweepLngt))"
system.time(TW2 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                               fam = "Tweedie", modelP = model2, gamma = 1, 
                               cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw2 = AIC.surveyIdx(TW2) # 2487.85
qqnorm(residuals(TW2), main = paste0("Tweedie, DRS AIC = ", round(aic.tw2))); abline(0,1) 
# TW2 is a better model

# remove - spatial, time interaction
model3 <- "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=40) + DayNight + s(Ship, bs = 're') + offset(log(SweepLngt))"
system.time(TW3 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model3, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw3 = AIC.surveyIdx(TW3) # 2503.37
qqnorm(residuals(TW3), main = paste0("Tweedie, DRS AIC = ", round(aic.tw3))); abline(0,1)
# TW2 is a better model

# remove Ship random effect
model4 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=40) + DayNight + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + offset(log(SweepLngt))"
system.time(TW4 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model4, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw4 = AIC.surveyIdx(TW4) # 2487.85
# TW4 is better => no need to have ship effect
qqnorm(residuals(TW4), main = paste0("Tweedie, DRS AIC = ", round(aic.tw4))); abline(0,1)

# remove Daynight
model5 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=40) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(TW5 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model5, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw5 = AIC.surveyIdx(TW5) # 2521.38
# TW4 is better

##############
# index plot 
surveyIdxPlots(TW1, dd, select = "index", par = list(mfrow = c(1, 1), mar = c(4, 3, 3, 3), main = "Index"))
surveyIdxPlots(TW4, dd, select = "index", par = list(mfrow = c(1, 1), mar = c(4, 3, 3, 3), main = "TW4 DRS"))
surveyIdxPlots(TW4, dd, myids = NULL)

table(dd[[1]]$Year)

surveyIdxPlots(TW4, dd, myids=NULL, predD=grid.df, select="absolutemap", year=dd[[1]]$Year, colors=rev(heat.colors(6)), par=list(mfrow=n2mfrow(nlevels(dd$Year)), mar=c(0,0,2,0), cex=0.6), legend=TRUE, legend.signif=1, map.cex=1)


###########################
## model - log normal

