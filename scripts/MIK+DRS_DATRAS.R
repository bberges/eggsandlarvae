path1 <- "C:/Users/chin008/OneDrive - Wageningen University & Research/git/eggsandlarvae_bberges/eggsandlarvae"
#path1 <- "C:/git/harring_eggsandlarvae/"
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
DATRAS_eggsLarvae  <- addSpatialData(DATRAS_eggsLarvae,"./shapefiles/ICES_areas.shp")
#load(paste0(getwd(), "./data/DRS2DATRAS_eggsLarvae.RData"))
DATRAS_eggsLarvae$Survey <- "DRS"

# load MIK data
MIK2DATRAS_eggsLarvae <- readICES(file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'), strict = TRUE)
MIK2DATRAS_eggsLarvae  <- addSpatialData(MIK2DATRAS_eggsLarvae,"./shapefiles/ICES_areas.shp")

MIK2DATRAS_eggsLarvae  <- subset(MIK2DATRAS_eggsLarvae,
                                 ICES_area %in% as.character(c("IVa","IVb","IVc")))
MIK2DATRAS_eggsLarvae$Survey <- "MIK"


# combine DRS and MIK data
eggsLarvae <- c(DATRAS_eggsLarvae, MIK2DATRAS_eggsLarvae)
table(eggsLarvae$Gear)
eggsLarvae$Gear <- ifelse(eggsLarvae$Gear == "MIKMT", "MIKMT", "MRN2")
eggsLarvae[[3]]$Count <- eggsLarvae[[3]]$TotalNo * eggsLarvae[[3]]$SubFactor

# make a new variable, combine Ship and Gear
# Convert Ship to a character vector if it's a factor
eggsLarvae$Ship <- as.character(eggsLarvae$Ship)
# Replace "64T2 " with "64T2"
eggsLarvae$Ship[eggsLarvae$Ship == "64T2 "] <- "64T2"
# Convert Ship back to a factor if needed
eggsLarvae$Ship <- as.factor(eggsLarvae$Ship)
# Check the updated table
table(eggsLarvae$Ship)

eggsLarvae$ShipG = factor(paste(eggsLarvae$Ship,
                                eggsLarvae$Gear, sep = ":"))
table(eggsLarvae$ShipG)
eggsLarvae$dum = 1 

dd.all <- addSpectrum(eggsLarvae, cm.breaks = seq(0, 40, by = 1))
names(dd.all[[1]])
summary(dd.all[[1]])
summary(dd.all[[2]])
summary(dd.all[[3]])
plot(dd.all[[2]]$lon, dd.all[[2]]$lat, pch = 1)

# number at length
NoAtLngt = aggregate(Count ~ haul.id + LngtClas, data = dd.all[[3]], FUN = sum)
dd.all$NL = dd.all$N

dd.all$Abundance <- rowSums(dd.all$NL[, 1:ncol(dd.all$NL)])
dd.all$CPUE <- dd.all$Abundance/dd.all[[2]]$SweepLngt
dd.all$Nage <- matrix(dd.all$CPUE, ncol = 1)
colnames(dd.all$Nage) <- "1"

## Remove levels of Gear, ShipG, StatRec with only zero observations
dd <- removeZeroClusters(dd.all, response="CPUE", factors=c("Gear","Ship"))
dd <- subset(dd,!is.na(Depth))

simplegrid = getGrid(dd, nLon = 25)
plot(simplegrid)

# grid as data.frame
grid.df = subset(dd, haul.id %in% simplegrid[[3]])[[2]]

table(dd[[2]]$Ship)
table(dd[[2]]$Gear)
str(dd[[2]])
summary(dd[[1]])
summary(dd[[2]])
dd[[1]]$Age <- 0

summary(dd$CPUE)
hist(dd$CPUE[dd$CPUE < 0.0001])
table(dd$CPUE)
table(dd$Country)
table(dd$Survey)

# plot
plot(dd$lon,dd$lat,pch=1,cex=sqrt(dd$CPUE)/2,main = "CPUE > 0")
points(dd$lon,dd$lat,pch=".",col=2)
maps::map("worldHires", fill = TRUE, plot = TRUE,add = TRUE, col = grey(0.5))

summary(dd$Abundance[dd$Survey == "MIK"])
summary(dd$Abundance[dd$Survey == "DRS"])
par(mfrow = c(2, 2))
hist(dd$CPUE[dd$Survey == "MIK"][dd$CPUE < 0.000001])
hist(dd$CPUE[dd$Survey == "DRS"][dd$CPUE < 0.001])
hist(dd$Abundance[dd$Survey == "MIK"][dd$Abundance < 10])
hist(dd$Abundance[dd$Survey == "DRS"][dd$Abundance < 10])
###################
#    modelling    #
###################
# tweedie - CPUE

# 
model1 = "Year + Country + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + Survey"
system.time(tw1 <- getSurveyIdx(dd, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model1, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw1 = AIC.surveyIdx(tw1) # -88384
qqnorm(residuals(tw1), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw1))); abline(0,1)

# remove Country
model2 = "Year + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr')"
system.time(tw2 <- getSurveyIdx(dd, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model2, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw2 = AIC.surveyIdx(tw2) # -88351
qqnorm(residuals(tw2), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw2))); abline(0,1)

# remove DayNight
model3 = "Year + Country + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr')"
system.time(tw3 <- getSurveyIdx(dd, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model3, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw3 = AIC.surveyIdx(tw3) # -88326
qqnorm(residuals(tw3), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw3))); abline(0,1)

# remove ShipG
model4 = "Year + Country + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(Depth, bs = 'cr')"
system.time(tw4 <- getSurveyIdx(dd, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model4, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw4 = AIC.surveyIdx(tw4) # -88126
qqnorm(residuals(tw4), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw4))); abline(0,1)

#######################
# tweedie - Abundance #
#######################
NoAtLngt = aggregate(Count ~ haul.id + LngtClas, data = dd.all[[3]], FUN = sum)
dd.all <- dd.all.abundance
dd.all.abundance$NL = dd.all.abundance$N

dd.all.abundance$Abundance <- rowSums(dd.all.abundance$NL[, 1:ncol(dd.all.abundance$NL)])
dd.all.abundance$Nage <- matrix(dd.all.abundance$Abundance, ncol = 1)
colnames(dd.all.abundance$Nage) <- "1"

## Remove levels of Gear, ShipG, StatRec with only zero observations
dd <- removeZeroClusters(dd.all.abundance, response="CPUE", factors=c("Gear","Ship"))
dd <- subset(dd,!is.na(Depth))

simplegrid = getGrid(dd, nLon = 25)
plot(simplegrid)

# grid as data.frame
grid.df = subset(dd, haul.id %in% simplegrid[[3]])[[2]]

summary(dd$Abundance)

# Abundance
model10 = "Year + Country + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + Survey + offset(log(SweepLngt))"
system.time(tw10 <- getSurveyIdx(dd, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model10, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw10 = AIC.surveyIdx(tw10) # -85600
qqnorm(residuals(tw10), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw10))); abline(0,1)

# remove Country
model11 = "Year + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + Survey + offset(log(SweepLngt))"
system.time(tw11 <- getSurveyIdx(dd, ages = 1, predD = grid.df,
                                 fam = "Tweedie", modelP = model11, gamma = 1,
                                 cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw11 = AIC.surveyIdx(tw11) # -85577, not good
qqnorm(residuals(tw11), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw11))); abline(0,1)


# remove DayNight
model12 = "Year + Country + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + Survey + offset(log(SweepLngt))"
system.time(tw12 <- getSurveyIdx(dd, ages = 1, predD = grid.df,
                                 fam = "Tweedie", modelP = model12, gamma = 1,
                                 cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw12 = AIC.surveyIdx(tw12) # -85555, not good
qqnorm(residuals(tw12), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw12))); abline(0,1)

# remove Survey
model13 = "Year + Country + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(tw13 <- getSurveyIdx(dd, ages = 1, predD = grid.df,
                                 fam = "Tweedie", modelP = model13, gamma = 1,
                                 cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw13 = AIC.surveyIdx(tw13) # -85601, about the same as model 10
qqnorm(residuals(tw13), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw13))); abline(0,1)

res_mik.drs <- list(mod1 = tw1,
                    mod2 = tw2,
                    mod3 = tw3, 
                    mod4 = tw4,
                    mod10 = tw10,
                    mod11 = tw11,
                    mod12 = tw12,
                    mod13 = tw13)
save(res_mik.drs, file = "MIKandDRS_tweedieCPUE.RData")

######################
# plots
dd
years = seq(1992, 2023)
#########
# model 1
surveyIdxPlots(tw1,dd,myids=NULL,predD=grid.df,select="absolutemap",year=years,colors=rev(heat.colors(6)),par=list(mfrow=n2mfrow(nlevels(dd$Year)),mar=c(0,0,2,0),cex=0.6),legend=TRUE,legend.signif=1,map.cex=1)
# CVmap
surveyIdxPlots(tw1,dd,myids=NULL,predD=grid.df,select="CVmap",year=years,colors=cm.colors(6),par=list(mfrow=n2mfrow(nlevels(dd$Year)),mar=c(0,0,2,0),cex=0.6),legend=TRUE,legend.signif=2,map.cex=1,cutp=c(0,0.2,0.4,0.6,1,2,Inf))

# Index - model 4
par(cex=1)
surveyIdxPlots(tw1,dd,select="index",par=list(mfrow=c(1,1),mar=c(4,3,3,3)),main="MIK+DRS Index - model1")


#########
# model 11
surveyIdxPlots(tw11,dd,myids=NULL,predD=grid.df,select="absolutemap",year=years,colors=rev(heat.colors(6)),par=list(mfrow=n2mfrow(nlevels(dd$Year)),mar=c(0,0,2,0),cex=0.6),legend=TRUE,legend.signif=1,map.cex=1)
# CVmap
surveyIdxPlots(tw11,dd,myids=NULL,predD=grid.df,select="CVmap",year=years,colors=cm.colors(6),par=list(mfrow=n2mfrow(nlevels(dd$Year)),mar=c(0,0,2,0),cex=0.6),legend=TRUE,legend.signif=2,map.cex=1,cutp=c(0,0.2,0.4,0.6,1,2,Inf))

# Index - model 11
par(cex=1)
surveyIdxPlots(tw11,dd,select="index",par=list(mfrow=c(1,1),mar=c(4,3,3,3)),main="MIK+DRS Index - model11")


###############################
#    modelling - 2018-2023    #
###############################
# tweedie - CPUE
years = seq(2018, 2023)
dd_recent <- subset(dd, Year %in% years)
table(dd_recent$Survey)

plot(dd_recent$Year, dd_recent$CPUE)

# 
model1_recent = "Year + Country + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + Survey"
system.time(tw1_recent <- getSurveyIdx(dd_recent, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model1_recent, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw1_recent = AIC.surveyIdx(tw1_recent) # -18950
qqnorm(residuals(tw1_recent), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw1_recent))); abline(0,1)

# remove Country
model2_recent = "Year + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + Survey"
system.time(tw2_recent <- getSurveyIdx(dd_recent, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model2_recent, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw2_recent = AIC.surveyIdx(tw2_recent) # -18929, no better 
qqnorm(residuals(tw2_recent), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw2_recent))); abline(0,1)

# remove DayNight
model3_recent = "Year + Country + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + Survey"
system.time(tw3_recent <- getSurveyIdx(dd_recent, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model3_recent, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw3_recent = AIC.surveyIdx(tw3_recent) # -18898, not good
qqnorm(residuals(tw3_recent), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw3_recent))); abline(0,1)

# remove ShipG
model4_recent = "Year + Country + DayNight + s(lon, lat, bs = 'ds', m = c(1, 0.5), k = 40) + s(lon, lat, bs = 'ds', m = c(1, 0.5), by = Year, k = 5, id = 1) + s(Depth, bs = 'cr') + Survey"
system.time(tw4_recent <- getSurveyIdx(dd_recent, ages = 1, predD = grid.df,
                                fam = "Tweedie", modelP = model4_recent, gamma = 1,
                                cutOff = 0.00000001, control = list(trace = TRUE, maxit = 20)))
aic.tw4_recent = AIC.surveyIdx(tw4_recent) # -18914, not good
qqnorm(residuals(tw4_recent), main = paste0("Tweedie, DRS+MIK AIC = ", round(aic.tw4_recent))); abline(0,1)
# there are warnings when there is ShipG in the model
# what does the warning mean?

## Look into the data
summary(dd_recent)
summary(dd_recent$SweepLngt)
hist(dd_recent$SweepLngt)
summary(dd_recent$CPUE)
plot(dd_recent$CPUE, dd_recent$lon)
plot(dd_recent$Abundance, dd_recent$lon)
plot(dd_recent$Abundance, dd_recent$lat)

################
# model 4
surveyIdxPlots(tw4_recent,dd_recent,myids=NULL,predD=grid.df,select="absolutemap",year=years,colors=rev(heat.colors(6)),par=list(mfrow=n2mfrow(nlevels(dd_recent$Year)),mar=c(0,0,2,0),cex=0.6),legend=TRUE,legend.signif=1,map.cex=1)
# CVmap
surveyIdxPlots(tw4_recent,dd_recent,myids=NULL,predD=grid.df,select="CVmap",year=years,colors=cm.colors(6),par=list(mfrow=n2mfrow(nlevels(dd_recent$Year)),mar=c(0,0,2,0),cex=0.6),legend=TRUE,legend.signif=2,map.cex=1,cutp=c(0,0.2,0.4,0.6,1,2,Inf))

# Index - model 4
par(cex=1)
surveyIdxPlots(tw4_recent,dd_recent,select="index",par=list(mfrow=c(1,1),mar=c(4,3,3,3)),main="Index - model4")

res <- list(mod1 = tw1_recent,
            mod2 = tw2_recent,
            mod3 = tw3_recent,
            mod4 = tw4_recent)
save(res, file = "2018_2023_MIKandDRS_tweedieCPUE.RData")
