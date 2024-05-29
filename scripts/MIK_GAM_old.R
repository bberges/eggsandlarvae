#path1 <- "C:/Users/chin008/OneDrive - Wageningen University & Research/git/harring_eggsandlarvae/"
path1 <- "C:/git/harring_eggsandlarvae/"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)
library(tidyr)
library(sp)
library(sf)

MIK2DATRAS_eggsLarvae <- readICES(file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'), strict = TRUE)
MIK2DATRAS_eggsLarvae  <- addSpatialData(MIK2DATRAS_eggsLarvae,"./shapefiles/ICES_areas.shp")

MIK2DATRAS_eggsLarvae  <- subset(MIK2DATRAS_eggsLarvae,
                                 ICES_area %in% as.character(c("IVa","IVb","IVc")))

MIK2DATRAS_eggsLarvae[[3]]$Count <- MIK2DATRAS_eggsLarvae[[3]]$TotalNo * MIK2DATRAS_eggsLarvae[[3]]$SubFactor

# make a new variable, combine Ship and Gear
MIK2DATRAS_eggsLarvae$ShipG = factor(paste(MIK2DATRAS_eggsLarvae$Ship,
                                           MIK2DATRAS_eggsLarvae$Gear, sep = ":"))
table(MIK2DATRAS_eggsLarvae$ShipG)
MIK2DATRAS_eggsLarvae$dum = 1 

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

dd.mik$Abundance <- rowSums(dd.mik$NL[, 1:ncol(dd.mik$NL)])
dd.mik$Nage <- matrix(dd.mik$Abundance, ncol = 1)
colnames(dd.mik$Nage) <- "1"

## Remove levels of Gear, ShipG, StatRec with only zero observations
dd <- removeZeroClusters(dd.mik, response="Abundance", factors=c("Gear","Ship"))
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

#################
#   modelling   #
#################
## tweedie
model1 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=20) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + s(Ship, bs = 're') + Gear + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(TW1 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model1, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw1 = AIC.surveyIdx(TW1) # 76115
qqnorm(residuals(TW1), main = paste0("Tweedie, MIK AIC = ", round(aic.tw1))); abline(0,1)


model2 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=80) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + s(Ship, bs = 're') + Gear + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(TW2 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model2, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw2 = AIC.surveyIdx(TW2) # 75636
qqnorm(residuals(TW2), main = paste0("Tweedie, MIK AIC = ", round(aic.tw2))); abline(0,1)

# remove ship 
model3 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=80) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + Gear + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(TW3 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model3, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw3 = AIC.surveyIdx(TW3) # 75762, not a good idea
qqnorm(residuals(TW3), main = paste0("Tweedie, MIK AIC = ", round(aic.tw3))); abline(0,1)

# use ShipG, remove Depth
model4 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=80) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + Gear + s(ShipG, bs = 're') + offset(log(SweepLngt))"
system.time(TW4 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model4, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw4 = AIC.surveyIdx(TW4) # 75674
qqnorm(residuals(TW4), main = paste0("Tweedie, MIK AIC = ", round(aic.tw4))); abline(0,1)

# 
model5 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=80) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + Gear + s(ShipG, bs = 're') + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(TW5 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model5, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw5 = AIC.surveyIdx(TW5) # 75628
qqnorm(residuals(TW5), main = paste0("Tweedie, MIK AIC = ", round(aic.tw5))); abline(0,1)

#
model6 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=80) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + s(ShipG, bs = 're') + Country + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(TW6 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model6, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw6 = AIC.surveyIdx(TW6) # 75589
qqnorm(residuals(TW6), main = paste0("Tweedie, MIK AIC = ", round(aic.tw6))); abline(0,1)

#
model7 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=80) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1)+ s(ShipG, bs = 're') + Country + s(Depth, bs = 'cr')"
system.time(TW7 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model7, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw7 = AIC.surveyIdx(TW7) # 75368
qqnorm(residuals(TW7), main = paste0("Tweedie, MIK AIC = ", round(aic.tw7))); abline(0,1)

#
model8 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=80) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1)+ s(ShipG, bs = 're')+ s(Depth, bs = 'cr')"
system.time(TW8 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model8, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw8 = AIC.surveyIdx(TW8) # 
qqnorm(residuals(TW8), main = paste0("Tweedie, MIK AIC = ", round(aic.tw8))); abline(0,1)

# Log-normal distribution
model = "Year + Country + s(sqrt(Depth),k=5,bs='ds',m=c(1,0)) + Gear + s(ShipG,bs='re',by=dum) + s(lon,lat,bs='ds',m=c(1,0.5),k=40) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + offset(log(HaulDur))"
modelZ = "Year + Country + s(sqrt(Depth),k=5,bs='ds',m=c(1,0)) + Gear + s(ShipG,bs='re',by=dum) + s(lon,lat,bs='ds',m=c(1,0.5),k=40) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + offset(log(HaulDur))"
system.time( SI.dln1 <- getSurveyIdx(dd,ages=1,predD=grid.df,fam="LogNormal",modelP=model,modelZ=modelZ,gamma=1,cutOff=0.1,control=list(trace=TRUE,maxit=20)))
aic.sidln1 = AIC.surveyIdx(SI.dln1) # 51082.97
qqnorm(residuals(SI.dln1), main=paste0("Delta-Log Normal, AIC = ",round(aic.sidln1,1))); abline(0,1)


######################

load(paste0(getwd(),"/model/MIK_models.RData"))

MIK.GAM.tw7 <- as.data.frame(TW7$idx)
colnames(MIK.GAM.tw7) <- c('idx')
MIK.GAM.tw7$year <- 1992:2023
MIK.GAM.tw7$lbnd <- TW7$lo
MIK.GAM.tw7$ubnd <- TW7$up
MIK.GAM.tw7$type <- 'GAM-tweedie'

MIK.GAM.log1 <- as.data.frame(SI.dln1$idx)
colnames(MIK.GAM.log1) <- c('idx')
MIK.GAM.log1$year <- 1992:2023
MIK.GAM.log1$lbnd <- SI.dln1$lo
MIK.GAM.log1$ubnd <- SI.dln1$up
MIK.GAM.log1$type <- 'GAM-delta log normal'

MIK.base <- read.csv(file = file.path('./review/new_plots/data','survey_ibts_0.csv'),check.names=FALSE)
colnames(MIK.base) <- c('year','idx')
MIK.base$lbnd <- NA
MIK.base$ubnd <- NA
MIK.base$type <- 'base'

MIK.all <- rbind(MIK.base, MIK.GAM.tw6, MIK.GAM.log1)

MIK.all <- MIK.all %>% group_by(type) %>% mutate(idx.norm = (idx-mean(idx))/sd(idx)) 

ggplot(MIK.all, aes(x=year,y=idx.norm,col=type))+
  geom_line()

####### base VS tweedie
MIK.both <- rbind(MIK.base, MIK.GAM.tw7)

MIK.both <- MIK.both %>% group_by(type) %>% mutate(idx.norm = (idx-mean(idx))/sd(idx)) 

ggplot(MIK.both, aes(x=year,y=idx.norm,col=type))+
  geom_line()

####### base VS delta-log normal GAM
MIK.both1 <- rbind(MIK.base, MIK.GAM.log1)

MIK.both1 <- MIK.both1 %>% group_by(type) %>% mutate(idx.norm = (idx-mean(idx))/sd(idx)) 

ggplot(MIK.both1, aes(x=year,y=idx.norm,col=type))+
  geom_line()

