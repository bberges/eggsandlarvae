rm(list=(ls()))

path1 <- "//wurnet.nl/dfs-root/IMARES/IJmuiden/WOT/WOT surveys Zout - Haringlarven/Down's recruitment survey/New index calculation/"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)
library(tidyr)

# load MIK data
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
view(dd.mik[[2]])

dd.mik$Abundance <- rowSums(dd.mik$NL[, 1:ncol(dd.mik$NL)])
dd.mik$Nage <- matrix(dd.mik$Abundance, ncol = 1)
colnames(dd.mik$Nage) <- "1"

## Remove levels of Gear, ShipG, StatRec with only zero observations
dd <- removeZeroClusters(dd.mik, response="Abundance", factors=c("Gear","Ship"))
dd <- subset(dd,!is.na(Depth))

simplegrid = getGrid(dd, nLon = 100)
plot(simplegrid)

# grid as data.frame
grid.df = subset(dd, haul.id %in% simplegrid[[3]])[[2]]

table(dd[[2]]$Ship)
table(dd[[2]]$Gear)
str(dd[[2]])
summary(dd[[1]])
summary(dd[[2]])
dd[[1]]$Age <- 0
str(dd[[2]])
table(dd[[1]]$Year)

## model - tweedie
model1 = "Year + s(lon,lat,bs='ds', m=c(1,0.5), k=20) + s(lon,lat,bs='ds',m=c(1,0.5),by=Year,k=5,id=1) + s(Ship, bs = 're') + Gear + s(Depth, bs = 'cr') + offset(log(SweepLngt))"
system.time(TW1 <- getSurveyIdx(dd, ages = 1, predD = grid.df, 
                                fam = "Tweedie", modelP = model1, gamma = 1, 
                                cutOff = 0.1, control = list(trace = TRUE, maxit = 20)))
aic.tw1 = AIC.surveyIdx(TW1) # 2489.16
qqnorm(residuals(TW1), main = paste0("Tweedie, MIK AIC = ", round(aic.tw1))); abline(0,1)

MIK.GAM <- as.data.frame(TW1$idx)
colnames(MIK.GAM) <- c('idx')
MIK.GAM$year <- 1992:2023
MIK.GAM$lbnd <- TW1$lo
MIK.GAM$ubnd <- TW1$up
MIK.GAM$type <- 'GAM'

MIK.base <- read.csv(file = file.path('./review/new_plots/data','survey_ibts_0.csv'),check.names=FALSE)
colnames(MIK.base) <- c('year','idx')
MIK.base$lbnd <- NA
MIK.base$ubnd <- NA
MIK.base$type <- 'base'

MIK.all <- rbind(MIK.base,MIK.GAM)

MIK.all <- MIK.all %>% group_by(type) %>% mutate(idx.norm = (idx-mean(idx))/sd(idx)) 

ggplot(MIK.all,aes(x=year,y=idx.norm,col=type))+
  geom_line()
