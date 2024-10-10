# MIKandDRS_tweedieCPUE.RData 
# mod1-mod4: tweedie, cpue
# mod10-mod13: tweedie, abundance


rm(list=(ls()))

path1 <- "C:/git/eggsandlarvae/"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)
library(tidyr)
library(corrplot)
library(ggpubr)

figuresPath <- file.path('.','figures')

# ------------------------------------------------------------
# model table
# ------------------------------------------------------------

mod.sel <- 'DRS_LogNormalAbundance'
load(file.path('./results',paste0(mod.sel,'.RData')))
df.models.DRS <- read.csv(file.path('./results',paste0(mod.sel,'.csv')))

mod.sel <- 'MIK_LogNormalAbundance'
load(file.path('./results',paste0(mod.sel,'.RData')))
df.models.MIK <- read.csv(file.path('./results',paste0(mod.sel,'.csv')))

models <- resMIK

load(file.path('./data','NSAS_HAWG2024_sf.RData'))

yearsModel <- rownames(models[[1]]$idx)

# combining MIK+DRS
MIK.idx <- resMIK[[df.models.MIK$model.name[1]]]$idx
DRS.idx <- resDRS[[df.models.DRS$model.name[1]]]$idx

MIK.idx[rownames(MIK.idx) %in% rownames(DRS.idx)] <- MIK.idx[rownames(MIK.idx) %in% rownames(DRS.idx)]+DRS.idx

MIK.idx <- MIK.idx[rownames(MIK.idx) %in% yearsModel]

dmns        <- dimnames(NSH.tun$IBTS0)
dmns$year <-  yearsModel
MIK.FL    <- FLQuant(array( MIK.idx,dim=c(length(dmns$age),
                                          length(dmns$year),
                                          1,
                                          1,
                                          1,
                                          1)), # iterations
                     dimnames=dmns)
MIK.FL <- FLIndex(index=MIK.FL)

# ------------------------------------------------------------
# extracting models and computing normalized indices
# ------------------------------------------------------------

NSH.tun <- window(NSH.tun,
                  start=min(an(yearsModel)),
                  end=max(an(yearsModel)))

NSH.tun <- NSH.tun[!grepl('LAI', names(NSH.tun))]
NSH.tun <- NSH.tun[!grepl('HERAS', names(NSH.tun))]

NSH.tun[['IBTS0.MIK+DRS']] <- MIK.FL

surveyNames <- names(NSH.tun) # get all the survey names

NSH.cohort  <- new("FLCohorts")

for(surveyCurrent in surveyNames){
  # z-norm
  #NSH.tun[[surveyCurrent]]@index <- sweep(sweep(NSH.tun[[surveyCurrent]]@index,1,yearMeans(NSH.tun[[surveyCurrent]]@index),'-'),1,apply(NSH.tun[[surveyCurrent]]@index,1,sd,na.rm=TRUE),'/')
  
  NSH.cohort[[surveyCurrent]] <- FLCohort(NSH.tun[[surveyCurrent]]@index)
}

NSH.cohort <- window(NSH.cohort,
                  start=2018,
                  end=2023)


IBTSQ1 <- as.data.frame(NSH.cohort[['IBTS-Q1']]) %>% select(c('cohort','data'))
#IBTSQ3 <- as.data.frame(NSH.cohort[['IBTS-Q3']]) %>% select(c('cohort','data'))
IBTS0.MIKDRS <- as.data.frame(NSH.cohort[['IBTS0.MIK+DRS']]) %>% select(c('cohort','data'))
IBTS0.MIKDRS <- IBTS0.MIKDRS %>% rename(data.MIKDRS=data)
IBTS0 <- as.data.frame(NSH.cohort[['IBTS0']]) %>% select(c('cohort','data'))
IBTS0 <- IBTS0 %>% rename(data.MIK=data)

all.df <- left_join(IBTSQ1,IBTS0.MIKDRS,by=c('cohort'))
all.df <- left_join(all.df,IBTS0,by=c('cohort'))
all.df <- all.df %>% pivot_longer(!cohort & !data,names_to = 'age0.idx')


windows()
ggplot(all.df,aes(x=data,y=value,col=age0.idx))+
  geom_point()+
  geom_smooth(method = lm)


