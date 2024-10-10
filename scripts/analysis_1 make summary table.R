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

# '2018_2023_DRS_tweedieCPUE'
#'MIK_tweedieCPUE'
#'MIKandDRS_tweedieCPUE'
#'2018_2023_MIKandDRS_tweedieCPUE'
load(file.path('./results','DRS_LogNormalAbundance.RData'))
load(file.path('./results','MIK_LogNormalAbundance.RData'))

load(file.path('./data','NSAS_HAWG2024_sf.RData'))

# ------------------------------------------------------------
# model table DRS
# ------------------------------------------------------------

models <- resDRS
mod.sel <- 'DRS_LogNormalAbundance'

flagFirst <- T
for(idx.model in names(models)){
  df.models <- data.frame(model.name=idx.model,
                          AIC=AIC.surveyIdx(models[[idx.model]]),
                          formula=as.character(models[[idx.model]]$pModels[[1]]$formula)[3])
  if(flagFirst){
    df.models.all <- df.models
    flagFirst<- F
  }else{
    df.models.all <- rbind(df.models.all,df.models)
  }
}

df.models.all <- df.models.all[order(df.models.all$AIC),]
df.models.all$names<- paste0('IBTS0.',df.models.all$model.name)

write.csv(df.models.all,file.path('./results',paste0(mod.sel,'.csv')),row.names = F)

# ------------------------------------------------------------
# model table MIK
# ------------------------------------------------------------

models <- resMIK
mod.sel <- 'MIK_LogNormalAbundance'

flagFirst <- T
for(idx.model in names(models)){
  df.models <- data.frame(model.name=idx.model,
                          AIC=AIC.surveyIdx(models[[idx.model]]),
                          formula=as.character(models[[idx.model]]$pModels[[1]]$formula)[3])
  if(flagFirst){
    df.models.all <- df.models
    flagFirst<- F
  }else{
    df.models.all <- rbind(df.models.all,df.models)
  }
}

df.models.all <- df.models.all[order(df.models.all$AIC),]
df.models.all$names<- paste0('IBTS0.',df.models.all$model.name)

write.csv(df.models.all,file.path('./results',paste0(mod.sel,'.csv')),row.names = F)

