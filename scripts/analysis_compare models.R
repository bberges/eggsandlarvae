# MIKandDRS_tweedieCPUE.RData 
# mod1-mod4: tweedie, cpue
# mod10-mod13: tweedie, abundance


rm(list=(ls()))

path1 <- "G:/git/eggsandlarvae/"
setwd(path1)

library(tidyverse)
library(icesDatras)
library(surveyIndex)
library(tidyr)
library(corrplot)
library(ggpubr)

figuresPath <- file.path('.','figures')

# ------------------------------------------------------------
# compare best models
# ------------------------------------------------------------

#'MIK_tweedieCPUE'
#'MIKandDRS_tweedieCPUE'
#'2018_2023_MIKandDRS_tweedieCPUE'
mod.sel <- '2018_2023_MIKandDRS_tweedieCPUE'
load(file.path('./results',paste0(mod.sel,'.RData')))
df.models.all <- read.csv(file.path('./results',paste0(mod.sel,'.csv')))
model.MIKandDRS_2018_2023 <- res[[df.models.all$model.name[1]]]

mod.sel <- 'MIK_tweedieCPUE'
load(file.path('./results',paste0(mod.sel,'.RData')))
df.models.all <- read.csv(file.path('./results',paste0(mod.sel,'.csv')))
model.MIK <- res[[df.models.all$model.name[1]]]

mod.sel <- 'MIKandDRS_tweedieCPUE'
load(file.path('./results',paste0(mod.sel,'.RData')))
df.models.all <- read.csv(file.path('./results',paste0(mod.sel,'.csv')))
model.MIKandDRS <- res[[df.models.all$model.name[1]]]

mod.sel <- '2018_2023_DRS_tweedieCPUE'
load(file.path('./results',paste0(mod.sel,'.RData')))
df.models.all <- read.csv(file.path('./results',paste0(mod.sel,'.csv')))
model.DRS <- res[[df.models.all$model.name[1]]]

yearsModel <- rownames(model.MIKandDRS[[1]])

models.comp <- list(MIKandDRS=model.MIKandDRS,
                    MIK=model.MIK,
                    MIKandDRS_2018_2023=model.MIKandDRS_2018_2023,
                    DRS_2018_2023=model.DRS)

load(file.path('./data','NSAS_HAWG2024_sf.RData'))
DRS.idx <- read.csv(file.path('./data','DRS.csv'))

NSH.tun <- window(NSH.tun,
                  start=min(an(yearsModel)),
                  end=max(an(yearsModel)))

for(model.name in names(models.comp)){
  dmns        <- dimnames(NSH.tun$IBTS0)
  dmns$year <-  rownames(models.comp[[model.name]]$idx)
  IBTS0.model    <- FLQuant(array( models.comp[[model.name]]$idx,dim=c(length(dmns$age),
                                                                  length(dmns$year),
                                                                  1,
                                                                  1,
                                                                  1,
                                                                  1)), # iterations
                            dimnames=dmns)
  IBTS0.model <- FLIndex(index=IBTS0.model)
  
  NSH.tun[[paste0('IBTS0.',model.name)]] <- IBTS0.model
}

NSH.tun <- NSH.tun[grepl('IBTS0', names(NSH.tun))]

# add DRS survey
dmns        <- dimnames(NSH.tun$IBTS0)
dmns$year <-  ac(DRS.idx$year)
DRS.FL    <- FLQuant(array( DRS.idx$index,dim=c(length(dmns$age),
                                                   length(dmns$year),
                                                   1,
                                                   1,
                                                   1,
                                                   1)), # iterations
                          dimnames=dmns)
DRS.FL <- FLIndex(index=DRS.FL)

NSH.tun[['DRS']] <- DRS.FL

# apply z-norm
for(surveyCurrent in names(NSH.tun)){
  # z-norm
  NSH.tun[[surveyCurrent]]@index <- sweep(sweep(NSH.tun[[surveyCurrent]]@index,1,yearMeans(NSH.tun[[surveyCurrent]]@index),'-'),1,apply(NSH.tun[[surveyCurrent]]@index,1,sd,na.rm=TRUE),'/')
}

# plot
df.plot <- as.data.frame(NSH.tun)
df.plot$cname[df.plot$cname == 'IBTS0.DRS_2018_2023'] <- 'DRS_mod'
df.plot <- subset(df.plot,slot=='index')
df.plot$survey <- 'IBTS0'
df.plot$survey[df.plot$cname == 'DRS'] <- 'DRS'
df.plot$survey[df.plot$cname == 'IBTS0.MIKandDRS_2018_2023'] <- 'DRS'
df.plot$survey[df.plot$cname == 'DRS_mod'] <- 'DRS'

temp <- subset(df.plot[df.plot$cname == 'IBTS0.MIKandDRS',],year >= min(DRS.idx$year))
temp$survey <- 'DRS'
temp2 <- subset(df.plot[df.plot$cname == 'IBTS0',],year >= min(DRS.idx$year))
temp2$survey <- 'DRS'
df.plot <- rbind(df.plot,temp,temp2)



p <- ggplot(df.plot,aes(x=year,y=data,col=cname))+
      theme_bw()+
      theme(legend.position="top")+
      ylab('z-norm')+
      geom_line()+
      geom_point()+
      facet_wrap(~survey,scales = 'free_x',ncol = 1)

ggsave(file.path(figuresPath,paste0('model_comparison TS.png')),
       p,
       width = 170,
       height = 170,
       units = c("mm"),
       dpi = 300)
