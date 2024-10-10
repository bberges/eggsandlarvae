rm(list=(ls()))

path1 <- "C:/git/eggsandlarvae/"
setwd(path1)

library(tidyverse)
library(icesDatras)
library(surveyIndex)
library(tidyr)
library(corrplot)
library(ggpubr)

figuresPath <- file.path('.','figures')

# ------------------------------------------------------------
# loading best models
# ------------------------------------------------------------

mod.sel <- 'MIK_LogNormalAbundance'
load(file.path('./results',paste0(mod.sel,'.RData')))
df.models.MIK <- read.csv(file.path('./results',paste0(mod.sel,'.csv')))
model.MIK <- resMIK[[df.models.MIK$model.name[1]]]

mod.sel <- 'DRS_LogNormalAbundance'
load(file.path('./results',paste0(mod.sel,'.RData')))
df.models.DRS <- read.csv(file.path('./results',paste0(mod.sel,'.csv')))
model.DRS <- resDRS[[df.models.DRS$model.name[1]]]

yearsModel <- rownames(model.MIK[[1]])

models.comp <- list(MIK=model.MIK,
                    DRS=model.DRS)

# ------------------------------------------------------------
# Creating tuning objects
# ------------------------------------------------------------

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
  
  NSH.tun[[paste0('model.',model.name)]] <- IBTS0.model
}

NSH.tun <- NSH.tun[grepl('IBTS0', names(NSH.tun)) | grepl('model', names(NSH.tun))]

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

# add MIK+DRS survey
MIK.idx <- model.MIK$idx
DRS.idx <- model.DRS$idx

MIK.idx[rownames(MIK.idx) %in% rownames(DRS.idx)] <- MIK.idx[rownames(MIK.idx) %in% rownames(DRS.idx)]+DRS.idx

dmns        <- dimnames(NSH.tun$IBTS0)
MIK.FL    <- FLQuant(array( MIK.idx,dim=c(length(dmns$age),
                                                length(dmns$year),
                                                1,
                                                1,
                                                1,
                                                1)), # iterations
                     dimnames=dmns)
MIK.FL <- FLIndex(index=MIK.FL)

NSH.tun[['model.MIK+DRS']] <- MIK.FL

# apply z-norm
for(surveyCurrent in names(NSH.tun)){
  # z-norm
  NSH.tun[[surveyCurrent]]@index <- sweep(sweep(NSH.tun[[surveyCurrent]]@index,1,yearMeans(NSH.tun[[surveyCurrent]]@index),'-'),1,apply(NSH.tun[[surveyCurrent]]@index,1,sd,na.rm=TRUE),'/')
}

# plot
df.plot <- as.data.frame(NSH.tun)
df.plot <- subset(df.plot,slot=='index')
df.plot$survey <- 'IBTS0'

df.plot$survey[df.plot$cname %in% c('DRS','IBTS0.DRS')] <- 'DRS'

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
