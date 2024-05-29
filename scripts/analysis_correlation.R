# MIKandDRS_tweedieCPUE.RData 
# mod1-mod4: tweedie, cpue
# mod10-mod13: tweedie, abundance


rm(list=(ls()))

path1 <- "G:/git/eggsandlarvae/"
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
mod.sel <- 'MIKandDRS_tweedieCPUE'
load(file.path('./results',paste0(mod.sel,'.RData')))
models <- res

load(file.path('./data','NSAS_HAWG2024_sf.RData'))

# ------------------------------------------------------------
# model table
# ------------------------------------------------------------

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

yearsModel <- rownames(models[[1]]$idx)

# ------------------------------------------------------------
# extracting models and computing normalized indices
# ------------------------------------------------------------

NSH.tun <- window(NSH.tun,
                  start=min(an(yearsModel)),
                  end=max(an(yearsModel)))

for(model.name in names(models)){
  dmns        <- dimnames(NSH.tun$IBTS0)
  dmns$year <-  yearsModel
  IBTS0.model    <- FLQuant(array( models[[model.name]]$idx,dim=c(length(dmns$age),
                                                               length(dmns$year),
                                                               1,
                                                               1,
                                                               1,
                                                               1)), # iterations
                            dimnames=dmns)
  IBTS0.model <- FLIndex(index=IBTS0.model)
  
  NSH.tun[[paste0('IBTS0.',model.name)]] <- IBTS0.model
}

NSH.tun <- NSH.tun[!grepl('LAI', names(NSH.tun))]

surveyNames <- names(NSH.tun) # get all the survey names

NSH.cohort  <- new("FLCohorts")

NSH.tun[['HERAS']]@index[NSH.tun[['HERAS']]@index == -1] <- NA

for(surveyCurrent in surveyNames){
  # z-norm
  NSH.tun[[surveyCurrent]]@index <- sweep(sweep(NSH.tun[[surveyCurrent]]@index,1,yearMeans(NSH.tun[[surveyCurrent]]@index),'-'),1,apply(NSH.tun[[surveyCurrent]]@index,1,sd,na.rm=TRUE),'/')
  
  NSH.cohort[[surveyCurrent]] <- FLCohort(NSH.tun[[surveyCurrent]]@index)
}

NSH.cohort <- window(NSH.cohort,start=an(dimnames(NSH.cohort$IBTS0)$cohort[1]))

# --------------------------------------
# computing correlations
# --------------------------------------
surveyBaseMat <- names(NSH.cohort)[grepl('IBTS0', names(NSH.cohort))]
surveyCompMat    <- names(NSH.cohort)[!grepl('IBTS0', names(NSH.cohort))]

inter.corr <- 15

flagFirst <- T
for(surveyBase in surveyBaseMat){
  for(surveyComp in surveyCompMat){
    agesCurrent <- dimnames(NSH.cohort[[surveyComp]])$age
    
    for(idxAge in agesCurrent){
      ratio.df <- as.data.frame(NSH.cohort[[surveyBase]][,dimnames(NSH.cohort[[surveyComp]][ac(idxAge)])$cohort]/NSH.cohort[[surveyComp]][ac(idxAge)])
      ratio.df$age.comp <- idxAge
      ratio.df <- ratio.df %>% select(-c('unit','season','area','iter'))
      
      IBTS0.df  <- as.data.frame(NSH.cohort[[surveyBase]][,dimnames(NSH.cohort[[surveyComp]][ac(idxAge)])$cohort])
      IBTS0.df <- IBTS0.df %>% select(-c('unit','season','area','iter'))
      comp.df   <- as.data.frame(NSH.cohort[[surveyComp]][ac(idxAge)])
      comp.df$age.comp <- comp.df$age
      comp.df$data.comp <- comp.df$data
      comp.df <- comp.df %>% select(-c('unit','season','area','iter','age','data'))
      
      if(length(comp.df$data.comp[!is.na(comp.df$data.comp)]) > 4){
        df.temp <- left_join(IBTS0.df,comp.df,by=c('cohort'))
        
        if(length(yearsModel) > inter.corr){
          idx.corr <- data.frame( start=seq(1,dim(df.temp)[1]-inter.corr+1,by=1),
                                  end=seq(1,dim(df.temp)[1]-inter.corr+1,by=1)+inter.corr-1)
          
          flag.temp <- T
          for(idx.temp in 1:dim(idx.corr)[1]){
            test.var <- cor.test(df.temp$data[idx.corr$start[idx.temp]:idx.corr$end[idx.temp]],
                                 df.temp$data.comp[idx.corr$start[idx.temp]:idx.corr$end[idx.temp]],
                                 method = "pearson")
            
            # plot(df.temp$data[idx.corr$start[idx.temp]:idx.corr$end[idx.temp]],
            #      df.temp$data.comp[idx.corr$start[idx.temp]:idx.corr$end[idx.temp]])
            test.var <- as.data.frame(t(c(test.var$statistic,test.var$p.value,test.var$estimate)))
            colnames(test.var) <- c('t','p-value','cor')
            test.var$age.comp <- idxAge
            test.var$var.iter <- idx.temp
            test.var$dist <- as.vector(dist(rbind(df.temp$data[idx.corr$start[idx.temp]:idx.corr$end[idx.temp]],
                                                  df.temp$data.comp[idx.corr$start[idx.temp]:idx.corr$end[idx.temp]]), method = "euclidean"))
            test.var$yearStart  <- min(IBTS0.df$cohort[idx.corr$start[idx.temp]:idx.corr$end[idx.temp]])
            test.var$yearEnd    <- max(IBTS0.df$cohort[idx.corr$start[idx.temp]:idx.corr$end[idx.temp]])
            if(flag.temp){
              test.temp.var <- test.var
              flag.temp <- F
            }else{
              test.temp.var <- rbind(test.temp.var,test.var)
            }
          }
        }
        
        # windows()
        # ggplot(test.temp.var,aes(x=yearStart,y=cor))+
        #  geom_line()
        
        test.temp <- cor.test(df.temp$data, df.temp$data.comp, method = "pearson")
        test.temp <- as.data.frame(t(c(test.temp$statistic,test.temp$p.value,test.temp$estimate)))
        colnames(test.temp) <- c('t','p-value','cor')
        test.temp$age.comp <- idxAge
        test.temp$dist <- as.vector(dist(rbind(df.temp$data,df.temp$data.comp), method = "euclidean"))
        
        df.temp$surveyComp <- surveyComp
        df.temp$surveyBase <- surveyBase
        test.temp$surveyComp <- surveyComp
        test.temp$surveyBase <- surveyBase
        ratio.df$surveyComp <- surveyComp
        ratio.df$surveyBase <- surveyBase
        if(length(yearsModel) > inter.corr){
          test.temp.var$surveyComp <- surveyComp
          test.temp.var$surveyBase <- surveyBase
        }
        
        if(flagFirst){
          df.all <- df.temp
          test.all <- test.temp
          ratio.all <- ratio.df
          if(length(yearsModel) > inter.corr){
            test.temp.var.all <- test.temp.var
          }
          flagFirst <- F
        }else{
          df.all <- rbind(df.all,df.temp)
          test.all <- rbind(test.all,test.temp)
          ratio.all <- rbind(ratio.all,ratio.df)
          if(length(yearsModel) > inter.corr){
            test.temp.var.all <- rbind(test.temp.var.all,test.temp.var)
          }
        }
      }
    }
  } 
}

test.all <- test.all %>% pivot_longer(!age.comp & !surveyComp & !surveyBase,names_to = 'metric',values_to = 'data')

# --------------------------------------
# Comparing model time series
# --------------------------------------
df.plot <- subset(df.all,age.comp == '0')

p <- ggplot(df.plot,aes(x=cohort,y=data,col=surveyBase))+
      theme_bw()+
      geom_line()+
      ylab('z-norm')

ggsave(file.path(figuresPath,paste0(mod.sel,'_IBTS0 TS.png')),
       p,
       width = 170,
       height = 90,
       units = c("mm"),
       dpi = 300)

# --------------------------------------
# plotting model results
# --------------------------------------
df.plot <- subset(df.all,surveyBase %in% c('IBTS0',df.models.all$names[1]))
p <- ggplot(df.plot,aes(x=data,y=data.comp,col=as.factor(surveyBase)))+
      theme_bw()+
      geom_point()+
      geom_abline (slope=1, linetype = "dashed", color="Red")+
      geom_smooth(method='lm', formula= y~x)+
      coord_fixed()+
      #theme(legend.position = "none")+
      xlab('IBTS0 index')+
      ylab('Other index')+
      theme(legend.position="top")+
      guides(colour = guide_legend(nrow = 3))+
      stat_cor(method = "pearson")+
      facet_grid(surveyComp~age.comp)

ggsave(file.path(figuresPath,paste0(mod.sel,'_cor best.png')),
       p,
       width = 300,
       height = 170,
       units = c("mm"),
       dpi = 300)

p <- ggplot(subset(test.all,metric == 'cor'),aes(x=as.numeric(age.comp),y=data,col=as.factor(surveyBase)))+
      theme_bw()+
      geom_point()+
      geom_line()+
      theme(legend.position="top")+
      guides(colour = guide_legend(nrow = 3))+
      facet_wrap(~surveyComp,scales='free',ncol = 1)

ggsave(file.path(figuresPath,paste0(mod.sel,'_cor.png')),
       p,
       width = 170,
       height = 170,
       units = c("mm"),
       dpi = 300)

p <- ggplot(subset(test.all,metric == 'dist'),aes(x=as.numeric(age.comp),y=data,col=as.factor(surveyBase)))+
      theme_bw()+
      geom_point()+
      geom_line()+
      theme(legend.position="top")+
      guides(colour = guide_legend(nrow = 3))+
      facet_wrap(~surveyComp,scales='free',ncol = 1)

ggsave(file.path(figuresPath,paste0(mod.sel,'_dist.png')),
       p,
       width = 170,
       height = 170,
       units = c("mm"),
       dpi = 300)

# --------------------------------------
# plotting time series comparison
# --------------------------------------

df.plot <- subset(df.all,surveyBase %in% c('IBTS0',df.models.all$names[1]))
df.temp <- subset(df.plot,surveyComp == 'IBTS-Q3' & age.comp == '0')
df.temp$surveyComp <- df.temp$surveyBase
age.repeat <- rep(unique(df.plot$age.comp), each=dim(df.temp)[1])
df.temp <- do.call("rbind", replicate(length(unique(df.plot$age.comp)),
                                      df.temp, simplify = FALSE))
df.temp$age.comp <- age.repeat
df.plot$data <- df.plot$data.comp
df.plot <- rbind(df.plot,df.temp)
df.plot <- df.plot %>% select(-c('surveyBase','data.comp','age'))

p <- ggplot(df.plot,aes(x=cohort,y=data,col=surveyComp))+
      theme_bw()+
      ylab('z-norm')+
      geom_line()+
      theme(legend.position="top")+
      facet_wrap(~age.comp,ncol = 1)

ggsave(file.path(figuresPath,paste0(mod.sel,'_TS.png')),
       p,
       width = 170,
       height = 300,
       units = c("mm"),
       dpi = 300)

# --------------------------------------
# plotting correlation over time
# --------------------------------------
if(length(yearsModel) > inter.corr){
  df.plot <- subset(test.temp.var.all,surveyBase %in% c('IBTS0',df.models.all$names[1]))
  
  p <- ggplot()+
    theme_bw()+
    geom_point(data=subset(df.plot),
               aes(x = yearStart, y = cor, colour = as.factor(surveyBase)))+
    theme(legend.position="top")+
    facet_grid(age.comp~surveyComp)+
    ylim(0,1)
  
  ggsave(file.path(figuresPath,paste0(mod.sel,'_cor over time.png')),
         p,
         width = 170,
         height = 170,
         units = c("mm"),
         dpi = 300)
  
  p <- ggplot()+
    theme_bw()+
    geom_point(data=subset(df.plot),
               aes(x = yearStart, y = dist, colour = as.factor(surveyBase)))+
    theme(legend.position="top")+
    facet_grid(age.comp~surveyComp)
  
  ggsave(file.path(figuresPath,paste0(mod.sel,'_dist over time.png')),
         p,
         width = 170,
         height = 170,
         units = c("mm"),
         dpi = 300)
}
# --------------------------------------
# plotting correlation over time
# --------------------------------------
if(length(yearsModel) > inter.corr){
  df.plot <- subset(test.temp.var.all,surveyBase %in% c('IBTS0',df.models.all$names[1]))
  
  components.df <- as.data.frame(components(NSH.sam))
  components.df$components <- rownames(components.df)
  components.df <- components.df %>% pivot_longer(!components,names_to = "year",values_to='Fraction')
  
  p <- ggplot()+
    theme_bw()+
    geom_line(data=subset(components.df,components == 'LAI-SNS'),aes(x=an(year),y=Fraction))+
    geom_segment(data=subset(df.plot,surveyBase == 'IBTS0'),
                 aes(x = yearStart, y = cor, xend = yearEnd, yend = cor, colour = as.factor(var.iter)))+
    facet_grid(age.comp~surveyComp)+
    theme(legend.position = "none")
  
  ggsave(file.path(figuresPath,paste0(mod.sel,'_cor and components.png')),
         p,
         width = 170,
         height = 170,
         units = c("mm"),
         dpi = 300)
}



