#'MIK_tweedieCPUE'
#'MIKandDRS_tweedieCPUE'
#'2018_2023_MIKandDRS_tweedieCPUE'

rm(list=(ls()))

path1 <- "C:/git/eggsandlarvae/"
setwd(path1)

library(icesTAF)
library(FLSAM)
library(FLCore)

source(file.path('./scripts','utilities_model_config.R'))

figuresPath <- file.path('.','figures')

#'MIK_models'
# model 9 lognormal, all others TWD distribution 
#'MIK_tweedieCPUE'
#'MIKandDRS_tweedieCPUE'
# mod1-mod4: tweedie, cpue
# mod10-mod13: tweedie, abundance
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

yearsModel <- rownames(models[[1]]$idx)
# ------------------------------------------------------------
# Assessment fit
# ------------------------------------------------------------

NSH.sams  <- new("FLSAMs")

NSH.tun <- window(NSH.tun,
                  end=max(an(yearsModel)))
NSH.tun.init <- NSH.tun

pg <- range(NSH)[2]

NSH.ctrl <- config_sf_IBPNSherring2021(NSH,NSH.tun.init,pg)
NSH.ctrl@residuals <- F

NSH.sams[['base']]     <- FLSAM(NSH, NSH.tun, NSH.ctrl)

for(idx.model in df.models.all$model.name){
  NSH.tun$IBTS0@index[1,] <- as.vector(models[[idx.model]]$idx)
  
  NSH.ctrl <- config_sf_IBPNSherring2021(NSH,NSH.tun.init,pg)
  NSH.ctrl@residuals <- F
  
  NSH.sams[[idx.model]]     <- FLSAM(NSH, NSH.tun, NSH.ctrl)
}

obs.var <- obs.var(NSH.sams)


p <- ggplot(subset(obs.var,fleet == 'IBTS0')) +
      geom_bar( aes(x=name, y=value), stat="identity", fill="skyblue", alpha=0.7)+
      geom_errorbar( aes(x=name, ymin=lbnd, ymax=ubnd), colour="orange", alpha=0.9, size=1.3)

ggsave(file.path(figuresPath,paste0(mod.sel,'_obs var SAM.png')),
       p,
       width = 170,
       height = 170,
       units = c("mm"),
       dpi = 300)
