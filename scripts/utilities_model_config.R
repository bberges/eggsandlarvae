config_sf_IBPNSherring2021 <- function(  NSH,
                                      NSH.tun,
                                      pg){
  NSH.ctrl                                    <- FLSAM.control(NSH,NSH.tun)
  
  catchRow                                    <- grep("catch",rownames(NSH.ctrl@f.vars))
  laiRow                                      <- grep("LAI",rownames(NSH.ctrl@obs.vars))
  
  #Variance in N random walks (set 1st one free is usually best)
  NSH.ctrl@logN.vars[]                        <- c(1,rep(2,length(1:pg)))
  
  #All fishing mortality states are free except
  #oldest ages to ensure stablity
  NSH.ctrl@states[catchRow,]                  <- seq(dim(NSH)[1])
  NSH.ctrl@states[catchRow,ac(7:pg)]           <- 101
  
  #Group observation variances of catches to ensure stability
  NSH.ctrl@obs.vars[catchRow,]                <- c(rep(0,2),rep(1,5),rep(2,length(7:pg)))#c(0,0,1,1,1,1,rep(2,length(6:pg)))
  NSH.ctrl@obs.vars["HERAS",ac(1:pg)]         <- c(101,102,103,rep(104,3),rep(105,length(7:pg)))
  NSH.ctrl@obs.vars["IBTS-Q1",ac(1)]          <- 201
  NSH.ctrl@obs.vars["IBTS0",ac(0)]            <- 301
  NSH.ctrl@obs.vars["IBTS-Q3",ac(0:5)]        <- c(rep(400,2),rep(401,4))#c(400,401,rep(402,4))
  NSH.ctrl@obs.vars[laiRow,1]                 <- 501
  
  #Catchabilities of the surveys. Set LAI all to 1 value, rest can be varied
  NSH.ctrl@catchabilities["HERAS",ac(1:pg)]    <- c(rep(100,2),rep(101,length(3:pg)))
  NSH.ctrl@catchabilities["IBTS-Q1",ac(1)]    <- c(201)
  NSH.ctrl@catchabilities["IBTS-Q3",ac(0:5)]  <- 300:305#c(300,rep(301,3),302,303)
  NSH.ctrl@catchabilities[laiRow,1]           <- 401
  
  #Add correlation correction for Q3 survey, not for HERAS
  idx                                       <- which(rownames(NSH.ctrl@cor.obs)=="IBTS-Q3")
  NSH.ctrl@cor.obs[idx,1:5]                   <- c(rep(101,5))
  NSH.ctrl@cor.obs.Flag[idx]                  <- as.factor("AR")
  
  #Variance of F random walks
  NSH.ctrl@f.vars[1,]                         <- c(101,101,rep(102,4),rep(103,length(6:pg)))
  #correlation structure among ages in F random walks
  NSH.ctrl@cor.F                              <- 2
  #Finalise
  NSH.ctrl@name                               <- "North Sea Herring"
  NSH.ctrl                                    <- update(NSH.ctrl)
  
  NSH.ctrl@residuals <- FALSE
  
  return(NSH.ctrl)
}

config_mf_IBPNSherring2021 <- function( NSHs3,
                                        NSH.tun,
                                        pg){
  NSH3.ctrl                         <- FLSAM.control(NSHs3,NSH.tun,sumFleets=dimnames(NSHs3[["residual"]]@catch)$area)
  
  catchRow                          <- grep("catch",rownames(NSH3.ctrl@f.vars))
  laiRow                            <- grep("LAI",rownames(NSH3.ctrl@obs.vars))
  
  #Variance in N random walks (set 1st one free is usually best)
  NSH3.ctrl@logN.vars[]               <- c(1,rep(2,length(1:pg)))
  
  # fishing mortality states
  NSH3.ctrl@states["catch A",]        <- c(-1,0:6,6)
  NSH3.ctrl@states["catch BD",]       <- c(7:9,10,10,10,rep(-1,3))
  NSH3.ctrl@states["catch C",]        <- c(-1,11:13,14,14,14,rep(-1,2))
  
  # obs.var
  NSH3.ctrl@obs.vars["catch A",]        <- c(-1,0,rep(1,5),rep(2,length(7:pg)))
  NSH3.ctrl@obs.vars["catch BD",]       <- c(101,102,rep(103,4),rep(-1,3))
  NSH3.ctrl@obs.vars["catch C",]        <- c(-1,201,202,rep(203,4),rep(-1,2))
  NSH3.ctrl@obs.vars["HERAS",ac(1:pg)]  <- c(301,302,303,rep(304,3),rep(305,length(7:pg)))
  NSH3.ctrl@obs.vars["IBTS-Q1",ac(1)]   <- 401
  NSH3.ctrl@obs.vars["IBTS0",ac(0)]     <- 501
  NSH3.ctrl@obs.vars["IBTS-Q3",ac(0:5)] <- c(rep(601,2),rep(602,4))#c(400,401,rep(402,4))
  NSH3.ctrl@obs.vars[laiRow,1]          <- 701#:604
  
  # catchabilities
  NSH3.ctrl@catchabilities["HERAS",ac(1:pg)]  <- c(rep(100,2),rep(101,length(3:pg)))
  NSH3.ctrl@catchabilities["IBTS-Q1",ac(1)]    <- c(201)
  NSH3.ctrl@catchabilities["IBTS-Q3",ac(0:5)] <- 300:305#c(300,rep(301,3),302,303)
  NSH3.ctrl@catchabilities[laiRow,1]          <- 401
  
  #Add correlation correction for Q3 survey, not for HERAS
  idx                                 <- which(rownames(NSH3.ctrl@cor.obs)=="IBTS-Q3")
  NSH3.ctrl@cor.obs[idx,1:5]          <- c(rep(101,5))
  NSH3.ctrl@cor.obs.Flag[idx]         <- as.factor("AR")
  
  #correlation structure among ages in F random walks
  NSH3.ctrl@cor.F                     <- as.integer(c(2,2,2))
  
  # f.vars
  NSH3.ctrl@f.vars["catch A",]        <- c(-1,101,rep(102,4),rep(103,3))
  NSH3.ctrl@f.vars["catch BD",]       <- c(202,203,rep(203,4),rep(-1,3))
  NSH3.ctrl@f.vars["catch C",]        <- c(-1,301,302,rep(303,4),rep(-1,2))
  
  # tidying up
  NSH3.ctrl@name                      <- "North Sea herring multifleet"
  NSH3.ctrl                           <- update(NSH3.ctrl)
  
  NSH3.ctrl@residuals <- FALSE
  
  return(NSH3.ctrl)
}

config_mf_WKPELA2018 <- function( NSHs3,
                                  NSH.tun){
  
  NSH3.ctrl                         <- FLSAM.control(NSHs3,NSH.tun,sumFleets=dimnames(NSHs3[["residual"]]@catch)$area)
  
  catchRow                          <- grep("catch",rownames(NSH3.ctrl@f.vars))
  laiRow                            <- grep("LAI",rownames(NSH3.ctrl@obs.vars))
  NSH3.ctrl@states["catch A",]        <- c(-1,0:6,6)
  NSH3.ctrl@states["catch BD",]       <- c(7:9,10,10,10,rep(-1,3))
  NSH3.ctrl@states["catch C",]        <- c(-1,11:13,14,14,14,rep(-1,2))
  
  NSH3.ctrl@catchabilities["HERAS",ac(1:8)]   <- c(101,102,rep(103,6))
  NSH3.ctrl@catchabilities["IBTS-Q3",ac(0:5)] <- c(200:205)
  NSH3.ctrl@catchabilities[laiRow,1]  <- 301
  
  NSH3.ctrl@obs.vars["catch A",]      <- c(-1,0,1,1,1,1,2,2,2)
  NSH3.ctrl@obs.vars["catch BD",]     <- c(101,102,rep(103,4),rep(-1,3))
  NSH3.ctrl@obs.vars["catch C",]      <- c(-1,201,202,rep(203,4),rep(-1,2))
  NSH3.ctrl@obs.vars["HERAS",ac(1:8)] <- c(301,302,rep(302,4),rep(303,2))
  NSH3.ctrl@obs.vars["IBTS-Q1",ac(1)] <- 401
  NSH3.ctrl@obs.vars["IBTS0",ac(0)]   <- 501
  NSH3.ctrl@obs.vars["IBTS-Q3",ac(0:5)]<- c(600,601,602,602,602,602)
  NSH3.ctrl@obs.vars[laiRow,1]        <- 701#:604
  
  
  idx                               <- which(rownames(NSH3.ctrl@cor.obs)=="IBTS-Q3")
  NSH3.ctrl@cor.obs[idx,1:5]          <- c(rep(102,5))
  NSH3.ctrl@cor.obs.Flag[idx]         <- as.factor("AR")
  NSH3.ctrl@cor.F                     <- as.integer(c(2,2,2))
  NSH3.ctrl@f.vars["catch A",]        <- c(-1,rep(102,5),rep(103,3))
  NSH3.ctrl@f.vars["catch BD",]       <- c(202,203,rep(203,4),rep(-1,3))
  NSH3.ctrl@f.vars["catch C",]        <- c(-1,301,302,rep(303,4),rep(-1,2))
  NSH3.ctrl@name                      <- "North Sea herring multifleet"
  NSH3.ctrl                           <- update(NSH3.ctrl)
  
  NSH3.ctrl@residuals <- FALSE
  
  return(NSH3.ctrl)
}

config_mf_HAWG2023 <- function( NSHs3,
                                NSH.tun,
                                pg){
  NSH3.ctrl                         <- FLSAM.control(NSHs3,NSH.tun,sumFleets=dimnames(NSHs3[["residual"]]@catch)$area)
  
  catchRow                          <- grep("catch",rownames(NSH3.ctrl@f.vars))
  laiRow                            <- grep("LAI",rownames(NSH3.ctrl@obs.vars))
  
  #Variance in N random walks (set 1st one free is usually best)
  NSH3.ctrl@logN.vars[]               <- c(1,rep(2,length(1:pg)))
  
  # fishing mortality states
  NSH3.ctrl@states["catch A",]        <- c(0,0,1:6,6)
  NSH3.ctrl@states["catch BD",]       <- c(7:9,10,10,10,rep(-1,3))
  NSH3.ctrl@states["catch C",]        <- c(-1,11:13,14,14,14,rep(-1,2))
  
  # obs.var
  NSH3.ctrl@obs.vars["catch A",]        <- c(0,0,rep(1,5),rep(2,length(7:pg)))
  NSH3.ctrl@obs.vars["catch BD",]       <- c(101,101,rep(102,4),rep(-1,3))
  NSH3.ctrl@obs.vars["catch C",]        <- c(-1,201,201,rep(202,4),rep(-1,2))
  NSH3.ctrl@obs.vars["HERAS",ac(1:pg)]  <- c(301,302,303,rep(304,3),rep(305,length(7:pg)))
  NSH3.ctrl@obs.vars["IBTS-Q1",ac(1)]   <- 401
  NSH3.ctrl@obs.vars["IBTS0",ac(0)]     <- 501
  NSH3.ctrl@obs.vars["IBTS-Q3",ac(0:5)] <- c(rep(601,2),rep(602,4))#c(400,401,rep(402,4))
  NSH3.ctrl@obs.vars[laiRow,1]          <- 701#:604
  
  # catchabilities
  NSH3.ctrl@catchabilities["HERAS",ac(1:pg)]  <- c(rep(100,2),rep(101,length(3:pg)))
  NSH3.ctrl@catchabilities["IBTS-Q1",ac(1)]    <- c(201)
  NSH3.ctrl@catchabilities["IBTS-Q3",ac(0:5)] <- 300:305#c(300,rep(301,3),302,303)
  NSH3.ctrl@catchabilities[laiRow,1]          <- 401
  
  #Add correlation correction for Q3 survey, not for HERAS
  idx                                 <- which(rownames(NSH3.ctrl@cor.obs)=="IBTS-Q3")
  NSH3.ctrl@cor.obs[idx,1:5]          <- c(rep(101,5))
  NSH3.ctrl@cor.obs.Flag[idx]         <- as.factor("AR")
  
  #correlation structure among ages in F random walks
  NSH3.ctrl@cor.F                     <- as.integer(c(2,2,2))
  
  # f.vars
  NSH3.ctrl@f.vars["catch A",]        <- c(101,101,rep(102,4),rep(103,3))
  NSH3.ctrl@f.vars["catch BD",]       <- c(201,202,rep(203,4),rep(-1,3))
  NSH3.ctrl@f.vars["catch C",]        <- c(-1,301,302,rep(303,4),rep(-1,2))
  
  # tidying up
  NSH3.ctrl@name                      <- "North Sea herring multifleet"
  NSH3.ctrl                           <- update(NSH3.ctrl)
  
  NSH3.ctrl@residuals <- FALSE
  
  return(NSH3.ctrl)
}

config_mf_HAWG2023_NTH <- function( NSHs3,
                                    NSH.tun,
                                    pg){
  NSH3.ctrl                         <- FLSAM.control(NSHs3,NSH.tun,sumFleets=dimnames(NSHs3[["residual"]]@catch)$area)
  
  catchRow                          <- grep("catch",rownames(NSH3.ctrl@f.vars))
  laiRow                            <- grep("LAI",rownames(NSH3.ctrl@obs.vars))
  
  #Variance in N random walks (set 1st one free is usually best)
  NSH3.ctrl@logN.vars[]               <- c(1,rep(2,length(1:pg)))
  
  # fishing mortality states
  NSH3.ctrl@states["catch A",]        <- c(-1,0,1:6,6)
  NSH3.ctrl@states["catch BD",]       <- c(7:9,-1,-1,-1,rep(-1,3))
  NSH3.ctrl@states["catch C",]        <- c(-1,11:13,14,14,14,rep(-1,2))
  
  # obs.var
  NSH3.ctrl@obs.vars["catch A",]        <- c(-1,0,rep(1,5),rep(2,length(7:pg)))
  NSH3.ctrl@obs.vars["catch BD",]       <- c(101,102,rep(103,1),rep(-1,6))
  NSH3.ctrl@obs.vars["catch C",]        <- c(-1,201,201,rep(202,4),rep(-1,2))
  NSH3.ctrl@obs.vars["HERAS",ac(1:pg)]  <- c(301,302,303,rep(304,3),rep(305,length(7:pg)))
  NSH3.ctrl@obs.vars["IBTS-Q1",ac(1)]   <- 401
  NSH3.ctrl@obs.vars["IBTS0",ac(0)]     <- 501
  NSH3.ctrl@obs.vars["IBTS-Q3",ac(0:5)] <- c(rep(601,2),rep(602,4))#c(400,401,rep(402,4))
  NSH3.ctrl@obs.vars[laiRow,1]          <- 701#:604
  
  # catchabilities
  NSH3.ctrl@catchabilities["HERAS",ac(1:pg)]  <- c(rep(100,2),rep(101,length(3:pg)))
  NSH3.ctrl@catchabilities["IBTS-Q1",ac(1)]    <- c(201)
  NSH3.ctrl@catchabilities["IBTS-Q3",ac(0:5)] <- 300:305#c(300,rep(301,3),302,303)
  NSH3.ctrl@catchabilities[laiRow,1]          <- 401
  
  #Add correlation correction for Q3 survey, not for HERAS
  idx                                 <- which(rownames(NSH3.ctrl@cor.obs)=="IBTS-Q3")
  NSH3.ctrl@cor.obs[idx,1:5]          <- c(rep(101,5))
  NSH3.ctrl@cor.obs.Flag[idx]         <- as.factor("AR")
  
  #correlation structure among ages in F random walks
  NSH3.ctrl@cor.F                     <- as.integer(c(2,2,2))
  
  # f.vars
  NSH3.ctrl@f.vars["catch A",]        <- c(-1,101,rep(102,4),rep(103,3))
  NSH3.ctrl@f.vars["catch BD",]       <- c(202,203,rep(203,1),rep(-1,6))
  NSH3.ctrl@f.vars["catch C",]        <- c(-1,301,302,rep(303,4),rep(-1,2))
  
  # tidying up
  NSH3.ctrl@name                      <- "North Sea herring multifleet"
  NSH3.ctrl                           <- update(NSH3.ctrl)
  
  NSH3.ctrl@residuals <- FALSE
  
  return(NSH3.ctrl)
}

config_mf_HAWG2024 <- function( NSHs3,
                                    NSH.tun,
                                    pg){
  NSH3.ctrl                         <- FLSAM.control(NSHs3,NSH.tun,sumFleets=dimnames(NSHs3[["residual"]]@catch)$area)
  
  catchRow                          <- grep("catch",rownames(NSH3.ctrl@f.vars))
  laiRow                            <- grep("LAI",rownames(NSH3.ctrl@obs.vars))
  
  #Variance in N random walks (set 1st one free is usually best)
  NSH3.ctrl@logN.vars[]               <- c(1,rep(2,length(1:pg)))
  
  # fishing mortality states
  NSH3.ctrl@states["catch A",]        <- c(-1,0,1:6,6)
  NSH3.ctrl@states["catch BD",]       <- c(7:9,-1,-1,-1,rep(-1,3))
  NSH3.ctrl@states["catch C",]        <- c(-1,11:13,14,14,14,rep(-1,2))
  
  # obs.var
  NSH3.ctrl@obs.vars["catch A",]        <- c(-1,0,rep(1,5),rep(2,length(7:pg)))
  NSH3.ctrl@obs.vars["catch BD",]       <- c(101,102,rep(103,1),rep(-1,6))
  NSH3.ctrl@obs.vars["catch C",]        <- c(-1,201,201,rep(202,4),rep(-1,2))
  NSH3.ctrl@obs.vars["HERAS",ac(1:pg)]  <- c(301,302,303,rep(304,3),rep(305,length(7:pg)))
  NSH3.ctrl@obs.vars["IBTS-Q1",ac(1)]   <- 401
  NSH3.ctrl@obs.vars["IBTS0",ac(0)]     <- 501
  NSH3.ctrl@obs.vars["IBTS-Q3",ac(0:5)] <- c(rep(601,2),rep(602,4))#c(400,401,rep(402,4))
  NSH3.ctrl@obs.vars[laiRow,1]          <- 701#:604
  
  # catchabilities
  NSH3.ctrl@catchabilities["HERAS",ac(1:pg)]  <- c(rep(100,2),rep(101,length(3:pg)))
  NSH3.ctrl@catchabilities["IBTS-Q1",ac(1)]    <- c(201)
  NSH3.ctrl@catchabilities["IBTS-Q3",ac(0:5)] <- 300:305#c(300,rep(301,3),302,303)
  NSH3.ctrl@catchabilities[laiRow,1]          <- 401
  
  #Add correlation correction for Q3 survey, not for HERAS
  idx                                 <- which(rownames(NSH3.ctrl@cor.obs)=="IBTS-Q3")
  NSH3.ctrl@cor.obs[idx,1:5]          <- c(rep(101,5))
  NSH3.ctrl@cor.obs.Flag[idx]         <- as.factor("AR")
  
  #correlation structure among ages in F random walks
  NSH3.ctrl@cor.F                     <- as.integer(c(2,2,2))
  
  # f.vars
  NSH3.ctrl@f.vars["catch A",]        <- c(-1,101,rep(102,4),rep(103,3))
  NSH3.ctrl@f.vars["catch BD",]       <- c(202,203,204,rep(-1,6))
  NSH3.ctrl@f.vars["catch C",]        <- c(-1,301,302,rep(303,4),rep(-1,2))
  
  # tidying up
  NSH3.ctrl@name                      <- "North Sea herring multifleet"
  NSH3.ctrl                           <- update(NSH3.ctrl)
  
  NSH3.ctrl@residuals <- FALSE
  
  return(NSH3.ctrl)
}


config_mf_IBPNSherring2021_alt <- function( NSHs3,
                                        NSH.tun,
                                        pg){
  NSH3.ctrl                         <- FLSAM.control(NSHs3,NSH.tun)
  
  catchRow                          <- grep("catch",rownames(NSH3.ctrl@f.vars))
  laiRow                            <- grep("LAI",rownames(NSH3.ctrl@obs.vars))
  
  #Variance in N random walks (set 1st one free is usually best)
  NSH3.ctrl@logN.vars[]               <- c(1,rep(2,length(1:pg)))
  
  # fishing mortality states
  NSH3.ctrl@states["catch A",]        <- c(-1,0:6,6)
  NSH3.ctrl@states["catch BD",]       <- c(7:9,10,10,10,rep(-1,3))
  NSH3.ctrl@states["catch C",]        <- c(-1,11:13,14,14,14,rep(-1,2))
  
  # obs.var
  NSH3.ctrl@obs.vars["catch A",]        <- c(-1,0,rep(1,5),rep(2,length(7:pg)))
  NSH3.ctrl@obs.vars["catch BD",]       <- c(101,102,rep(103,4),rep(-1,3))
  NSH3.ctrl@obs.vars["catch C",]        <- c(-1,201,202,rep(203,4),rep(-1,2))
  NSH3.ctrl@obs.vars["HERAS",ac(1:pg)]  <- c(301,302,303,rep(304,3),rep(305,length(7:pg)))
  NSH3.ctrl@obs.vars["IBTS-Q1",ac(1)]   <- 401
  NSH3.ctrl@obs.vars["IBTS0",ac(0)]     <- 501
  NSH3.ctrl@obs.vars["IBTS-Q3",ac(0:5)] <- c(rep(601,2),rep(602,4))#c(400,401,rep(402,4))
  NSH3.ctrl@obs.vars[laiRow,1]          <- 701#:604
  
  # catchabilities
  NSH3.ctrl@catchabilities["HERAS",ac(1:pg)]  <- c(rep(100,2),rep(101,length(3:pg)))
  NSH3.ctrl@catchabilities["IBTS-Q1",ac(1)]    <- c(201)
  NSH3.ctrl@catchabilities["IBTS-Q3",ac(0:5)] <- 300:305#c(300,rep(301,3),302,303)
  NSH3.ctrl@catchabilities[laiRow,1]          <- 401
  
  #Add correlation correction for Q3 survey, not for HERAS
  idx                                 <- which(rownames(NSH3.ctrl@cor.obs)=="IBTS-Q3")
  NSH3.ctrl@cor.obs[idx,1:5]          <- c(rep(101,5))
  NSH3.ctrl@cor.obs.Flag[idx]         <- as.factor("AR")
  
  #correlation structure among ages in F random walks
  NSH3.ctrl@cor.F                     <- as.integer(c(2,2,2))
  
  # f.vars
  NSH3.ctrl@f.vars["catch A",]        <- c(-1,101,rep(102,4),rep(103,3))
  NSH3.ctrl@f.vars["catch BD",]       <- c(202,203,rep(203,4),rep(-1,3))
  NSH3.ctrl@f.vars["catch C",]        <- c(-1,301,302,rep(303,4),rep(-1,2))
  
  # tidying up
  NSH3.ctrl@name                      <- "North Sea herring multifleet"
  NSH3.ctrl                           <- update(NSH3.ctrl)
  
  NSH3.ctrl@residuals <- FALSE
  
  return(NSH3.ctrl)
}