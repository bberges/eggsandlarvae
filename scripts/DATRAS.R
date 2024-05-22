path1 <- "//wurnet.nl/dfs-root/IMARES/IJmuiden/WOT/WOT surveys Zout - Haringlarven/Down's recruitment survey/New index calculation/"
setwd(path1)

library(dplyr)
library(readxl)

########################
##    Data editing    ##
########################
# load the excel sheet
hh <- read_excel(paste0(path1, "DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "HH-EH")
hl <- read_excel(paste0(path1, "DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "HL-EH-EM")
ca <- read_excel(paste0(path1, "DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "CA-EH-EM")

# load our data
EH.DRS <- read.csv(paste0(path1, "EH_DRS_2018_2023.csv")) # 326
EM.DRS <- read.csv(paste0(path1, "EM_DRS_2018_2023.csv")) # 1549
summary(EH.DRS)
summary(EM.DRS)
# merge EH.DRS and EM.DRS data
EH.DRS$RecordType <- NULL
EM.DRS$RecordType <- NULL
EH.DRS$Notes <- NULL
EM.DRS$Notes <- NULL

DRS <- full_join(EM.DRS, EH.DRS, by = "HaulID")
summary(DRS)
DRS$RecordType <- "EH-EM"
DRS$Notes <- "NA"

# change column names of hh, hl, ca data
colnames(hh) <- c("Datras", "fields1", "width1",
                  "mandatory1", "datatype1", "note1",
                  "EggsLarvae", "field2", "width2",
                  "mandatory2", "datatype2")
# delete first row 
hh <- hh[-1,]
# extract names listed in the fields2 column
hh_nameslist <- na.omit(hh$field2)
# change the name Day.night
hh_nameslist <- gsub("Day/night", "Day.night", hh_nameslist)

hh.drs <- DRS[, intersect(colnames(DRS), hh_nameslist)]
hh.drs$RecordType <- "HH"

# change column names of hl
colnames(hl) <- c("Datras", "fields1", "width1",
                  "mandatory1", "datatype1", "note1",
                  "EggsLarvae", "field2", "width2",
                  "mandatory2", "datatype2")
hl <- hl[-1,]

# extract names listed in the fields2 column
hl_nameslist <- na.omit(hl$field2)
hl_nameslist <- gsub("Yes", "RecordType", hl_nameslist)

hl.drs <- DRS[, intersect(colnames(DRS), hl_nameslist)]
hl.drs$RecordType <- "HL"

# ca
colnames(ca) <- c("Datras", "fields1", "width1",
                  "mandatory1", "datatype1", "note1",
                  "EggsLarvae", "field2", "width2",
                  "mandatory2", "datatype2")
ca <- ca[-1,]
# extract names listed in the fields2 column
ca_nameslist <- na.omit(ca$field2)
ca_nameslist <- gsub("Yes", "RecordType", ca_nameslist)

ca.drs <- DRS[, intersect(colnames(DRS), ca_nameslist)]
ca.drs$RecordType <- "CA"

DATRAS.drs <- list(ca = ca.drs, hl = hl.drs, hh = hh.drs)


DATRAS.drs[[1]]$Year <-  DATRAS.drs[["hh"]]$Year
DATRAS.drs[["ca"]]$Year <-  DATRAS.drs[["hh"]]$Year
DATRAS.drs[[3]]$Year <-  DATRAS.drs[["hh"]]$Year
DATRAS.drs[[1]]
DATRAS.drs[[2]]$Year <- DATRAS.drs[["hh"]]$Year
DATRAS.drs[[2]]
table(DATRAS.drs[[2]]$Year)
table(DATRAS.drs[[1]]$Year)

save(DATRAS.drs, file = paste(getwd(), "/data/DATRAS.drs.RData", sep = ""))

################################################
##     Convert data format into DATRASraw     ##
################################################
library(surveyIndex)
library(data.table)
library(ggplot2)
library(DATRAS)
#load(paste0(getwd(), "/data/DATRAS.drs.RData"))
#load(paste0(getwd(), "/data/DATRAS_eggsLarvae.csv"))
summary(DATRAS.drs[[1]]$Length)
dim(DATRAS.drs[[1]])
dim(DATRAS.drs[[2]])
dim(DATRAS.drs[[3]])
table(DATRAS.drs[[1]]$Year)
table(DATRAS.drs[[3]]$Year)

drs <- getDatrasExchange_kelly(existing_dat = DATRAS.drs, years = 2018:2021, strict = FALSE)

class(DATRAS.drs)

DATRAS.drs <- addSpectrum(DATRAS.drs, cm.breaks = 1:50)
DATRAS.drs <- addWeightByHaul(DATRAS.drs)
bubblePlot(DATRAS.drs)
