rm(list=(ls()))

path1 <- "//wurnet.nl/dfs-root/IMARES/IJmuiden/WOT/WOT surveys Zout - Haringlarven/Down's recruitment survey/New index calculation/"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)

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
?gsub
hh.drs <- DRS[, intersect(colnames(DRS), hh_nameslist)]
hh.drs$RecordType <- "HH"
hh.drs$VolumeFiltInt <- ifelse(is.na(hh.drs$VolumeFiltInt), (hh.drs$FlowIntRevs/hh.drs$FlowIntCalibr)*hh.drs$NetopeningArea, hh.drs$VolumeFiltInt)
summary(hh.drs)
table(hh.drs$Day.night)

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

# -------------------------------------------------
# conversion fields
# -------------------------------------------------
conversion.fields <- read.csv(file.path('./data','conversion_fields.csv'))

conversion.hh <- subset(conversion.fields,RecordType =='HH')
conversion.hl <- subset(conversion.fields,RecordType =='HL')
conversion.ca <- subset(conversion.fields,RecordType =='CA')

# -------------------------------------------------
# create hh data frame
# there is duplicates in hh.drs
# double check that we take all the hauls in the raw data
# check the 'C' in data type is correct
# -------------------------------------------------
# add VolumeFiltInt to the DATRAS SweepLngt
conversion.hh$fields_eggsLarvae <- ifelse(conversion.hh$fields_DATRAS == "SweepLngt", 
                                          "VolumeFiltInt", conversion.hh$fields_eggsLarvae)
# delete VolumeFiltInt
conversion.hh <- conversion.hh[-88, ]
conversion.hh$fields_eggsLarvae <- ifelse(conversion.hh$fields_eggsLarvae == "Day/night", 
                                          "Day.night", conversion.hh$fields_eggsLarvae)

hh <- hh.drs %>% 
      select(-c(conversion.hh$fields_eggsLarvae[is.na(conversion.hh$order_slots)])) %>% 
      distinct(HaulID,StationNumber,VolumeFiltInt, .keep_all=T)

idxFields <- match(colnames(hh), 
                   conversion.hh$fields_eggsLarvae)

hh <- hh %>% select(-c(colnames(hh)[which(is.na(idxFields))]))

idxFields <- match(colnames(hh),
                   conversion.hh$fields_eggsLarvae)

colnames(hh) <- conversion.hh$fields_DATRAS[idxFields]

hh[conversion.hh$fields_DATRAS[conversion.hh$fields_eggsLarvae == '']] <- ''
idxFields <- match(colnames(hh),
                   conversion.hh$fields_DATRAS)

hh <- hh[,order(conversion.hh$order_slots[idxFields])]

hh$Quarter <- 2
hh$HaulNo <- gsub(" ", "", hh$HaulNo, fixed = TRUE)
hh$DataType <- 'R' # check this value

# hh$ShootLong[grepl(" E", hh$ShootLong, fixed = TRUE)] <- "5.58383" # dirty fix, needs correcting in the db
# hh$ShootLat[grepl(" N", hh$ShootLat, fixed = TRUE)]  <- "55.8333" # dirty fix, needs correcting in the db

hh[is.na(hh)]<- ""

# -------------------------------------------------
# create hl data frame
# double check LngtCode with Cindy. Currently set at 0
# double check with CIndy that there is only herring. There is different entries in hl$SpecCode:
# "Clupea harengus"  "Clupea harengus " NA
# -------------------------------------------------
hl <- hl.drs %>% select(-c(conversion.hl$fields_eggsLarvae[is.na(conversion.hl$order_slots)]))  %>% 
      distinct(HaulID,Length,StationNumber,.keep_all=T)

idxFields <- match(colnames(hl),
                   conversion.hl$fields_eggsLarvae)

hl <- hl %>% select(-c(colnames(hl)[which(is.na(idxFields))]))

idxFields <- match(colnames(hl),
                   conversion.hl$fields_eggsLarvae)

colnames(hl) <- conversion.hl$fields_DATRAS[idxFields]

hl[conversion.hl$fields_DATRAS[conversion.hl$fields_eggsLarvae == '']] <- ''
idxFields <- match(colnames(hl),
                   conversion.hl$fields_DATRAS)

hl <- hl[,order(conversion.hl$order_slots[idxFields])]

hl$HaulNo <- gsub(" ", "", hl$HaulNo, fixed = TRUE)
hl$LngtCode <- 0 # this can mess the data, please double checked thoroughly

# fix of fields, take from hh
for(idxHaul in unique(hl$HaulNo)){
  idxFilt <- hl$HaulNo == idxHaul
  
  hl$Quarter[idxFilt] <- hh$Quarter[hh$HaulNo == idxHaul]
  hl$Country[idxFilt] <- hh$Country[hh$HaulNo == idxHaul]
  hl$Year[idxFilt]    <- hh$Year[hh$HaulNo == idxHaul]
}
#"StatRec" "Year", "Quarter", "Country"

hl$SpecVal <- 1
hl$SpecCodeType <- 'W'
hl$SpecCode <- 126417

hl[is.na(hl)]<- ""

# ------------------------------------------------------------------------
# create ca data frame
# at the moment this table is not correctly built
# it should be one entry for each individual, taken from the LF from hl
# ------------------------------------------------------------------------
ca.drs$IndividualNumber <- 1:dim(ca.drs)[1]
ca <- ca.drs %>% select(-c(conversion.ca$fields_eggsLarvae[is.na(conversion.ca$order_slots)]))

idxFields <- match(colnames(ca),
                   conversion.ca$fields_eggsLarvae)

ca <- ca %>% select(-c(colnames(ca)[which(is.na(idxFields))]))

idxFields <- match(colnames(ca),
                   conversion.ca$fields_eggsLarvae)

colnames(ca) <- conversion.ca$fields_DATRAS[idxFields]

ca[conversion.ca$fields_DATRAS[conversion.ca$fields_eggsLarvae == '']] <- ''
idxFields <- match(colnames(ca),
                   conversion.ca$fields_DATRAS)

ca <- ca[,order(conversion.ca$order_slots[idxFields])]

ca$HaulNo <- gsub(" ", "", ca$HaulNo, fixed = TRUE)
ca$LngtCode <- 1  # this can mess the data, please double checked thoroughly

# fix of fields, take from hh
for(idxHaul in unique(ca$HaulNo)){
  idxFilt <- ca$HaulNo == idxHaul
  
  ca$Quarter[idxFilt] <- hh$Quarter[hh$HaulNo == idxHaul]
  ca$Country[idxFilt] <- hh$Country[hh$HaulNo == idxHaul]
  ca$Year[idxFilt]    <- hh$Year[hh$HaulNo == idxHaul]
}

# assuming all is herring, please double check that, there is empty entries
ca$SpecCodeType <- 'W'
ca$SpecCode <- 126417
ca$ValidAphiaID <- 126417
ca$ScientificName_WoRMS <- 'Clupea harengus'

ca[is.na(ca)]<- ""

# -------------------------------------------------
# write tables
# -------------------------------------------------
write.table(hh,file = file.path('./data','DATRAS_eggsLarvae_all.csv'),
             row.names = F,quote = F,append = F, sep=",")

write.table(hl,file = file.path('./data','DATRAS_eggsLarvae_all.csv'),
             row.names = F,quote = F,append = T, sep=",")

write.table(ca,file = file.path('./data','DATRAS_eggsLarvae_all.csv'),
             row.names = F,quote = F,append = T, sep=",")

# setwd('./data')
# zip(zipfile = 'DATRAS_eggsLarvae_all.zip', files = 'DATRAS_eggsLarvae_all.csv')
# setwd('..')

# DATRAS_eggsLarvae <- readExchangeDir(path = file.path(".","/data/"),
#                                      pattern = ".zip", strict = TRUE)

DATRAS_eggsLarvae <- readICES(file.path('./data','DATRAS_eggsLarvae_all.csv'), strict = TRUE)

# DATRAS_eggsLarvae  <- subset(DATRAS_eggsLarvae,Species==species,Quarter == 1,Year %in% yearlist,HaulVal=="V",StdSpecRecCode==1)
DATRAS_eggsLarvae  <- addSpatialData(DATRAS_eggsLarvae,"./data/shapefiles/ICES_areas.shp")
DATRAS_eggsLarvae[[1]] #CA data
DATRAS_eggsLarvae[[2]] #HH data
DATRAS_eggsLarvae[[3]] #HL data
DATRAS_eggsLarvae[[3]]$Count <- DATRAS_eggsLarvae[[3]]$SubFactor * DATRAS_eggsLarvae[[3]]$TotalNo

dAll <- addSpectrum(DATRAS_eggsLarvae, cm.breaks=seq(0,40,by=1))

names(dAll[[1]])
summary(dAll[[1]])
summary(dAll[[2]])
summary(dAll[[3]])


