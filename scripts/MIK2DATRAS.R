rm(list = (ls()))

path1 <- "//wurnet.nl/dfs-root/IMARES/IJmuiden/WOT/WOT surveys Zout - Haringlarven/Down's recruitment survey/New index calculation/"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)

# load the excel sheet
hh <- read_excel(paste0(path1, "DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "HH-EH")
hl <- read_excel(paste0(path1, "DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "HL-EH-EM")
ca <- read_excel(paste0(path1, "DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "CA-EH-EM")

# load our data
EH.MIK <- read.csv(paste0(path1, "EH_EggsAndLarvaeDataSet202397_1992_2023.csv"))
EM.MIK <- read.csv(paste0(path1, "EM_EggsAndLarvaeDataSet202397_1992_2023.csv"))

summary(EH.MIK)
summary(EM.MIK)
EH.MIK$RecordType <- NULL
EM.MIK$RecordType <- NULL
EH.MIK$Notes <- NULL
EM.MIK$Notes <- NULL

# calculate missing VolumeFiltInt
EH.MIK$VolumeFiltInt <- ifelse(is.na(EH.MIK$VolumeFiltInt), (EH.MIK$FlowIntRevs/EH.MIK$FlowIntCalibr)*EH.MIK$NetopeningArea, EH.MIK$VolumeFiltInt)

MIK <- full_join(EM.MIK, EH.MIK, by = c("HaulID", "ICES_FileID", "ICES_HaulID"))
summary(MIK)

MIK <- MIK %>%
  filter(!is.na(ICES_MeasurementID)) %>%
  filter(!ELHaulFlag == "U")

# find the ICES rectangle (statrec) where the threshold needs to be applied
ices.rect <- c("36E9", "36F0", "36F1", "36F2", "36F3", "36F4", "36F5", "36F6", "36F7",
               "35F0", "35F1", "35F2", "35F3", "35F4",
               "34F1", "34F2", "34F3", "34F4",
               "33F1", "33F2", "33F3", "33F4",
               "32F1", "32F2", "32F3",
               "31F1", "31F2",
               "30F0", "30F1",
               "29F0", "29F1")
MIK$threshold.area <- ifelse(MIK$statrec %in% ices.rect, "threshold", "original")
MIK.thres <- subset(MIK, threshold.area == "threshold")
MIK.origi <- subset(MIK, threshold.area == "original")
MIK.thres <- MIK.thres %>%
  filter(!(Length > 0 & Length < 19))

MIK <- rbind(MIK.thres, MIK.origi) # 67854 obs.
summary(MIK)

MIK$RecordType <- "EH-EM"
MIK$Notes <- "NA"

# Change column names of hh, hl, ca data
colnames(hh) <- c("Datras", "fields1", "width1",
                  "mandatory1", "datatype1", "note1",
                  "EggsLarvae", "field2", "width2",
                  "mandatory2", "datatype2")
# delete first row
hh <- hh[-1, ]
# extract names listed in the fields2 column
hh_nameslist <- na.omit(hh$field2)
# change the name Day.night
hh_nameslist <- gsub("Day/night", "Day.night", hh_nameslist)

hh.mik <- MIK[, intersect(colnames(MIK), hh_nameslist)]
hh.mik$RecordType <- "HH"

# change column names of hl
colnames(hl) <- c("Datras", "fields1", "width1",
                  "mandatory1", "datatype1", "note1",
                  "EggsLarvae", "field2", "width2",
                  "mandatory2", "datatype2")
hl <- hl[-1,]

# extract names listed in the fields2 column
hl_nameslist <- na.omit(hl$field2)
hl_nameslist <- gsub("Yes", "RecordType", hl_nameslist)

hl.mik <- MIK[, intersect(colnames(MIK), hl_nameslist)]
hl.mik$RecordType <- "HL"

# change column names of ca
colnames(ca) <- c("Datras", "fields1", "width1",
                  "mandatory1", "datatype1", "note1",
                  "EggsLarvae", "field2", "width2",
                  "mandatory2", "datatype2")
ca <- ca[-1,]
# extract names listed in the fields2 column
ca_nameslist <- na.omit(ca$field2)
ca_nameslist <- gsub("Yes", "RecordType", ca_nameslist)

ca.mik <- MIK[, intersect(colnames(MIK), ca_nameslist)]
ca.mik$RecordType <- "CA"

# -------------------------------------------------
# conversion fields
# -------------------------------------------------
conversion.fields <- read.csv(file.path('./data','conversion_fields.csv'))

conversion.hh <- subset(conversion.fields,RecordType =='HH')
conversion.hl <- subset(conversion.fields,RecordType =='HL')
conversion.ca <- subset(conversion.fields,RecordType =='CA')

# add VolumeFiltInt to the DATRAS SweepLngt
conversion.hh$fields_eggsLarvae <- ifelse(conversion.hh$fields_DATRAS == "SweepLngt", 
                                          "VolumeFiltInt", conversion.hh$fields_eggsLarvae)
# delete VolumeFiltInt
conversion.hh <- conversion.hh[-88, ]
conversion.hh$fields_eggsLarvae <- ifelse(conversion.hh$fields_eggsLarvae == "Day/night", 
                                          "Day.night", conversion.hh$fields_eggsLarvae)

hh <- hh.mik %>% 
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

hh[is.na(hh)]<- ""

# -------------------------------------------------
# create hl data frame
# double check LngtCode with Cindy. Currently set at 0
# double check with CIndy that there is only herring. There is different entries in hl$SpecCode:
# "Clupea harengus"  "Clupea harengus " NA
# -------------------------------------------------
hl <- hl.mik %>% select(-c(conversion.hl$fields_eggsLarvae[is.na(conversion.hl$order_slots)]))  %>% 
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
ca.mik$IndividualNumber <- 1:dim(ca.mik)[1]
ca <- ca.mik %>% select(-c(conversion.ca$fields_eggsLarvae[is.na(conversion.ca$order_slots)]))

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
write.table(hh,file = file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'),
            row.names = F,quote = F,append = F, sep=",")

write.table(hl,file = file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'),
            row.names = F,quote = F,append = T, sep=",")

write.table(ca,file = file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'),
            row.names = F,quote = F,append = T, sep=",")

MIK2DATRAS_eggsLarvae <- readICES(file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'), strict = TRUE)
MIK2DATRAS_eggsLarvae[[3]]$Count <- MIK2DATRAS_eggsLarvae[[3]]$SubFactor * MIK2DATRAS_eggsLarvae[[3]]$TotalNo

dAll <- addSpectrum(MIK2DATRAS_eggsLarvae, cm.breaks=seq(0,40,by=1))

names(dAll[[1]])
summary(dAll[[1]])
summary(dAll[[2]])
summary(dAll[[3]])

rm(list=(ls()))

path1 <- "//wurnet.nl/dfs-root/IMARES/IJmuiden/WOT/WOT surveys Zout - Haringlarven/Down's recruitment survey/New index calculation/"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)
library(tidyr)
MIK2DATRAS_eggsLarvae <- readICES(file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'), strict = TRUE)


