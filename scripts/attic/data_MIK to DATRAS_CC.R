rm(list = (ls()))

path1 <- "C:/Users/chen072/OneDrive - Wageningen University & Research/0_2026_KBWOT_and others/DRS/eggsandlarvae-main/"
#path1 <- "C:/git/harring_eggsandlarvae/"
setwd(path1)

library(tidyverse)
library(readxl)
library(icesDatras)
library(surveyIndex)

# load the excel sheet
hh <- read_excel(paste0(path1, 'data/',"DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "HH-EH")
hl <- read_excel(paste0(path1, 'data/',"DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "HL-EH-EM")
ca <- read_excel(paste0(path1, 'data/',"DATRAS_EggsLarvae_conversion.xlsx", sep = ""), sheet = "CA-EH-EM")

# load our data
EH.MIK <- read.csv(paste0(path1, 'data/',"EH_MIK_1992_2026.csv"))
EM.MIK <- read.csv(paste0(path1, 'data/',"EM_MIK_1992_2026.csv"))
length(unique(EH.MIK$HaulID))
length(unique(EM.MIK$HaulID))
summary(EH.MIK)
summary(EM.MIK)
EH.MIK$RecordType <- NULL
EM.MIK$RecordType <- NULL
EH.MIK$Notes <- NULL
EM.MIK$Notes <- NULL

# calculate missing VolumeFiltInt ----
EH.MIK$VolumeFiltInt <- ifelse(is.na(EH.MIK$VolumeFiltInt), (EH.MIK$FlowIntRevs/EH.MIK$FlowIntCalibr)*EH.MIK$NetopeningArea, EH.MIK$VolumeFiltInt)
summary(EH.MIK$VolumeFiltInt)

# merge trawlist with length samples ----
MIK <- full_join(EM.MIK, EH.MIK, by = c("HaulID", "ICES_FileID", "ICES_HaulID"))
summary(MIK)
length(unique(MIK$HaulID))

table(MIK$ELHaulFlag, useNA="always")
summary(MIK$ICES_MeasurementID)
summary(MIK$Number)
unique(MIK$Species)

## CC: assign zero catches
MIK$Number[is.na(MIK$ICES_MeasurementID)] <- 0

# CC: check duplicate rows:  Cindy said include them ----
## check
dup_rows <- MIK %>%
  group_by(HaulID, Length, StationNumber) %>%
  filter(n() > 1) %>%
  arrange(HaulID, StationNumber, Length)

dup_rows
MIK %>%
  count(HaulID, Length, StationNumber, Number) %>%
  filter(n > 1)

MIK[MIK$HaulID == "2026NL3400208" & MIK$Length == 26 & MIK$StationNumber=="10183427",]

## CC: disabled this, ICES_MeasurementID= NA are zero hauls, should include
#MIK <- MIK %>%
#  filter(!is.na(ICES_MeasurementID)) %>%  ## this would exclude the zero observations
#  filter(!ELHaulFlag == "U")

# assign larva source ----
## when length = na, include 
# find the ICES rectangle (statrec) where the threshold needs to be applied
ices.rect <- c("36E9", "36F0", "36F1", "36F2", "36F3", "36F4", "36F5", "36F6", "36F7",
               "35F0", "35F1", "35F2", "35F3", "35F4",
               "34F1", "34F2", "34F3", "34F4",
               "33F1", "33F2", "33F3", "33F4",
               "32F1", "32F2", "32F3",
               "31F1", "31F2",
               "30F0", "30F1",
               "29F0", "29F1")
MIK$threshold.area <- ifelse(MIK$statrec %in% ices.rect | MIK$StartLatitude < 54, "MIK-south", "MIK-north")
MIK$threshold.area[MIK$threshold.area == "MIK-south" & MIK$Length < 19 &  !(is.na(MIK$Length)) & MIK$Length != 0] <- "Downs"
table(MIK$threshold.area, useNA="always")
## CC: updated the area-length selection part. 
#MIK.thres <- subset(MIK, threshold.area == "south")
#MIK.origi <- subset(MIK, threshold.area == "north")
#MIK.thres <- MIK.thres %>%
#  filter(!(Length > 0 & Length < 19))
#MIK.down <- MIK.thres %>%
 # filter(Length <19 &  !(is.na(Length)) & Length != 0)
#MIK.thres <- MIK.thres %>%
 # filter(Length >= 19 | is.na(Length) | Length == 0)
#summary(MIK.thres$Length)
#MIK <- rbind(MIK.thres, MIK.origi) # 67854 obs.
#summary(MIK)
#length(unique(MIK$HaulID))

## plot sample locations
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world   <- ne_countries(scale = "medium", returnclass = "sf")
myvalue <-  c("MIK-north" = "#185FA5", "MIK-south" = "#D85A30","Downs" = "#2A9D8F" )

# Get unique haul locations (
haul_locs <- MIK |>
  distinct(HaulID, StartLongitude, StartLatitude, threshold.area)

# Plot
ggplot() +
  geom_sf(data = world, fill = "grey85", color = "white", linewidth = 0.3) +
  geom_point(data = haul_locs,
             aes(x = StartLongitude, y = StartLatitude, color = threshold.area),
             size = 2, alpha = 0.7) +
  coord_sf(xlim = c(-5, 15), ylim = c(50, 65)) +  # adjust to your data extent
  scale_color_manual(values = myvalue) +
  facet_wrap(~threshold.area) +
  labs(x = NULL, y = NULL, color = "threshold area") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

# sample locations per year 
haul_locs_year <- MIK |>
  distinct(Year, HaulID, StartLongitude, StartLatitude, threshold.area)
ggplot() +
  geom_sf(data = world,
          fill = "grey85",
          color = "white",
          linewidth = 0.3) +
  geom_point(
    data = haul_locs_year,
    aes(
      x = StartLongitude,
      y = StartLatitude,
      color = threshold.area
    ),
    size = 2,
    alpha = 0.7
  ) +
  coord_sf(xlim = c(-5, 15), ylim = c(50, 65)) +
  scale_color_manual(
    values = myvalue
  ) +
  facet_wrap(~Year, ncol=10) +
  labs(
    x = NULL,
    y = NULL,
    color = "threshold area"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

# length histogram ----
summary(MIK$Length)
## histogram
MIK_plot <- MIK %>%
  filter(!is.na(Length), Length != 0) %>%
  group_by(HaulID, Year, threshold.area, Length) %>%
  summarise(
    Number = sum(Number, na.rm = TRUE),
    .groups = "drop"
  )
ggplot(MIK_plot, aes(x = Length, fill = threshold.area, weight = Number)) +
  geom_histogram(
    binwidth = 1,
    position = "identity",
    alpha = 0.5,
    color = "black"
  ) +
  #facet_wrap(~Year) +
  scale_fill_manual(
    values = myvalue
  ) +
  labs(
    x = "Length",
    y = "Number of fish",
    fill = "threshold area"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggplot(MIK_plot[MIK_plot$threshold.area != "Downs",],
       aes(x = Length, fill = threshold.area, weight = Number)) +
  geom_histogram(
    binwidth = 1,
    position = "identity",
    alpha = 0.5,
    color = "black"
  ) +
  facet_wrap(~Year) +
  coord_cartesian(xlim = c(0, 50)) +
  scale_fill_manual(values = myvalue) +
  theme_bw()


library(Hmisc)

length_summary <- MIK %>%
  filter(!is.na(Length), Length != 0, !is.na(Number)) %>%
  group_by(Year, threshold.area) %>%
  summarise(
    mean_length = weighted.mean(Length, w = Number),
    median_length = wtd.quantile(Length, weights = Number, probs = 0.5),
    min_length = min(Length),
    max_length = max(Length),
    total_fish = sum(Number),
    .groups = "drop"
  )

length_summary

ggplot(length_summary, aes(x = Year, color = threshold.area)) +
  geom_line(aes(y = mean_length), linewidth = 1) +
  geom_point(aes(y = mean_length), size = 2) +
  geom_line(aes(y = median_length), linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = median_length), shape = 17, size = 2) +
  scale_color_manual(values = myvalue) +
  labs(
    y = "Length",
    color = "Threshold area",
    title = "Mean and median of observed length by year"
  ) +
  theme_bw()

ggplot(length_summary,
       aes(x = Year, y = median_length, color = threshold.area)) +
  geom_linerange(aes(ymin = min_length, ymax = max_length),
                 position = position_dodge(width = 0.4),
                 linewidth = 1) +
  geom_point(position = position_dodge(width = 0.4),
             size = 3) +
  scale_color_manual(values = myvalue) +
  labs(
    y = "Length",
    color = "Threshold area",
    title = "Length range and median by year"
  ) +
  theme_bw()

# CC: exclude Downs ----
MIK <- MIK[MIK$threshold.area != "Downs",]
length(unique(MIK$HaulID))
sum(MIK$Number)
summary(MIK$Length)

saveRDS(MIK, "data/MIK.rds")

# convert to Datras ----
## process MIK
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
### CC: disabled distinct(HaulID,Length,StationNumber,.keep_all=T), Cindy said these duplicates are valid
#hl <- hl.mik %>% select(-c(conversion.hl$fields_eggsLarvae[is.na(conversion.hl$order_slots)]))  %>% 
#  distinct(HaulID,Length,StationNumber,.keep_all=T)
hl <- hl.mik %>% select(-c(conversion.hl$fields_eggsLarvae[is.na(conversion.hl$order_slots)]))

###?/

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
write.table(hh,file = file.path('./data','MIK2DATRAS_all.csv'),
            row.names = F,quote = F,append = F, sep=",")

write.table(hl,file = file.path('./data','MIK2DATRAS_all.csv'),
            row.names = F,quote = F,append = T, sep=",")

write.table(ca,file = file.path('./data','MIK2DATRAS_all.csv'),
            row.names = F,quote = F,append = T, sep=",")

# MIK2DATRAS_eggsLarvae <- readICES(file.path('./data','MIK2DATRAS_all.csv'), strict = TRUE)
# MIK2DATRAS_eggsLarvae[[3]]$Count <- MIK2DATRAS_eggsLarvae[[3]]$SubFactor * MIK2DATRAS_eggsLarvae[[3]]$TotalNo
# 
# dAll <- addSpectrum(MIK2DATRAS_eggsLarvae, cm.breaks=seq(0,40,by=1))
# 
# names(dAll[[1]])
# summary(dAll[[1]])
# summary(dAll[[2]])
# summary(dAll[[3]])
# 
# rm(list=(ls()))
# 
# path1 <- "C:/Users/chin008/OneDrive - Wageningen University & Research/git/eggsandlarvae_bberges/eggsandlarvae"
# #path1 <- "C:/git/harring_eggsandlarvae/"
# setwd(path1)
# 
# library(tidyverse)
# library(readxl)
# library(icesDatras)
# library(surveyIndex)
# library(tidyr)
# MIK2DATRAS_eggsLarvae <- readICES(file.path('./data','MIK2DATRAS_eggsLarvae_all.csv'), strict = TRUE)


