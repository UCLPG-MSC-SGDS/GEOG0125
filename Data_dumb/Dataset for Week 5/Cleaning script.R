
# set directory
setwd("/Volumes/Anwar-HHD/GEOG0125/Practicals/Road data")

casualties <- read.csv(file = "Casualties 1979-2020.csv", header = TRUE, sep = ",")
accidents <- read.csv(file = "Accidents 1979-2020.csv", header = TRUE, sep = ",")

raw_accidents <- accidents[accidents$accident_year > 2014,]
raw_casualties <- casualties[casualties$accident_year > 2014,]

rm(list = c("casualties", "accidents"))

# accidents table keep following risk factors:
# --- weather_conditions
# --- urban_or_rural_area
# --- light_conditions
# --- speed limit
# --- time

raw_acc_reduced <- raw_accidents[,c(1:3, 9, 16, 21, 28, 29, 33)]

raw_acc_reduced$fatal <- NA
raw_acc_reduced$fatal[raw_acc_reduced$accident_severity == 1] <- 1
raw_acc_reduced$fatal[raw_acc_reduced$accident_severity != 1] <- 0

raw_acc_reduced$Nonfatal <- NA
raw_acc_reduced$Nonfatal[raw_acc_reduced$accident_severity == 2 | raw_acc_reduced$accident_severity == 3] <- 1
raw_acc_reduced$Nonfatal[is.na(raw_acc_reduced$Nonfatal)] <- 0

raw_acc_reduced$accidents <- 1

raw_acc_reduced$speedlimit_cat <- ""
raw_acc_reduced$speedlimit_cat[raw_acc_reduced$speed_limit == "0" | raw_acc_reduced$speed_limit == "10" | raw_acc_reduced$speed_limit == "15" | raw_acc_reduced$speed_limit == "20"] <- "Category 1: 20 & below" 
raw_acc_reduced$speedlimit_cat[raw_acc_reduced$speed_limit == "30" | raw_acc_reduced$speed_limit == "40" | raw_acc_reduced$speed_limit == "50"] <- "Category 2: 30, 40 & 50" 
raw_acc_reduced$speedlimit_cat[raw_acc_reduced$speed_limit == "60" | raw_acc_reduced$speed_limit == "70"] <- "Category 3: 60 & 70" 

raw_acc_reduced <- raw_acc_reduced[raw_acc_reduced$speedlimit_cat!="",]

raw_acc_reduced$areatype <- ""
raw_acc_reduced$areatype[raw_acc_reduced$urban_or_rural_area == 1] <- "Urban"
raw_acc_reduced$areatype[raw_acc_reduced$urban_or_rural_area == 2] <- "Rural"

raw_acc_reduced <- raw_acc_reduced[raw_acc_reduced$areatype!="",]

raw_acc_reduced$timeperiod <- ""
raw_acc_reduced$timeperiod[raw_acc_reduced$light_conditions == 1] <- "Daytime" 
raw_acc_reduced$timeperiod[raw_acc_reduced$light_conditions == 4 | raw_acc_reduced$light_conditions == 5 | raw_acc_reduced$light_conditions == 6 | raw_acc_reduced$light_conditions == 7 ] <- "Nighttime" 

raw_acc_reduced <- raw_acc_reduced[raw_acc_reduced$timeperiod!="",]

raw_acc_reduced$weathertype <- ""
raw_acc_reduced$weathertype[raw_acc_reduced$weather_conditions == 1 | raw_acc_reduced$weather_conditions == 4] <- "1. Normal (Fine)" 
raw_acc_reduced$weathertype[raw_acc_reduced$weather_conditions == 2 | raw_acc_reduced$weather_conditions == 5] <- "2. Bad (Raining)" 
raw_acc_reduced$weathertype[raw_acc_reduced$weather_conditions == 3 | raw_acc_reduced$weather_conditions == 6] <- "3. Severe (Snowing)" 
raw_acc_reduced$weathertype[raw_acc_reduced$weather_conditions == 7 | raw_acc_reduced$weather_conditions == 8 | raw_acc_reduced$weather_conditions == 9] <- "4. Extreme (Fog, mist and others)"

raw_acc_reduced <- raw_acc_reduced[raw_acc_reduced$weathertype!="",]

names(raw_acc_reduced)
raw_acc_final <- raw_acc_reduced[,c(2,1,3,5,13:16)]

write.csv(raw_acc_final, file = "Accidents information 2015-2020.csv", row.names = FALSE)

rm(list = c("raw_acc_reduced", "raw_accidents"))

# reduce casualty data to car occupants only

raw_caroccupants <- raw_casualties[raw_casualties$casualty_type == 9, c(2,1,3,7:10)]

rm(raw_casualties)

raw_caroccupants$fatal <- NA
raw_caroccupants$fatal[raw_caroccupants$casualty_severity == 1] <- 1
raw_caroccupants$fatal[raw_caroccupants$casualty_severity != 1] <- 0

raw_caroccupants$Nonfatal <- NA
raw_caroccupants$Nonfatal[raw_caroccupants$casualty_severity == 2 | raw_caroccupants$casualty_severity == 3] <- 1
raw_caroccupants$Nonfatal[is.na(raw_caroccupants$Nonfatal)] <- 0

raw_caroccupants$casualties <- 1

raw_caroccupants$gender <- ""
raw_caroccupants$gender[raw_caroccupants$sex_of_casualty == 1] <- "Male"
raw_caroccupants$gender[raw_caroccupants$sex_of_casualty == 2] <- "Female"

raw_caroccupants <- raw_caroccupants[raw_caroccupants$gender!="",]

raw_caroccupants <- raw_caroccupants[raw_caroccupants$age_of_casualty != -1,]
raw_caroccupants$age <- raw_caroccupants$age_of_casualty
raw_caroccupants$age[raw_caroccupants$age>98] <- 99

#raw_caroccupants$agegroup <- ""
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 1] <- "1. 0-5 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 2] <- "2. 6-10 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 3] <- "3. 11-15 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 4] <- "4. 16-20 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 5] <- "5. 21-25 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 6] <- "6. 26-35 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 7] <- "7. 36-45 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 8] <- "8. 46-55 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 9] <- "9. 56-65 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 10] <- "10. 66-75 years"
#raw_caroccupants$agegroup[raw_caroccupants$age_band_of_casualty == 11] <- "11. 0ver 75 years"
#raw_caroccupants <- raw_caroccupants[raw_caroccupants$agegroup!="",]


raw_caroccupants_reduced <- raw_caroccupants[,c(1:3,8:12)]
write.csv(raw_caroccupants_reduced, file = "Casualties information 2015-2020.csv", row.names = FALSE)

rm(raw_caroccupants)
gc()

merged_data <- merge(raw_caroccupants_reduced, raw_acc_final, by.x = c("accident_year", "accident_index", "accident_reference"), by.y = c("accident_year", "accident_index", "accident_reference"))
merged_data$accidentref <- paste(merged_data$accident_year, "-", merged_data$accident_index, "-", merged_data$accident_reference, sep = "")
names(merged_data)

casualtytable <- merged_data[,c(14, 1, 9, 4:8, 10:13)]

# load denominators in memory
library("reshape2")
library("data.table")
# men
male2015 <- read.csv(file = "Male2015UK.csv", header = TRUE, sep = ",")
male2016 <- read.csv(file = "Male2016UK.csv", header = TRUE, sep = ",")
male2017 <- read.csv(file = "Male2017UK.csv", header = TRUE, sep = ",")
male2018 <- read.csv(file = "Male2018UK.csv", header = TRUE, sep = ",")
male2019 <- read.csv(file = "Male2019UK.csv", header = TRUE, sep = ",")
male2020 <- read.csv(file = "Male2020UK.csv", header = TRUE, sep = ",")

male2015 <- male2015[male2015$LSOAName!="",]
col2cvt <- 3:94
male2015[,col2cvt] <- lapply(male2015[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

male2016 <- male2016[male2016$LSOAName!="",]
col2cvt <- 3:94
male2016[,col2cvt] <- lapply(male2016[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

male2017 <- male2017[male2017$LSOAName!="",]
col2cvt <- 3:94
male2017[,col2cvt] <- lapply(male2017[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

male2018 <- male2018[male2018$LSOAName!="",]
col2cvt <- 3:94
male2018[,col2cvt] <- lapply(male2018[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

col2cvt <- 7:98
male2019[,col2cvt] <- lapply(male2019[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

col2cvt <- 7:98
male2020[,col2cvt] <- lapply(male2020[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

# keep list of LSOAs LAs

ladata2018 <- male2020[,c(1:4)]
colnames(ladata2018)[1]<- "LSOACodes"

male2015 <- male2015[,-3]
male2015long <- data.table::melt(male2015, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
male2015long$agevalue  <- as.numeric(gsub("year", "", male2015long$agevalue))
colnames(male2015long)[4]<- "population"
male2015long <- merge(male2015long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
male2015long <- male2015long[,c(1,2,5,6,3,4)]
male2015long$year <- 2015

male2016 <- male2016[,-3]
male2016long <- data.table::melt(male2016, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
male2016long$agevalue  <- as.numeric(gsub("year", "", male2016long$agevalue))
colnames(male2016long)[4]<- "population"
male2016long <- merge(male2016long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
male2016long <- male2016long[,c(1,2,5,6,3,4)]
male2016long$year <- 2016

male2017 <- male2017[,-3]
male2017long <- data.table::melt(male2017, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
male2017long$agevalue  <- as.numeric(gsub("year", "", male2017long$agevalue))
colnames(male2017long)[4]<- "population"
male2017long <- merge(male2017long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
male2017long <- male2017long[,c(1,2,5,6,3,4)]
male2017long$year <- 2017

male2018 <- male2018[,-3]
male2018long <- data.table::melt(male2018, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
male2018long$agevalue  <- as.numeric(gsub("year", "", male2018long$agevalue))
colnames(male2018long)[4]<- "population"
male2018long <- merge(male2018long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
male2018long <- male2018long[,c(1,2,5,6,3,4)]
male2018long$year <- 2018

colnames(male2019)[1]<- "LSOACodes"
male2019 <- male2019[,-c(3:7)]
male2019long <- data.table::melt(male2019, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
male2019long$agevalue  <- as.numeric(gsub("year", "", male2019long$agevalue))
colnames(male2019long)[4]<- "population"
male2019long <- merge(male2019long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
male2019long <- male2019long[,c(1,2,5,6,3,4)]
male2019long$year <- 2019

colnames(male2020)[1]<- "LSOACodes"
male2020 <- male2020[,-c(3:7)]
male2020long <- data.table::melt(male2020, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
male2020long$agevalue  <- as.numeric(gsub("year", "", male2020long$agevalue))
colnames(male2020long)[4]<- "population"
male2020long <- merge(male2020long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
male2020long <- male2020long[,c(1,2,5,6,3,4)]
male2020long$year <- 2020

# women
female2015 <- read.csv(file = "Female2015UK.csv", header = TRUE, sep = ",")
female2016 <- read.csv(file = "Female2016UK.csv", header = TRUE, sep = ",")
female2017 <- read.csv(file = "Female2017UK.csv", header = TRUE, sep = ",")
female2018 <- read.csv(file = "Female2018UK.csv", header = TRUE, sep = ",")
female2019 <- read.csv(file = "Female2019UK.csv", header = TRUE, sep = ",")
female2020 <- read.csv(file = "Female2020UK.csv", header = TRUE, sep = ",")

female2015 <- female2015[female2015$LSOAName!="",]
col2cvt <- 3:94
female2015[,col2cvt] <- lapply(female2015[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

female2016 <- female2016[female2016$LSOAName!="",]
col2cvt <- 3:94
female2016[,col2cvt] <- lapply(female2016[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

female2017 <- female2017[female2017$LSOAName!="",]
col2cvt <- 3:94
female2017[,col2cvt] <- lapply(female2017[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

female2018 <- female2018[female2018$LSOAName!="",]
col2cvt <- 3:94
female2018[,col2cvt] <- lapply(female2018[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

col2cvt <- 7:98
female2019[,col2cvt] <- lapply(female2019[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

col2cvt <- 7:98
female2020[,col2cvt] <- lapply(female2020[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

# keep list of LSOAs LAs

#ladata2018 <- male2020[,c(1:4)]
#colnames(ladata2018)[1]<- "LSOACodes"

female2015 <- female2015[,-3]
female2015long <- data.table::melt(female2015, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
female2015long$agevalue  <- as.numeric(gsub("year", "", female2015long$agevalue))
colnames(female2015long)[4]<- "population"
female2015long <- merge(female2015long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
female2015long <- female2015long[,c(1,2,5,6,3,4)]
female2015long$year <- 2015

female2016 <- female2016[,-3]
female2016long <- data.table::melt(female2016, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
female2016long$agevalue  <- as.numeric(gsub("year", "", female2016long$agevalue))
colnames(female2016long)[4]<- "population"
female2016long <- merge(female2016long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
female2016long <- female2016long[,c(1,2,5,6,3,4)]
female2016long$year <- 2016

female2017 <- female2017[,-3]
female2017long <- data.table::melt(female2017, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
female2017long$agevalue  <- as.numeric(gsub("year", "", female2017long$agevalue))
colnames(female2017long)[4]<- "population"
female2017long <- merge(female2017long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
female2017long <- female2017long[,c(1,2,5,6,3,4)]
female2017long$year <- 2017

female2018 <- female2018[,-3]
female2018long <- data.table::melt(female2018, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
female2018long$agevalue  <- as.numeric(gsub("year", "", female2018long$agevalue))
colnames(female2018long)[4]<- "population"
female2018long <- merge(female2018long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
female2018long <- female2018long[,c(1,2,5,6,3,4)]
female2018long$year <- 2018

colnames(female2019)[1]<- "LSOACodes"
female2019 <- female2019[,-c(3:7)]
female2019long <- data.table::melt(female2019, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
female2019long$agevalue  <- as.numeric(gsub("year", "", female2019long$agevalue))
colnames(female2019long)[4]<- "population"
female2019long <- merge(female2019long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
female2019long <- female2019long[,c(1,2,5,6,3,4)]
female2019long$year <- 2019

colnames(female2020)[1]<- "LSOACodes"
female2020 <- female2020[,-c(3:7)]
female2020long <- data.table::melt(female2020, id.vars = c("LSOACodes","LSOAName"), variable.name = "agevalue")
female2020long$agevalue  <- as.numeric(gsub("year", "", female2020long$agevalue))
colnames(female2020long)[4]<- "population"
female2020long <- merge(female2020long, ladata2018, by.x = c("LSOACodes","LSOAName"), by.y = c("LSOACodes","LSOAName"))
female2020long <- female2020long[,c(1,2,5,6,3,4)]
female2020long$year <- 2020

male_pop <- rbind(male2015long, male2016long, male2017long, male2018long, male2019long, male2020long)
male_pop$gender <- "Male"

female_pop <- rbind(female2015long, female2016long, female2017long, female2018long, female2019long, female2020long)
female_pop$gender <- "Female"

population_data <- rbind(male_pop, female_pop)

population_data <- population_data[order(population_data$LACode2018, population_data$LSOACodes, population_data$year, population_data$gender, population_data$agevalue),]

population_data$population <- as.numeric(gsub(",", "", population_data$population))

aggregate_population_data <- aggregate(list(population_data$population), FUN = sum, by = list(population_data$year, population_data$LACode2018, population_data$LAname2018, population_data$gender, population_data$agevalue))
names(aggregate_population_data) <- c("year", "lacode", "laname" , "gender", "age", "population")

write.csv(aggregate_population_data, file = "Age-Sex Denominators 2015-2020.csv", row.names = FALSE)

# back to casualtytable 

aggregate_casualty_data <- aggregate(list(casualtytable$casualties, casualtytable$fatal, casualtytable$Nonfatal), FUN = sum, by = list(casualtytable$accident_year, casualtytable$local_authority_ons_district, casualtytable$gender, casualtytable$age))
names(aggregate_casualty_data) <- c("year", "lacode" , "gender", "age", "casualties", "fatal", "nonfatal")


data_v1 <- merge(aggregate_population_data, aggregate_casualty_data, by.x =c("year","lacode","gender","age"), by.y=c("year","lacode","gender","age"), all.x = TRUE)
data_v1$casualties[is.na(data_v1$casualties)] <- 0
data_v1$fatal[is.na(data_v1$fatal)] <- 0
data_v1$nonfatal[is.na(data_v1$nonfatal)] <- 0


data_v1$agegroup <- ""
data_v1$agegroup[data_v1$age>= 0 & data_v1$age<= 20] <- 1
data_v1$agegroup[data_v1$age>= 21 & data_v1$age<= 35] <- 2
data_v1$agegroup[data_v1$age>= 36 & data_v1$age<= 50] <- 3
data_v1$agegroup[data_v1$age>= 51 & data_v1$age<= 65] <- 4
data_v1$agegroup[data_v1$age>= 66 & data_v1$age<= 80] <- 5
data_v1$agegroup[data_v1$age>= 81 & data_v1$age<= 100] <- 6

# create two dataset 1. spatialtemporal; 2. spatial but nontemporal
aggregate_casualty_spatialtemp <- aggregate(list(data_v1$population, data_v1$casualties, data_v1$fatal), FUN = sum, by = list(data_v1$year, data_v1$lacode, data_v1$laname, data_v1$gender, data_v1$agegroup))
names(aggregate_casualty_spatialtemp) <- c("year", "lacode" , "laname" , "gender", "agegroup", "population", "casualties", "fatal")

aggregate_casualty_spatialnontemp <- aggregate(list(data_v1$population, data_v1$casualties, data_v1$fatal), FUN = sum, by = list(data_v1$lacode, data_v1$laname, data_v1$gender, data_v1$agegroup))
names(aggregate_casualty_spatialnontemp) <- c("lacode" , "laname" , "gender", "agegroup", "population", "casualties", "fatal")

write.csv(aggregate_casualty_spatialtemp, file = "Road Accidents Spatiotemporal Data 2015-2020.csv", row.names = FALSE)
write.csv(aggregate_casualty_spatialnontemp, file = "Road Accidents Nontemporal Data 2015-2020.csv", row.names = FALSE)

road_accidents <- read.csv("Road Accidents Nontemporal Data 2015-2020.csv", header = TRUE, sep = ",")
road_accidents$onscode <- substr(road_accidents$lacode, 1, 1)
road_accidents <- road_accidents[road_accidents$onscode == "E", -7]

write.csv(road_accidents, file = "Road Accidents in England 2015-2020 NST.csv", row.names = FALSE)

road_accidents <- read.csv("Road Accidents Spatiotemporal Data 2015-2020.csv", header = TRUE, sep = ",")
road_accidents$onscode <- substr(road_accidents$lacode, 1, 1)
road_accidents <- road_accidents[road_accidents$onscode == "E", -8]

write.csv(road_accidents, file = "Road Accidents in England 2015-2020 ST.csv", row.names = FALSE)

# load the shape file LA codes
laboundaries <- read_sf("LAD_MAY_2021_UK_BFE_V2.shp")
laboundaries$onscode <- substr(laboundaries$LAD21CD, 1, 1)
laboundaries_new <- laboundaries[laboundaries$onscode == "E", -c(4:9,11)]

tm_shape(laboundaries_new) + tm_polygons()

st_write(laboundaries_new, "England Local Authority Shapefile.shp")

regionboundaries <- read_sf("england_gor_2011.shp")
st_write(regionboundaries, "England Regions Shapefile.shp")

################################################ script
rm(list = ls())
gc()

# load the libraries required for this exercise
library("sf")
library("tmap")
library("spdep")
library("SpatialEpi")
library("INLA")

# set directory
setwd("/Volumes/Anwar-HHD/GEOG0125/Practicals/Road data")
road_acc_nontemp <- read.csv("Road Accidents in England 2015-2020 NST.csv", header = TRUE, sep = ",")

# This code chunk loads into memory the shapefiles for the English districts and regions 
laboundaries <- read_sf("England Local Authority Shapefile.shp")
regionboundaries <- read_sf("England Regions Shapefile.shp")

# This code chunk creates an empty map of England
#tm_shape(laboundaries) + tm_polygons(alpha = 0, border.alpha = 0.3) + 
#	tm_shape(regionboundaries) + tm_polygons(alpha = 0, border.col = "black") +
#	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

# Expected number route
# Analysis::: Non-temporal

# data preparation
aggregate_road_acc <- aggregate(list(road_acc_nontemp$casualties, road_acc_nontemp$population), FUN = sum, by = list(road_acc_nontemp$lacode, road_acc_nontemp$laname))
names(aggregate_road_acc) <-c("lacode", "laname", "casualties", "population")

# calculate the expected number of casualties adjusted for gender and age groups from original data
# first sort the data in order of lacode, laname, gender and age group
sort_road_acc <- road_acc_nontemp[order(road_acc_nontemp$lacode, road_acc_nontemp$laname, road_acc_nontemp$gender, road_acc_nontemp$agegroup),]

# 2 gender group & 6 age groups - 12 strata in total - calculate expected number of casualties
expected <- expected(population = sort_road_acc$population, cases = sort_road_acc$casualties, n.strata = 12)

aggregate_road_acc$expected <- expected[match(aggregate_road_acc$lacode, unique(sort_road_acc$lacode))]

# check first 20 rows
head(aggregate_road_acc, n=10)

# merge data to shapefile
analysis_data_nontemp <- merge(laboundaries, aggregate_road_acc, by.x = "LAD21CD", by.y="lacode", all.x=TRUE)

# create the neighbourhood matrix as an INLA object
adjacencyMatrix <- poly2nb(analysis_data_nontemp)
nb2INLA("adjacencyObject.adj", adjacencyMatrix)
g <- inla.read.graph(filename = "adjacencyObject.adj")

# specify the id number for the random effect
analysis_data_nontemp$uI <- 1:nrow(analysis_data_nontemp)

# specify priors
prior <- list(
	prec = list(prior = "loggamma", param = c(1, 0.0005)), # set prior for spatial random effects
	phi = list(prior = "loggamma", param = c(1, 0.0005))   # set prior for spatial random effects
)

# formula
formula = casualties ~ 1 + f(uI, model = "bym2", graph = g, hyper = prior)
riskestimates <- inla(formula, family = "poisson", data = analysis_data_nontemp, E = expected, control.predictor = list(compute = TRUE), control.compute = list(dic = TRUE))
summary(riskestimates)

riskratio <- riskestimates$summary.fitted.values
head(riskratio, n=10)

analysis_data_nontemp$RR <- riskratio[, "mean"]       # Relative risk
analysis_data_nontemp$LL <- riskratio[, "0.025quant"] # Lower credibility limit
analysis_data_nontemp$UL <- riskratio[, "0.975quant"] # Upper credibility limit

head(analysis_data_nontemp)

# Get a feel of the lower and highest value of RR
summary(analysis_data_nontemp$RR)
#min:0.007379
#max:3.097010

# Notes 1: create risk categories for legend for map
# Notes 2: RR = 1 is the mid-point of the legend
RiskCategorylist <- c("0.01 to 0.10", "0.11 to 0.25", "0.26 to 0.50", "0.51 to 0.75", "0.76 to 0.99", "1.00 (null value)",
											">1.00 to 1.10", "1.11 to 1.25", "1.26 to 1.50", "1.51 to 1.75", "1.76 to 2.00", "2.01 to 3.00",
											"Above 3.00")

# Create the colours for the above categories - from extreme blues to extreme reds
RRPalette <- c("#33a6fe","#65bafe","#98cffe","#cbe6fe","#dfeffe","#fef9f9","#fed5d5","#feb1b1","#fe8e8e","#fe6a6a","#fe4646","#fe2424","#fe0000")

# Now generate categories
analysis_data_nontemp$RelativeRiskCat <- NA
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR>= 0 & analysis_data_nontemp$RR <= 0.10] <- -5
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 0.10 & analysis_data_nontemp$RR <= 0.25] <- -4
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 0.25 & analysis_data_nontemp$RR <= 0.50] <- -3
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 0.50 & analysis_data_nontemp$RR <= 0.75] <- -2
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 0.75 & analysis_data_nontemp$RR < 1] <- -1
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR == 1] <- 0
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 1.00 & analysis_data_nontemp$RR <= 1.10] <- 1
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 1.10 & analysis_data_nontemp$RR <= 1.25] <- 2
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 1.25 & analysis_data_nontemp$RR <= 1.50] <- 3
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 1.50 & analysis_data_nontemp$RR <= 1.75] <- 4
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 1.75 & analysis_data_nontemp$RR <= 2.00] <- 5
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 2.00 & analysis_data_nontemp$RR <= 3.00] <- 6
analysis_data_nontemp$RelativeRiskCat[analysis_data_nontemp$RR> 3.00 & analysis_data_nontemp$RR <= 250] <- 7

# create categories to define if an area has significant increase or decrease or not all 
analysis_data_nontemp$Significance <- NA
analysis_data_nontemp$Significance[analysis_data_nontemp$LL<1 & analysis_data_nontemp$UL>1] <- 0 # NOT SIGNIFICANT
analysis_data_nontemp$Significance[analysis_data_nontemp$LL==1 | analysis_data_nontemp$UL==1] <- 0 # NOT SIGNIFICANT
analysis_data_nontemp$Significance[analysis_data_nontemp$LL>1 & analysis_data_nontemp$UL>1] <- 1 # SIGNIFICANT INCREASE
analysis_data_nontemp$Significance[analysis_data_nontemp$LL<1 & analysis_data_nontemp$UL<1] <- -1 # SIGNIFICANT DECREASE

# map of relative risk
tm_shape(analysis_data_nontemp) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "Relavtive Risk", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

# map of significance regions
tm_shape(analysis_data_nontemp) + 
	tm_fill("Significance", style = "cat", title = "Significance Categories", palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.10) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))


Prob <- round(sapply(riskestimates$marginals.fitted.values, FUN = function(marg){1 - inla.pmarginal(q = 1, marginal = marg)}), 3)

analysis_data_nontemp$Pr <- Prob
analysis_data_nontemp$ProbCat <- NA
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0 & analysis_data_nontemp$Pr< 0.01] <- 1
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.01 & analysis_data_nontemp$Pr< 0.10] <- 2
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.10 & analysis_data_nontemp$Pr< 0.20] <- 3
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.20 & analysis_data_nontemp$Pr< 0.30] <- 4
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.30 & analysis_data_nontemp$Pr< 0.40] <- 5
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.40 & analysis_data_nontemp$Pr< 0.50] <- 6
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.50 & analysis_data_nontemp$Pr< 0.60] <- 7
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.60 & analysis_data_nontemp$Pr< 0.70] <- 8
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.70 & analysis_data_nontemp$Pr< 0.80] <- 9
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.80 & analysis_data_nontemp$Pr< 0.90] <- 10
analysis_data_nontemp$ProbCat[analysis_data_nontemp$Pr>=0.90 & analysis_data_nontemp$Pr<= 1.00] <- 11

ProbCategorylist <- c("<0.01", "0.01-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49","0.50-0.59", "0.60-0.69", 
											"0.70-0.79", "0.80-0.89", "0.90-1.00")

# map of relative risk
tm_shape(analysis_data_nontemp) + 
	tm_fill("ProbCat", style = "cat", title = "Probability", palette = "OrRd", labels = ProbCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))



# road accident data with temporal structure
road_acc_temp <- read.csv("Road Accidents in England 2015-2020 ST.csv", header = TRUE, sep = ",")
aggregate_road_temp <- aggregate(list(road_acc_temp$casualties, road_acc_temp$population), FUN = sum, by = list(road_acc_temp$year ,road_acc_temp$lacode, road_acc_temp$laname))
names(aggregate_road_temp) <-c("year" ,"lacode", "laname", "casualties", "population")

# calculate expected counts from original data
sort_road_temp <- road_acc_temp[order(road_acc_temp$year, road_acc_temp$lacode, road_acc_temp$laname, road_acc_temp$gender, road_acc_temp$agegroup),]
expected_temp <- expected(population = sort_road_temp$population, cases = sort_road_temp$casualties, n.strata = 12)

# The cleaning is a bit involve here. 
# extract the length  for year = 6
nyrs <- length(unique(sort_road_temp$year))
# expand each lacode 6 times
laE <- rep(unique(sort_road_temp$lacode), each = nyrs)
# similarly
# extract the length for lacode = 309
nla <- length(unique(sort_road_temp$lacode))
# expand each lyear 309 times
yrsE <- rep(unique(sort_road_temp$year), times = nla)

temporalExpected <- data.frame(lacode = laE, year = yrsE, expected=expected_temp)
head(temporalExpected, n = 20)

data_long <- merge(aggregate_road_temp, temporalExpected, by.x=c("year","lacode"), by.y=c("year","lacode"))
analysis_data_temp <- merge(laboundaries, data_long, by.x = "LAD21CD", by.y="lacode", all.x=TRUE)


analysis_data_temp <- analysis_data_temp[order(analysis_data_temp$LAD21CD, analysis_data_temp$year),]
analysis_data_temp$idyear <- analysis_data_temp$year
analysis_data_temp$OBJECTID2 <- analysis_data_temp$OBJECTID

# create the neighbourhood matrix as an INLA object
adjacencyMatrix <- poly2nb(laboundaries)
nb2INLA("adjacencyObject.adj", adjacencyMatrix)
g <- inla.read.graph(filename = "adjacencyObject.adj")

formulaTemp <- casualties ~ f(OBJECTID, model="bym", graph=g) + f(OBJECTID2, idyear, model="iid") + idyear
tempRiskEstimates <- inla(formulaTemp, family = "poisson", data = analysis_data_temp, E = expected, control.predictor = list(link=1), control.compute = list(dic = TRUE))
summary(tempRiskEstimates)

riskratio <- tempRiskEstimates$summary.fitted.values
head(riskratio, n=10)

analysis_data_temp$RR <- riskratio[, "mean"]       # Relative risk
analysis_data_temp$LL <- riskratio[, "0.025quant"] # Lower credibility limit
analysis_data_temp$UL <- riskratio[, "0.975quant"] # Upper credibility limit

# Get a feel of the lower and highest value of RR
summary(analysis_data_temp$RR)
#min:0.00117
#max:166.30556

# Notes 1: create risk categories for legend for map
# Notes 2: RR = 1 is the mid-point of the legend
RiskCategorylist <- c("0.01 to 0.10", "0.11 to 0.25", "0.26 to 0.50", "0.51 to 0.75", "0.76 to 0.99", "1.00 (null value)",
											">1.00 to 1.10", "1.11 to 1.25", "1.26 to 1.50", "1.51 to 1.75", "1.76 to 2.00", "2.01 to 3.00",
											"Above 3.00")

# Create the colours for the above categories - from extreme blues to extreme reds
RRPalette <- c("#33a6fe","#65bafe","#98cffe","#cbe6fe","#dfeffe","#fef9f9","#fed5d5","#feb1b1","#fe8e8e","#fe6a6a","#fe4646","#fe2424","#fe0000")

# Now generate categories RR temporal
analysis_data_temp$RelativeRiskCat <- NA
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR>= 0 & analysis_data_temp$RR <= 0.10] <- -5
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 0.10 & analysis_data_temp$RR <= 0.25] <- -4
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 0.25 & analysis_data_temp$RR <= 0.50] <- -3
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 0.50 & analysis_data_temp$RR <= 0.75] <- -2
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 0.75 & analysis_data_temp$RR < 1] <- -1
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR == 1] <- 0
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 1.00 & analysis_data_temp$RR <= 1.10] <- 1
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 1.10 & analysis_data_temp$RR <= 1.25] <- 2
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 1.25 & analysis_data_temp$RR <= 1.50] <- 3
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 1.50 & analysis_data_temp$RR <= 1.75] <- 4
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 1.75 & analysis_data_temp$RR <= 2.00] <- 5
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 2.00 & analysis_data_temp$RR <= 3.00] <- 6
analysis_data_temp$RelativeRiskCat[analysis_data_temp$RR> 3.00 & analysis_data_temp$RR <= 250] <- 7

# Significance
analysis_data_temp$Significance <- NA
analysis_data_temp$Significance[analysis_data_temp$LL<1 & analysis_data_temp$UL>1] <- 0 # NOT SIGNIFICANT
analysis_data_temp$Significance[analysis_data_temp$LL==1 | analysis_data_temp$UL==1] <- 0 # NOT SIGNIFICANT
analysis_data_temp$Significance[analysis_data_temp$LL>1 & analysis_data_temp$UL>1] <- 1 # SIGNIFICANT INCREASE
analysis_data_temp$Significance[analysis_data_temp$LL<1 & analysis_data_temp$UL<1] <- -1 # SIGNIFICANT DECREASE

Prob <- round(sapply(tempRiskEstimates$marginals.fitted.values, FUN = function(marg){1 - inla.pmarginal(q = 1, marginal = marg)}), 3)

analysis_data_temp$Pr <- Prob
analysis_data_temp$ProbCat <- NA
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0 & analysis_data_temp$Pr< 0.01] <- 1
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.01 & analysis_data_temp$Pr< 0.10] <- 2
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.10 & analysis_data_temp$Pr< 0.20] <- 3
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.20 & analysis_data_temp$Pr< 0.30] <- 4
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.30 & analysis_data_temp$Pr< 0.40] <- 5
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.40 & analysis_data_temp$Pr< 0.50] <- 6
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.50 & analysis_data_temp$Pr< 0.60] <- 7
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.60 & analysis_data_temp$Pr< 0.70] <- 8
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.70 & analysis_data_temp$Pr< 0.80] <- 9
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.80 & analysis_data_temp$Pr< 0.90] <- 10
analysis_data_temp$ProbCat[analysis_data_temp$Pr>=0.90 & analysis_data_temp$Pr<= 1.00] <- 11

ProbCategorylist <- c("<0.01", "0.01-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49","0.50-0.59", "0.60-0.69", 
											"0.70-0.79", "0.80-0.89", "0.90-1.00")

names(analysis_data_temp)
final.output.data <- as.data.frame(st_drop_geometry(analysis_data_temp))

names(final.output.data)

final.output.data <- final.output.data[,c(1,3,5,4,6,8,7,11,12,13,16,14,15,17)]

write.csv(final.output.data, "Road Accidents Output Data.csv", row.names = FALSE)

# split data,frame to years
temp2015 <- analysis_data_temp[analysis_data_temp$year==2015,]
temp2016 <- analysis_data_temp[analysis_data_temp$year==2016,]
temp2017 <- analysis_data_temp[analysis_data_temp$year==2017,]
temp2018 <- analysis_data_temp[analysis_data_temp$year==2018,]
temp2019 <- analysis_data_temp[analysis_data_temp$year==2019,]
temp2020 <- analysis_data_temp[analysis_data_temp$year==2020,]

# generate maps

# map of relative risk
p1 <- tm_shape(temp2015) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "RR 2015", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

p2 <- tm_shape(temp2016) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "RR 2016", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

p3 <- tm_shape(temp2017) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "RR 2017", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

p4 <- tm_shape(temp2018) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "RR 2018", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

p5 <- tm_shape(temp2019) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "RR 2019", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

p6 <- tm_shape(temp2020) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "RR 2020", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tmap_arrange(p1, p2, p3, p4, p5, p6, nrow = 2)


# map of significance regions
s1 <- tm_shape(temp2015) + 
	tm_fill("Significance", style = "cat", title = "Significance 2015", palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.10) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

s2 <- tm_shape(temp2016) + 
	tm_fill("Significance", style = "cat", title = "Significance 2016", palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.10) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

s3 <- tm_shape(temp2017) + 
	tm_fill("Significance", style = "cat", title = "Significance 2017", palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.10) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

s4 <- tm_shape(temp2018) + 
	tm_fill("Significance", style = "cat", title = "Significance 2018", palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.10) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

s5 <- tm_shape(temp2019) + 
	tm_fill("Significance", style = "cat", title = "Significance 2019", palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.10) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

s6 <- tm_shape(temp2020) + 
	tm_fill("Significance", style = "cat", title = "Significance 2020", palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.10) + tm_text("name", size = "AREA") +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

# plot Sign maps together
tmap_arrange(s1, s2, s3, s4, s5, s6, nrow = 2)

tm_shape(analysis_data_nontemp) + 
	tm_fill("ProbCat", style = "cat", title = "Probability", palette = "OrRd", labels = ProbCategorylist) +
	tm_shape(regionboundaries) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))