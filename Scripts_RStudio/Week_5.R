rm(list = ls())
gc()

setwd("/Users/anwarmusah/Documents/Websites/GEOG0125/Data_dumb/Dataset for Week 5")

# Load the packages with library()
library("sf")
library("tmap")
library("spdep")
library("rstan")
library("geostan")
library("SpatialEpi")
library("tidybayes")
library("tidyverse")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# load in the shapefiles
england_LA_shp <- read_sf("England Local Authority Shapefile.shp")
england_Region_shp <- read_sf("England Regions Shapefile.shp")

# prepare the dataset
data.expand <- st_drop_geometry(england_LA_shp)
# replicate row six times
data.expand <- data.expand[rep(seq_len(nrow(data.expand)), each = 6), ]
data.expand$year <- rep(2015:2020, times = nrow(england_LA_shp))
# upload the spatially- & temporally structured dataset
data <- read.csv("Road Accidents in England 2015-2020 ST.csv")

# aggregate the counts to produce a clean file
aggregated.data <- aggregate(
	list(data$population, data$casualties), 
	FUN = sum, 
	by = list(data$year, data$lacode, data$laname)
	)

# rename columns
colnames(aggregated.data)[1] <- "year"
colnames(aggregated.data)[2] <- "LAD21CD" 
colnames(aggregated.data)[3] <- "LAD21NM"
colnames(aggregated.data)[4] <- "population"
colnames(aggregated.data)[5] <- "casualties"
	
# calculate the expected number of casualties	
aggregated.data$expectednum <- round(
	expected(
		population = aggregated.data$population,
		cases = aggregated.data$casualties, 
		n.strata = 1
		),
	0
	)

# align casualties information with updated areas data frame from good shapefile
casualties.long <- merge(
	data.expand,
	aggregated.data,
	by.x = c("LAD21CD", "LAD21NM", "year"),
	by.y = c("LAD21CD", "LAD21NM", "year"),
	all.x = TRUE
)

# arrange columns into good order
casualties.long <- casualties.long[, c(4, 1, 2, 3, 5:7)]

# save dataset
write.csv(casualties.long, file = "Road Accidents 2015-2020 ST.csv", row.names = FALSE)

# need to be coerced into a spatial object
sp.object <- as(england_LA_shp, "Spatial")

# needs to be coerced into a matrix object
adjacencyMatrix <- shape2mat(sp.object)
# we extract the components for the ICAR model
extractComponents <- prep_icar_data(adjacencyMatrix)

n <- as.numeric(extractComponents$group_size)
nod1 <- extractComponents$node1
nod2 <- extractComponents$node2
n_edges <- as.numeric(extractComponents$n_edges)

