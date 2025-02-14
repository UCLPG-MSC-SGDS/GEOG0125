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
library("dplyr")
library("tidyr")

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# load in the shapefiles
NorthWest_LA_shp <- read_sf("North West Local Authorities Shapefile.shp")

# prepare the dataset
data.expand <- st_drop_geometry(NorthWest_LA_shp)
# create i index to represent the area 'i'
data.expand$i <- 1:nrow(data.expand)
# replicate row six times
data.expand <- data.expand[rep(seq_len(nrow(data.expand)), each = 6), ]
# create the variable for year from 2015 to 2020, inclusively
data.expand$year <- rep(2015:2020, times = nrow(NorthWest_LA_shp))
# create the index for time 't'
data.expand$t <- data.expand$year-2014

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

# show names
names(casualties.long)

# arrange columns into good order
casualties.long <- casualties.long[, c(4, 1, 2, 3, 7:9, 5, 6)]

# save dataset
write.csv(casualties.long, file = "North West Road Accidents 2015-2020 ST.csv", row.names = FALSE)
##################################################################################################

casualties.long <- read.csv("North West Road Accidents 2015-2020 ST.csv")

# data cleaning convert from long to wide format

# reshape data for Y into wide format
Y_wide <- casualties.long %>%
	select(i, t, casualties) %>%
	pivot_wider(
		names_from = t,           # pivot on the time index
		values_from = casualties  # fill the new columns with casualties
	) %>%
	arrange(i)

# remove the i index column once it’s pivoted
Y_wide_mat <- Y_wide %>%
	select(-i) %>%
	as.matrix()
# the above result is a Y_wide_mat is a 50 x 6 matrix i.e., 50 areas by 6 time points.

# reshape data for expected number column into wide format
Offset_wide <- casualties.long %>%
	select(i, t, expectednum) %>%
	pivot_wider(
		names_from = t,
		values_from = expectednum
	) %>%
	arrange(i)

Offset_wide_mat <- Offset_wide %>%
	select(-i) %>%
	as.matrix()

# create the spatial adjacency matrix

# need to be coerced into a spatial object
sp.object <- as(NorthWest_LA_shp, "Spatial")
# needs to be coerced into a matrix object
adjacencyMatrix <- shape2mat(sp.object)
# we extract the components for the ICAR model
extractComponents <- prep_icar_data(adjacencyMatrix)

# extract the components from the spatial area
n <- as.numeric(extractComponents$group_size)
nod1 <- extractComponents$node1
nod2 <- extractComponents$node2
n_edges <- as.numeric(extractComponents$n_edges)

stan.spatiotemporal.dataset <- list(
	N = n,
	T = max(unique(casualties.long$t)),
	N_edges = n_edges,
	node1 = nod1,
	node2 = nod2,
	Y = Y_wide_mat,
	Off_set = Offset_wide_mat
)

# Start the clock
ptm <- proc.time()
# compile linear regression model for now
spatiotempal.model = stan("Spatiotemporal_ICAR_model.stan", 
	data=stan.spatiotemporal.dataset, iter=10000, chains=6)
# Stop the clock
proc.time() - ptm