rm(list = ls())
gc()

setwd("/Users/anwarmusah/Documents/Websites/GEOG0125/Data_dumb/Dataset for Week 4")

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

# load the shape files
england_LA_shp <- read_sf("England Local Authority Shapefile.shp")
england_Region_shp <- read_sf("England Regions Shapefile.shp")

# load in the cross-sectional road accident dataset
road_accidents <- read.csv("Road Casualties Data 2015-2020.csv")

# calculate the expected number of cases
road_accidents$ExpectedNum <- round(expected(population = road_accidents$Population, cases = road_accidents$Casualties, n.strata = 1), 0)

# merge the attribute table to the shapefile
spatial.data <- merge(england_LA_shp, road_accidents, by.x = c("LAD21CD", "LAD21NM"), by.y = c("LAD21CD", "LAD21NM"), all.x = TRUE)
# reordering the columns
spatial.data <- spatial.data[, c(3,1,2,4,5,7,6)]
# need to be coerced into a spatial object
sp.object <- as(spatial.data, "Spatial")

# needs to be coerced into a matrix object
adjacencyMatrix <- shape2mat(sp.object)
# we extract the components for the ICAR model
extractComponents <- prep_icar_data(adjacencyMatrix)

n <- as.numeric(extractComponents$group_size)
nod1 <- extractComponents$node1
nod2 <- extractComponents$node2
n_edges <- as.numeric(extractComponents$n_edges)

y <- spatial.data$Casualties
x <- spatial.data$IMDScore
e <- spatial.data$ExpectedNum

# put all components into a list object
stan.spatial.dataset <- list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, Y=y, X=x, Off_set=e)

# Start the clock
ptm <- proc.time()
# compile linear regression model for now
icar_poisson_fit = stan("/Users/anwarmusah/Documents/Websites/GEOG0125/Scripts_RStan/Week_4_ICAR_model.stan", 
	data=stan.spatial.dataset, iter=20000, chains=6, control = list(max_treedepth = 12), verbose = FALSE)
# Stop the clock
proc.time() - ptm

# show first 6 rows only instead of the full 307
head(summary(icar_poisson_fit, pars=c("phi"), probs=c(0.025, 0.975))$summary)
# print full table to avoid some rows from being omitted.
options(max.print = 100000)
# print the results
print(icar_poisson_fit, pars=c("alpha", "beta", "rr_alpha", "rr_beta", "rr_mu", "sigma"), probs=c(0.025, 0.975))

# diagnostic check on the rHats - put everything into a data frame
diagnostic.checks <- as.data.frame(summary(icar_poisson_fit, pars=c("alpha", "beta", "rr_alpha", "rr_beta", "rr_mu", "sigma", "phi", "lp__"), probs=c(0.025, 0.5, 0.975))$summary)
# create binary variable
diagnostic.checks$valid <- ifelse(diagnostic.checks$Rhat < 1.05, 1, 0)
# tabulate it
table(diagnostic.checks$valid)

# show first 6 rows only instead of the full 307
summary(icar_poisson_fit, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary

# extraction key posterior results for the generated quantities 
relativeRisk.results <- as.data.frame(summary(icar_poisson_fit, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary)
# now cleaning up this table up
# first, insert clean row numbers to new data frame
row.names(relativeRisk.results) <- 1:nrow(relativeRisk.results)
# second, rearrange the columns into order
relativeRisk.results <- relativeRisk.results[, c(1,4,5,7)]
# third, rename the columns appropriately
colnames(relativeRisk.results)[1] <- "rr"
colnames(relativeRisk.results)[2] <- "rrlower"
colnames(relativeRisk.results)[3] <- "rrupper"
colnames(relativeRisk.results)[4] <- "rHAT"

# view clean table 
head(relativeRisk.results)

# now, we proceed to generate our risk maps
# align the results to the areas in shapefile
spatial.data$rr <- relativeRisk.results[, "rr"]
spatial.data$rrlower <- relativeRisk.results[, "rrlower"]
spatial.data$rrupper <- relativeRisk.results[, "rrupper"]

# create categories to define if an area has significant increase or decrease in risk, or nothing all 
spatial.data$Significance <- NA
spatial.data$Significance[spatial.data$rrlower<1 & spatial.data$rrupper>1] <- 0    # NOT SIGNIFICANT
spatial.data$Significance[spatial.data$rrlower==1 | spatial.data$rrupper==1] <- 0  # NOT SIGNIFICANT
spatial.data$Significance[spatial.data$rrlower>1 & spatial.data$rrupper>1] <- 1    # SIGNIFICANT INCREASE
spatial.data$Significance[spatial.data$rrlower<1 & spatial.data$rrupper<1] <- -1   # SIGNIFICANT DECREASE


# For map design for the relative risk -- you want to understand or get a handle on what the distribution for risks look like
# this would inform you of how to create the labelling for the legends when make a map in tmap
summary(spatial.data$rr)
hist(spatial.data$rr)

# creating the labels
RiskCategorylist <- c(">0.0 to 0.25", "0.26 to 0.50", "0.51 to 0.75", "0.76 to 0.99", "1.00 & <1.01",
	"1.01 to 1.10", "1.11 to 1.25", "1.26 to 1.50", "1.51 to 1.75", "1.76 to 2.00", "2.01 to 3.00")

# next, we are creating the discrete colour changes for my legends and want to use a divergent colour scheme
# scheme ranges from extreme dark blues to light blues to white to light reds to extreme dark reds
# you can pick your own colour choices by checking out this link [https://colorbrewer2.org]

RRPalette <- c("#65bafe","#98cffe","#cbe6fe","#dfeffe","white","#fed5d5","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15")

# categorising the risk values to match the labelling in RiskCategorylist object
spatial.data$RelativeRiskCat <- NA
spatial.data$RelativeRiskCat[spatial.data$rr>= 0 & spatial.data$rr <= 0.25] <- -4
spatial.data$RelativeRiskCat[spatial.data$rr> 0.25 & spatial.data$rr <= 0.50] <- -3
spatial.data$RelativeRiskCat[spatial.data$rr> 0.50 & spatial.data$rr <= 0.75] <- -2
spatial.data$RelativeRiskCat[spatial.data$rr> 0.75 & spatial.data$rr < 1] <- -1
spatial.data$RelativeRiskCat[spatial.data$rr>= 1.00 & spatial.data$rr < 1.01] <- 0
spatial.data$RelativeRiskCat[spatial.data$rr>= 1.01 & spatial.data$rr <= 1.10] <- 1
spatial.data$RelativeRiskCat[spatial.data$rr> 1.10 & spatial.data$rr <= 1.25] <- 2
spatial.data$RelativeRiskCat[spatial.data$rr> 1.25 & spatial.data$rr <= 1.50] <- 3
spatial.data$RelativeRiskCat[spatial.data$rr> 1.50 & spatial.data$rr <= 1.75] <- 4
spatial.data$RelativeRiskCat[spatial.data$rr> 1.75 & spatial.data$rr <= 2.00] <- 5
spatial.data$RelativeRiskCat[spatial.data$rr> 2.00 & spatial.data$rr <= 10] <- 6

# check to see if legend scheme is balanced - if a number is missing that categorisation is wrong!
table(spatial.data$RelativeRiskCat)

# map of relative risk
rr_map <- tm_shape(spatial.data) + 
	tm_fill("RelativeRiskCat", style = "cat", title = "Relavtive Risk", palette = RRPalette, labels = RiskCategorylist) +
	tm_shape(england_Region_shp) + tm_polygons(alpha = 0.05) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

# map of significance regions
sg_map <- tm_shape(spatial.data) + 
	tm_fill("Significance", style = "cat", title = "Significance Categories", 
		palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(england_Region_shp) + tm_polygons(alpha = 0.10) + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

# create side-by-side plot
tmap_arrange(rr_map, sg_map, ncol = 2, nrow = 1)

# extract the exceedence probabilities from the icar_possion_fit object
# compute the probability that an area has a relative risk ratio > 1.0
threshold <- function(x){mean(x > 1.00)}
excProbrr <- icar_poisson_fit %>% spread_draws(rr_mu[i]) %>% 
	group_by(i) %>% summarise(rr_mu=threshold(rr_mu)) %>%
	pull(rr_mu)

# insert the exceedance values into the spatial data frame
spatial.data$excProb <- excProbrr

# create the labels for the probabilities
ProbCategorylist <- c("<0.01", "0.01-0.09", "0.10-0.19", "0.20-0.29", "0.30-0.39", "0.40-0.49","0.50-0.59", "0.60-0.69", "0.70-0.79", "0.80-0.89", "0.90-0.99", "1.00")

# categorising the probabilities in bands of 10s
spatial.data$ProbCat <- NA
spatial.data$ProbCat[spatial.data$excProb>=0 & spatial.data$excProb< 0.01] <- 1
spatial.data$ProbCat[spatial.data$excProb>=0.01 & spatial.data$excProb< 0.10] <- 2
spatial.data$ProbCat[spatial.data$excProb>=0.10 & spatial.data$excProb< 0.20] <- 3
spatial.data$ProbCat[spatial.data$excProb>=0.20 & spatial.data$excProb< 0.30] <- 4
spatial.data$ProbCat[spatial.data$excProb>=0.30 & spatial.data$excProb< 0.40] <- 5
spatial.data$ProbCat[spatial.data$excProb>=0.40 & spatial.data$excProb< 0.50] <- 6
spatial.data$ProbCat[spatial.data$excProb>=0.50 & spatial.data$excProb< 0.60] <- 7
spatial.data$ProbCat[spatial.data$excProb>=0.60 & spatial.data$excProb< 0.70] <- 8
spatial.data$ProbCat[spatial.data$excProb>=0.70 & spatial.data$excProb< 0.80] <- 9
spatial.data$ProbCat[spatial.data$excProb>=0.80 & spatial.data$excProb< 0.90] <- 10
spatial.data$ProbCat[spatial.data$excProb>=0.90 & spatial.data$excProb< 1.00] <- 11
spatial.data$ProbCat[spatial.data$excProb == 1.00] <- 12

# check to see if legend scheme is balanced
table(spatial.data$ProbCat)

# map of exceedance probabilities
tm_shape(spatial.data) + 
	tm_fill("ProbCat", style = "cat", title = "Probability", palette = "GnBu", labels = ProbCategorylist) +
	tm_shape(england_Region_shp) + tm_polygons(alpha = 0.05, border.col = "black") + tm_text("name", size = "AREA") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 0.8, legend.text.size = 0.7) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))


results <- icar_poisson_fit %>% spread_draws(rr_mu) %>% pull(rr_mu)

exceedance_results <- results[[17]]

# create a graph i.e., histogram
hist(exceedance_results)
plot(density(exceedance_results), main = "Exceedance Probability", xlab = "Rutland [Pr(RR > 1.00)]" , ylab = "Posterior Density (Plausibility)")


# Generate density estimates
dens <- density(exceedance_results)
percentile_0_25 <- quantile(exceedance_results, probs = 0.025)
percentile_05 <- quantile(exceedance_results, probs = 0.5)
percentile_97_5 <- quantile(exceedance_results, probs = 0.975)

percentile_0_25
percentile_05
percentile_97_5

# Plot the density curve
plot(dens, main = "Rutland, RR: 0.93 (95% CrI: 0.45-1.85) [Range: 0.005-3.06]", 
	xlab = "Shaded red area is the exceedance probability for Rutland [Pr(RR > 1.40)]", 
	ylab = "Posterior Density (Plausibility)")

# Shade the area where x > 1.40
x_shade <- dens$x[dens$x > 1.40]  # Select x values greater than 1.40
y_shade <- dens$y[dens$x > 1.40]  # Corresponding y values

polygon(c(x_shade, rev(x_shade)), 
	c(y_shade, rep(0, length(y_shade))), 
	col = rgb(1, 0, 0, 0.5), border = NA)  # Semi-transparent red shading

# add vertical line for RR null value = 1
abline(v=1.00, lwd = 1.5, col="black", lty = "dashed")
abline(v=0.93, lwd = 1.5, col="red")
abline(v=percentile_0_25, lwd = 1.5, col="grey")
abline(v=percentile_97_5, lwd = 1.5, col="grey")

library(pracma)

# Calculate the area under the curve (probability) for x > 1.40
area_shaded <- trapz(x_shade, y_shade)