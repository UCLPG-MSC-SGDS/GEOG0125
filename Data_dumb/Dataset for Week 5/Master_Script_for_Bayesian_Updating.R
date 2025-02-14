# use objects: Bayesian.model.updated1
# use objects: Bayesian.model.updated2

# create a handle for the Bayesian updated results
updated_results <- Bayesian.model.updated1

###### DIAGNOSTICS
# print full table to avoid some rows from being omitted.
options(max.print = 100000)
# print the all results for quick diagnostics
print(updated_results, 
	pars=c("alpha", "beta", "rr_alpha", "rr_beta", "rr_mu", "sigma", "phi", "lp__"), 
	probs=c(0.025, 0.975)
)

# diagnostic check on the rHats - put everything into a data frame
diagnostic.checks <- as.data.frame(
	summary(updated_results, pars=c("alpha", "beta", "rr_alpha", "rr_beta", "rr_mu", "sigma", "phi", "lp__"), 
		probs=c(0.025, 0.5, 0.975))$summary
)
# create binary variable
diagnostic.checks$valid <- ifelse(diagnostic.checks$Rhat < 1.05, 1, 0)
# tabulate it
table(diagnostic.checks$valid)

# check across each chains for samples that have diverged (should not exceed 10% of the total 30000)
d1 <- get_sampler_params(updated_results, inc_warmup = F)[[1]][, 'divergent__'] %>% sum() # chain 1
d2 <- get_sampler_params(updated_results, inc_warmup = F)[[2]][, 'divergent__'] %>% sum() # chain 2
d3 <- get_sampler_params(updated_results, inc_warmup = F)[[3]][, 'divergent__'] %>% sum() # chain 3
d4 <- get_sampler_params(updated_results, inc_warmup = F)[[4]][, 'divergent__'] %>% sum() # chain 4
d5 <- get_sampler_params(updated_results, inc_warmup = F)[[5]][, 'divergent__'] %>% sum() # chain 5
d6 <- get_sampler_params(updated_results, inc_warmup = F)[[6]][, 'divergent__'] %>% sum() # chain 6

# tells you how many samples per chain did not converged (i.e., diverged)
d1
d2
d3
d4
d5
d6

# Check the proportion of diverged samples
(d1+d2+d3+d4+d5+d6)/30000 * 100 > 10

###### GLOBAL RESULTS
# print summary table
print(updated_results, 
	pars=c("rr_alpha", "rr_beta", "sigma"), 
	probs=c(0.025, 0.975)
)

# compute exceedance probability for the intercept/baseline risk is bigger null value 1
threshold.intercept <- function(x){mean(x > 1.00)}
rr_alpha.exc.probs <- updated_results %>% spread_draws(rr_alpha) %>% 
	summarise(rr_alpha=threshold.intercept(rr_alpha)) %>%
	pull(rr_alpha)

# compute exceedance probability for each of the independent variables/risk are bigger than 1
threshold.coefficients <- function(x){mean(x > 1.00)}
rr_beta.exc.probs <- updated_results %>% spread_draws(rr_beta[i]) %>% 
	group_by(i) %>% summarise(rr_beta=threshold.coefficients(rr_beta)) %>%
	pull(rr_beta)

# reports exceedance probabilities
rr_alpha.exc.probs
rr_beta.exc.probs

# creating a table
# Step 1: create the names in the appropriate order as there were modelled in Stan
names <- c("Baseline Risk", "Temperature", "Precipitation", "NDVI", "Urbanisation")

# Step 2: now, pull all global relative risk results into the object called 'results'
results <- as.data.frame(summary(updated_results, pars = c("rr_alpha", "rr_beta"), probs = c(0.025, 0.975))$summary)
results$variables <- names
row.names(results) <- 1:nrow(results)

# Step 3: fudge with the results to make it look clean
results <- results[,c(8, 1, 4, 5, 6, 7)]
results$mean <- round(results$mean, 4)
colnames(results)[2] <- "coefficient"
colnames(results)[3] <- "lower95"
colnames(results)[4] <- "upper95"
colnames(results)[5] <- "ess"
colnames(results)[6] <- "rhat"
results$lower95<- round(results$lower95, 4)
results$upper95 <- round(results$upper95, 4)
results$ess <- round(results$ess, 0)
results$rhat <- round(results$rhat, 4)

# Step 4: stitch the credibility results as text so it reads as (95% CrI: XXX to XXX)
results$beta_95CrI <- paste(results$coefficient, " (95% CrI: ", results$lower95, " to ", results$upper95, ")", sep = "")

# Step 5: pull the exceedance probabilities into the result table
a <- c(rr_alpha.exc.probs, rr_beta.exc.probs)
results$ExceedanceProb <- round(a, 4)
results$Uncertainty <- paste("Prob = ", results$ExceedanceProb, sep = "")

# Step 6: finally, more fudging around with the results to make it look cleaner and done!
final.output <- results[,c(1, 7, 9, 5, 6)]

# view final output and... presto! not too shabby!
final.output

# Now export Global table results into a .csv file
# make sure to update the name of the saved file!
write.csv(final.output, file = "Global_Result_2017-2.csv", row.names = FALSE)

### Mapping

# extraction key posterior results for the generated quantities 
relativeRisk.results <- as.data.frame(summary(updated_results, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary)
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

# template for merging spatial risk data
spatial.data <- campina_grande_neighbourhoods # it will overwrite the previous merge!
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

# compute the exceedance probabilities for the areas
threshold <- function(x){mean(x > 1.00)}
excProbrr <- updated_results %>% spread_draws(rr_mu[i]) %>% 
	group_by(i) %>% summarise(rr_mu=threshold(rr_mu)) %>%
	pull(rr_mu)

# insert the exceedance values into the spatial data frame
spatial.data$excProb <- excProbrr

# create the map labels for the probabilities
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

# use these functions to understand the distribution of these risk estimates
summary(spatial.data$rr)
hist(spatial.data$rr)
table(spatial.data$Significance)

# check to see if legend scheme is balanced
table(spatial.data$ProbCat)

# map of relative risk
rr_map <- tm_shape(spatial.data) + 
	tm_fill("rr", style = "cont", title = "Relavtive Risk [RR]", palette = "OrRd") +
	tm_shape(campina_grande_neighbourhoods) + tm_polygons(alpha = 0.10) + tm_text("NeighNum") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

# map of significance regions
sg_map <- tm_shape(spatial.data) + 
	tm_fill("Significance", style = "cat", title = "Significance Categories", 
		palette = c("#33a6fe", "white", "#fe0000"), labels = c("Significantly low", "Not Significant", "Significantly high")) +
	tm_shape(campina_grande_neighbourhoods) + tm_polygons(alpha = 0.10) + tm_text("NeighNum") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

# map of exceedance probabilities
ep_map <- tm_shape(spatial.data) + 
	tm_fill("ProbCat", style = "cont", title = "Probability", palette = "GnBu", labels = ProbCategorylist) +
	tm_shape(campina_grande_neighbourhoods) + tm_polygons(alpha = 0.10) + tm_text("NeighNum") +
	tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1) +
	tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

# make sure to update the name of the saved file!
tmap_save(rr_map, "rr_2017-2.png", height = 12)
tmap_save(sg_map, "sign_2017-2.png", height = 12)
tmap_save(ep_map, "ep_2017-2.png", height = 12)

# retain updated map dataset for future
updated_map_result <- st_drop_geometry(spatial.data)
# save updated results
# make sure to update the name of the saved file!
write.csv(updated_map_result, file = "Updated 2017-2.csv", row.names = FALSE)