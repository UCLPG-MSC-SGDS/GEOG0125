

rm(list = ls())
gc()

library("rstan")
library('tidybayes')
library('tidyverse')

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

setwd("/Users/anwarmusah/Documents/Websites/GEOG0125/Data_dumb")

WHO_AFRO_Cholera <- read.csv("WHO-AFRO Cholera 2000-17.csv")

# create dataset for Stan
stan.cholera.dataset <- list(
	N=nrow(WHO_AFRO_Cholera), 
	Country=max(unique(WHO_AFRO_Cholera$country_id)),
	CountryID=as.integer(WHO_AFRO_Cholera$country_id),
	Cholera=WHO_AFRO_Cholera$cholera,
	Log_Population = log(WHO_AFRO_Cholera$population),
	Water = WHO_AFRO_Cholera$basic_water,
	Sanitation = WHO_AFRO_Cholera$basic_sanitation,
	GDP = WHO_AFRO_Cholera$gdp,
	Rainfall = WHO_AFRO_Cholera$rainfall,
	Temperature = WHO_AFRO_Cholera$temperature,
	Overdispersion_Parameter = 0.4
)

# Start the clock
ptm <- proc.time()
# compile linear regression model for now
bayesian.hierarchical.model = stan("Cholera Script.stan", data=stan.cholera.dataset, iter=40000, chains=6, control = list(adapt_delta = 0.99), verbose = FALSE)
# Stop the clock
proc.time() - ptm


# print full table to avoid some rows from being omitted.
options(max.print = 100000)

# print full table to show all results
print(bayesian.hierarchical.model)

# print table to reports the relative risk ratios for Cholera
print(bayesian.hierarchical.model, 
	probs=c(0.025, 0.975), 
	pars = c("gamma00_RR", "gamma01_RR", "gamma02_RR", "beta3_RR", "beta4_RR", "beta5_RR",
		"group_intercept_sd", "group_slope_water_sd", "group_slope_sanitation_sd"))

# This portion of the code computes the exceedance probabilities for the intercept & each beta coefficient
threshold.gamma00_RR <- function(x){mean(x > 1.00)}
gamma00_RR.exc.probs <- bayesian.hierarchical.model %>% spread_draws(gamma00_RR) %>% 
	summarise(gamma00_RR=threshold.gamma00_RR(gamma00_RR)) %>%
	pull(gamma00_RR)

threshold.gamma01_RR <- function(x){mean(x > 1.00)}
gamma01_RR.exc.probs <- bayesian.hierarchical.model %>% spread_draws(gamma01_RR) %>% 
	summarise(gamma01_RR=threshold.gamma01_RR(gamma01_RR)) %>%
	pull(gamma01_RR)

threshold.gamma02_RR <- function(x){mean(x > 1.00)}
gamma02_RR.exc.probs <- bayesian.hierarchical.model %>% spread_draws(gamma02_RR) %>% 
	summarise(gamma02_RR=threshold.gamma02_RR(gamma02_RR)) %>%
	pull(gamma02_RR)

threshold.beta3_RR <- function(x){mean(x > 1.00)}
beta3_RR.exc.probs <- bayesian.hierarchical.model %>% spread_draws(beta3_RR) %>% 
	summarise(beta3_RR=threshold.beta3_RR (beta3_RR)) %>%
	pull(beta3_RR)

threshold.beta4_RR <- function(x){mean(x > 1.00)}
beta4_RR.exc.probs <- bayesian.hierarchical.model %>% spread_draws(beta4_RR) %>% 
	summarise(beta4_RR=threshold.beta4_RR(beta4_RR)) %>%
	pull(beta4_RR)

threshold.beta5_RR <- function(x){mean(x > 1.00)}
beta5_RR.exc.probs <- bayesian.hierarchical.model %>% spread_draws(beta5_RR) %>% 
	summarise(beta5_RR=threshold.beta5_RR(beta5_RR)) %>%
	pull(beta5_RR)

# report exceedance probability results
gamma00_RR.exc.probs
gamma01_RR.exc.probs
gamma02_RR.exc.probs
beta3_RR.exc.probs
beta4_RR.exc.probs
beta5_RR.exc.probs

names <- c("baseline", "water", "sanitation", "gdp", "rainfall", "temperature", "ben_w", "bur_w", "drc_w", "gha_w", "ivc_w", "ken_w",
	"mal_w", "moz_w", "ner_w", "nir_w", "som_w", "tan_w", "tog_w", "ben_s", "bur_s", "drc_s", "gha_s", "ivc_s", "ken_s",
	"mal_s", "moz_s", "ner_s", "nir_s", "som_s", "tan_s", "tog_s")

results <- as.data.frame(summary(bayesian.hierarchical.model, probs=c(0.025, 0.975), pars = c("gamma00_RR" , "gamma01_RR", "gamma02_RR", "beta3_RR", "beta4_RR", "beta5_RR", "beta01_RR", "beta02_RR"))$summary)

results$variables <- names
row.names(results) <- 1:nrow(results)

results <- results[,c(8, 1, 4, 5, 6, 7)]
results$mean <- round(results$mean, 2)
colnames(results)[2] <- "RelativeRisks"
colnames(results)[3] <- "lower95"
colnames(results)[4] <- "upper95"
colnames(results)[5] <- "ess"
colnames(results)[6] <- "rhat"
results$lower95<- round(results$lower95, 2)
results$upper95 <- round(results$upper95, 2)
results$ess <- round(results$ess, 0)
results$rhat <- round(results$rhat, 2)


# This portion of the code computes the exceedance probabilities for the varying slope coefficients for each country

threshold.beta01_RR <- function(x){mean(x > 1.00)}
beta01_RR.exc.probs <- bayesian.hierarchical.model %>% spread_draws(beta01_RR[i]) %>% 
	group_by(i) %>% summarise(beta01_RR=threshold.beta01_RR(beta01_RR)) %>%
	pull(beta01_RR)

threshold.beta02_RR <- function(x){mean(x > 1.00)}
beta02_RR.exc.probs <- bayesian.hierarchical.model %>% spread_draws(beta02_RR[i]) %>% 
	group_by(i) %>% summarise(beta02_RR=threshold.beta02_RR(beta02_RR)) %>%
	pull(beta02_RR)

table <- results
table$RR_95CrI <- paste(table$RelativeRisks, " (95% CrI: ", table$lower95, " to ", table$upper95, ")", sep = "")
probs <- c(gamma00_RR.exc.probs, gamma01_RR.exc.probs, gamma02_RR.exc.probs, beta3_RR.exc.probs, beta4_RR.exc.probs, beta5_RR.exc.probs, beta01_RR.exc.probs, beta01_RR.exc.probs)
table$ExceedProbs <- round(probs, 2)
table$ESS_Rhat <- paste(table$ess, " (Rhat = ", table$rhat, " < 1.05)", sep = "")
finaltable <- table[,c(1,7,8,9)]
