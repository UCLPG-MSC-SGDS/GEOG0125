


library("gamair")
library("dlnm")

data("chicago")
data("chicagoNMMAPS")

chicagoNMMAPS$timetrans <- chicago$time
chicagoNMMAPS$pm10median <- chicago$pm10median
chicagoNMMAPS$so2median <- chicago$so2median
chicagoNMMAPS$o3median <- chicago$o3median

ggplot(shortdata, aes(x=time, y=death)) + 
	geom_point(size=2, fill="white", colour="black") + 
	theme_classic()

ggplot(shortdata, aes(x=pm10median, y=death)) + 
	geom_point(size=2, fill="white", colour="black") + 
	theme_classic()

ggplot(shortdata, aes(x=so2median, y=death)) + 
	geom_point(size=2, fill="white", colour="black") + 
	theme_classic()

ggplot(shortdata, aes(x=o3median, y=death)) + 
	geom_point(size=2, fill="white", colour="black") + 
	theme_classic()

shortdata$time <- 1:nrow(shortdata)

shortdata <- chicagoNMMAPS[chicagoNMMAPS$year == 1999 | chicagoNMMAPS$year == 2000,]

library("brms")
model.bayes.gam1 <- brm(death ~ s(time, k=3) + s(pm10median, k=3),
	data = shortdata, 
	family = poisson(link = "log"), 
	cores = 6,
	iter = 4000, 
	warmup = 100, 
	thin = 10, 
	refresh = 0,
	control = list(adapt_delta = 0.99))

respiratory_data <- read.csv("/Users/anwarmusah/Desktop/Respiratory_Illness.csv")
respiratory_data$incidence <- respiratory_data$Overall/respiratory_data$TotalPop*1000

ggplot(respiratory_data, aes(x=PM10, y=incidence)) + 
	geom_point(size=2, fill="white", colour="black") + 
	theme_classic()

ggplot(respiratory_data, aes(x=CO, y=incidence)) + 
	geom_point(size=2, fill="white", colour="black") + 
	theme_classic()

ggplot(respiratory_data, aes(x=NO2, y=incidence)) + 
	geom_point(size=2, fill="white", colour="black") + 
	theme_classic()

################################

# clear Rstudio's memory
rm(list = ls())
gc()

# install the following packages
install.packages("brms")
install.packages("mgcv")
install.packages("devtools")
install.packages("ggplot2")
devtools::install_github('gavinsimpson/schoenberg')

# load up packages
library("brms")
library("mgcv")
library("ggplot2")
library("schoenberg")
library("rstan")

# find out how many cores are on you machine as this will be needed in the brms()
parallel::detectCores()

# configure Stan
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# load up the data
respiratory_data <- read.csv("/Users/anwarmusah/Desktop/Respiratory_Illness.csv")

# get prior
prior.list <- get_prior(Overall ~ s(PM10) + s(CO) + s(NO2), data = respiratory_data, family = poisson())

# start timer gauge how long it takes to run a GAM
ptm <- proc.time()

# run a GAM model
model.bayes.gam <- brm(bf(Overall ~ s(PM10) + s(CO) + s(NO2)),
	data = respiratory_data, 
	family = poisson(),
	prior = prior.list,
	cores = 6,
	iter = 8000, 
	warmup = 1000, 
	thin = 10, 
	refresh = 0,
	control = list(adapt_delta = 0.99))

# Stop the clock
proc.time() - ptm

# Report results
summary(model.bayes.gam)

# The predicted effect of PM10 
smooth_plot <- conditional_smooths(model.bayes.gam)

# draw relationship

# extract results

# PM10
beta_coef_PM10 <- data.frame(pm10 = smooth_plot$`mu: s(PM10)`[1], beta = smooth_plot$`mu: s(PM10)`[4], 
	lower = smooth_plot$`mu: s(PM10)`[6], lower = smooth_plot$`mu: s(PM10)`[7])

plot(beta_coef_PM10$PM10, beta_coef_PM10$estimate__, type="l",
	ylab = "Coefficient for PM10", xlab="Observed PM10", ylim=c(-4, 5),
	main = "GAM: Impact Respiratory burden in Turin")

lines(beta_coef_PM10$PM10, beta_coef_PM10$upper__,col="grey")
lines(beta_coef_PM10$PM10, beta_coef_PM10$lower__,col="grey")

# Add null value
abline(h=0, lty = "dashed", col ="red")

# NO2
beta_coef_NO2 <- data.frame(no2 = smooth_plot$`mu: s(NO2)`[1], beta = smooth_plot$`mu: s(NO2)`[4], 
	lower = smooth_plot$`mu: s(NO2)`[6], lower = smooth_plot$`mu: s(NO2)`[7])

plot(beta_coef_NO2$NO2, beta_coef_NO2$estimate__, type="l",
	ylab = "Coefficient for NO2", xlab="Observed NO2", ylim=c(-2, 3),
	main = "GAM: Impact Respiratory burden in Turin")

lines(beta_coef_NO2$NO2, beta_coef_NO2$upper__,col="grey")
lines(beta_coef_NO2$NO2, beta_coef_NO2$lower__,col="grey")

# Add null value
abline(h=0, lty = "dashed", col ="red")

# CO
beta_coef_CO <- data.frame(co = smooth_plot$`mu: s(CO)`[1], beta = smooth_plot$`mu: s(CO)`[4], 
	lower = smooth_plot$`mu: s(CO)`[6], lower = smooth_plot$`mu: s(CO)`[7])

plot(beta_coef_CO$CO, beta_coef_CO$estimate__, type="l",
	ylab = "Coefficient for CO", xlab="Observed CO", ylim=c(-8, 8),
	main = "GAM: Impact Respiratory burden in Turin")

lines(beta_coef_CO$CO, beta_coef_CO$upper__,col="grey")
lines(beta_coef_CO$CO, beta_coef_CO$lower__,col="grey")

# Add null value
abline(h=0, lty = "dashed", col ="red")





pp_check(model.bayes.gam, type="ecdf_overlay")

# This prints the stancode
stancode(model.bayes.gam)