
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

houseprice <- read.csv("London LSOA 2015 data.csv")

# transform them all to log-scale
houseprice$AVEPRICE <- log(houseprice$AVEPRICE)
houseprice$AVEINCOME <- log(houseprice$AVEINCOME)
houseprice$IMDSCORE <- log(houseprice$IMDSCORE)
houseprice$PTAINDEX <- log(houseprice$PTAINDEX)

data <- list(N=4968, k=4, logprice=houseprice[,2], logincome=houseprice[,3], logdeprivation=houseprice[,4], logptaindex=houseprice[,5])

# run stan model
results = stan(file = "Stan script for house price task.stan", data = data, chains = 3, iter = 3000, warmup = 500, thin = 10)
print(results, pars = c("beta", "sigma"))