
setwd("/Volumes/Anwar-HHD/GEOG0125/Github/2022_2023/Scripts_RStudio")

library('rstan')
library('tidybayes')
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# random-intercept-only model

# There are 4 classrooms each with 50 Students. There is a total of 200
# Students each with a unique `StudentID` number and `GroupID` number i.e.,
# classroom they are in.

# Here, we will need to prepare a create the stan dataset

# Extract the following information
# total number of students (level 1)
# maximum number of group (level 2)
# list of all the group numbers
# specify the k-number of indepedent variables to be included in the model (k=2)
# dependent variable
# independent variable

stan.dataset <- list(N=nrow(Maths_dataset), 
	CL=max(unique(Maths_dataset$GroupID)),
	ClassroomID=as.integer(Maths_dataset$GroupID),
	k=2,
	MathScore=Maths_dataset$Math,
	ActiveTime=Maths_dataset$ActiveTime,
	Supportive=Maths_dataset$Support
	)

# We can run the Bayesian model using the Stan() function

# Start the clock
ptm <- proc.time()
# compile linear regression model for now
bayesian.hierarchical.model = stan("Maths_Activity_study.stan", data=stan.dataset, iter=10000, chains=6, verbose = FALSE)
# Stop the clock
proc.time() - ptm

# view results
bayesian.hierarchical.model

# its a bit involved. We would need to create a set of matrices for the independent variables in R. In Stan, we would 
# need to create a matrix for the random effects since they will be many.

selectedVariables=Maths_dataset[,c(2,3)]

# convert only variable that are factor variables
X <- model.matrix(~ 1 + ActiveTime + Support, data = selectedVariables)
colnames(X)[1] <- "gamma00"
# see conversion
View(X)

# Start the clock
ptm <- proc.time()
# compile linear regression model for now
bayesian.hierarchical.model_2 = stan("Maths_Activity_study_slope.stan", data=stan.dataset, iter=10000, chains=6, verbose = FALSE)
# Stop the clock
proc.time() - ptm



stan.dataset <- list(N=nrow(Maths_dataset), 
	CL=max(unique(Maths_dataset$GroupID)),
	ClassroomID=as.integer(Maths_dataset$GroupID),
	k=3,
	MathScore=Maths_dataset$Math,
	X=X
)

Maths_dataset <- Maths_dataset[, c(1:4,6,7)]
write.csv(Maths_dataset, file = "Maths attainment and activity study.csv", row.names = FALSE)