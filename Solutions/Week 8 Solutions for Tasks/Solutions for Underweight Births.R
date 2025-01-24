rm(list = ls())
gc()

# solutions
georgia_shp <- read_sf("Georgia_Shapefile.shp")
low_births <- read.csv("Low_birth_weights_data.csv")

georgia.spatial.data <- merge(georgia_shp, low_births, by.x = "NAME", by.y = "NAME", all.x = TRUE)
georg.sp.object <- as(georgia.spatial.data, "Spatial")

adjacencyMatrix_v1 <- shape2mat(georg.sp.object)
extractComponents_v1 <- prep_icar_data(adjacencyMatrix_v1)

n <- as.numeric(extractComponents_v1$group_size)
nod1 <- extractComponents_v1$node1
nod2 <- extractComponents_v1$node2
n_edges <- as.numeric(extractComponents_v1$n_edges)
y <- georgia.spatial.data$Lowbirths
e <- georgia.spatial.data$ExpectedNumber

# We need to build the adjacency matrix using INLA library functions based from our nodes from the shapefile for England.
adj.matrix=sparseMatrix(i=nod1,j=nod2,x=1,symmetric=TRUE)

# The next four lines of code performs some horrible matrix manipulation to compute the scaling factor 
# (which is a geometric mean of the variances captured along the diagonal of invert adjacency matrix.)
Q=Diagonal(n, rowSums(adj.matrix)) - adj.matrix
Q_pert=Q+Diagonal(n) * max(diag(Q)) * sqrt(.Machine$double.eps)
Q_inv=inla.qinv(Q_pert, constr=list(A = matrix(1,1,n),e=0))
scaling_factor=exp(mean(log(diag(Q_inv))))

# no independent variable
stan.spatial.dataset_v1 <- list(N=n, N_edges=n_edges, node1=nod1, node2=nod2, Y=y, Offset=e, factor=scaling_factor)

# use the same stan code from practicals!
icar_birthweights_fit = stan("Stan ICAR birthweight script.stan", data=stan.spatial.dataset_v1, iter=10000, chains=6, verbose = FALSE)

options(scipen = 999)
summary(icar_birthweights_fit, pars=c("rr_alpha", "sigma"), probs=c(0.025, 0.975))$summary

# diagnostic check on the rHats
diagnostic.checks <- as.data.frame(summary(icar_birthweights_fit, pars=c("alpha", "sigma", "phi", "lp__"), probs=c(0.025, 0.5, 0.975))$summary)
diagnostic.checks$valid <- ifelse(diagnostic.checks$Rhat < 1.1, 1, 0)
table(diagnostic.checks$valid)

# extraction key results
relativeRisk.results <- as.data.frame(summary(icar_birthweights_fit, pars=c("rr_mu"), probs=c(0.025, 0.975))$summary)

# rest is easy -- following the notes on practicals