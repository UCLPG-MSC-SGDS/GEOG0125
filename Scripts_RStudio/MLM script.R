

#Set seed so your answers are all the same
set.seed(9)
# Sample Per class room people
n1 <- 50; n2 <- 50; n3 <- 50; n4 <- 50
N<-n1+n2+n3+n4 # Total N
# Uniform distrobution of ActiveTime per classroom
X1 <- runif(n1, 0, 1); X2 <- runif(n2, 0, 1)
X3 <- runif(n3, 0, 1); X4 <- runif(n4, 0, 1)
# Uniform distrobution of support per classroom
Z1 <- runif(n1, 0, 1); Z2 <- runif(n2, 0, 1)
Z3 <- runif(n3, 0, 1); Z4 <- runif(n4, 0, 1)
# Intercepts per classroom
B0.1 <- 80; B0.2 <- 75
B0.3 <- 65; B0.4 <- 68
# Same slope for ActiveTime per classroom + Noise
B1 <- rnorm(n1, 10, sd=2.5); B2 <- rnorm(n2, 10, sd=2.5)
B3 <- rnorm(n3, 10, sd=2.5); B4 <- rnorm(n4, 10, sd=2.5)
# different slope for support per classroom + Noise
g1 <- rnorm(n1, 10, sd=2.5); g2 <- rnorm(n2, 5, sd=2.5)
g3 <- rnorm(n3, -5, sd=2.5); g4 <- rnorm(n4, 2, sd=2.5)
# Same interaction between ActiveTime*support support per classroom + Noise
f1<- rnorm(n3, 15, sd=2.5); f2<- rnorm(n3, 15, sd=2.5)
f3<- rnorm(n3, 15, sd=2.5); f4<- rnorm(n3, 15, sd=2.5)
# noise per student within each classroom
e1 <- rnorm(n1, 0, sd=2.5); e2 <- rnorm(n2, 0, sd=2.5)
e3 <- rnorm(n3, 0, sd=2.5); e4 <- rnorm(n4, 0, sd=2.5)

# Our equation to  create Y for each classroom
Y1 = B1*scale(X1,scale=F)+g1*Z1+f1*scale(X1,scale=F)*scale(Z1,scale=F) + B0.1 + e1
Y2 = B2*scale(X2,scale=F)+g2*Z2+f2*scale(X2,scale=F)*scale(Z2,scale=F) + B0.2 + e2
Y3 = B3*scale(X3,scale=F)+g3*Z3+f3*scale(X3,scale=F)*scale(Z3,scale=F) + B0.3 + e3
Y4 = B4*scale(X4,scale=F)+g4*Z4+f4*scale(X4,scale=F)*scale(Z4,scale=F) + B0.4 + e4
# Merge classrooms into 1 data.frame
MLM.Data<-data.frame(Math=c(Y1,Y2,Y3,Y4),ActiveTime=c(X1,X2,X3,X4),Support=c(Z1,Z2,Z3,Z4),
	Classroom=c(rep("C1",n1),rep("C2",n2),rep("C3",n3),rep("C4",n4)),
	Told=c(rep("Works",n1),rep("Works",n2),
		rep("Experimental",n3),rep("Experimental",n4)),
	StudentID=as.factor(1:N))

# Create a unique group number
MLM.Data$GroupID <- as.numeric(as.factor(MLM.Data$Classroom))
Maths_dataset <- MLM.Data

Maths_dataset$Intervention <- ifelse(Maths_dataset$Told == "Works", 1, 0)











fit <- lm(MLM.Data$Math ~ MLM.Data$ActiveTime, data = MLM.Data)
cf <- coef(fit)

MLM.Data$Classroom <- as.factor(MLM.Data$Classroom)
plot(MLM.Data$ActiveTime, MLM.Data$Math, 
	ylab = "Maths score", xlab = "Active Time", col = MLM.Data$Classroom, main = "Maths score and learning times [Classrooms not accounted for]")
legend(0,101,unique(MLM.Data$Classroom),col=1:length(MLM.Data$Classroom),pch=1)
abline(a=cf["(Intercept)"], b=cf["MLM.Data$ActiveTime"], col = "black", lty="dashed")
cf["(Intercept)"]

MLM.Data1 <- MLM.Data[MLM.Data$Classroom == "C1",]
MLM.Data2 <- MLM.Data[MLM.Data$Classroom == "C2",]
MLM.Data3 <- MLM.Data[MLM.Data$Classroom == "C3",]
MLM.Data4 <- MLM.Data[MLM.Data$Classroom == "C4",]

fitcl1 <- lm(Math ~ ActiveTime, data = MLM.Data1)
fitcl2 <- lm(Math ~ ActiveTime, data = MLM.Data2)
fitcl3 <- lm(Math ~ ActiveTime, data = MLM.Data3)
fitcl4 <- lm(Math ~ ActiveTime, data = MLM.Data4)

cf1 <- coef(fitcl1)
cf2 <- coef(fitcl2)
cf3 <- coef(fitcl3)
cf4 <- coef(fitcl4)

plot(MLM.Data$ActiveTime, MLM.Data$Math, 
	ylab = "Maths score", xlab = "Active Time", col = MLM.Data$Classroom, main = "Maths score and learning times [Classrooms accounted for]")
legend(0,101,unique(MLM.Data$Classroom),col=1:length(MLM.Data$Classroom),pch=1)
abline(a=cf1["(Intercept)"], b=cf1["ActiveTime"], col = "black", lty="dashed")
abline(a=cf2["(Intercept)"], b=cf2["ActiveTime"], col = "red", lty="dashed")
abline(a=cf3["(Intercept)"], b=cf3["ActiveTime"], col = "green", lty="dashed")
abline(a=cf4["(Intercept)"], b=cf4["ActiveTime"], col = "blue", lty="dashed")


cf["(Intercept)"]





plot(Melanoma$age, Melanoma$time, 
	main = "Survival Time from Malignant Melanoma",
	xlab = "Age (in years)",
	ylab = "Survival Time (in days)",
	col = ifelse(Melanoma$sex == "1", "blue", "red"))
legend("topleft", 
	pch = c(1, 1), 
	c("Female", "Male"), 
	col = c("red", "blue")) 
abline(lm(melanoma_female$time ~ melanoma_female$age), col = "red")
abline(lm(melanoma_male$time ~ melanoma_male$age), col = "blue")


data<-iris
plot(data$Sepal.Length, data$Sepal.Width, col=data$Species)
legend(7,4.3,unique(data$Species),col=1:length(data$Species),pch=1)


library(foreign)
dat <- read.arff(url("http://www.cs.umb.edu/~rickb/files/UCI/autos.arff"))


abline(a=cf["(Intercept)"], b=cf["MLM.Data$ActiveTime"])


dataset <- read.table("https://grodri.github.io/datasets/hospital.dat")

names(dataset) <- c("births", "logincome", "distance", "dropout", "graduate", "motherID")

write.csv(dataset, file = "Pregnancies and hospital data.csv", row.names = FALSE)
