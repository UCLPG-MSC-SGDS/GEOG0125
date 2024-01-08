
a = 1
b = 0
c = 0
d = 0
e = 0
f = 0

quadratic = function(x) {
  a*x^2 + b*x + c
}

x = -10:10
plot(x, quadratic(x), type="l", ylab = "Dependent variable", xlab="Independent variable", main = "Quadratic")
abline(h=0)
abline(v=0)

cubic = function(x) {
  a*x^3 + b*x^2 + c*x + d
}

x = -10:10
plot(x, cubic(x), type="l", ylab = "Dependent variable", xlab="Independent variable", main = "Cubic")
abline(h=0)
abline(v=0)

higher_order4 = function(x) {
  a*x^4 + b*x^3 + c*x^2 + d*x + e
}

x = -10:10
plot(x, higher_order4(x), type="l", ylab = "Dependent variable", xlab="Independent variable", main = "Higher Degree (4)")
abline(h=0)
abline(v=0)

higher_order5 = function(x) {
  a*x^5 + b*x^4 + c*x^3 + d*x^2 + e*x + f
}

x = -10:10
plot(x, higher_order5(x), type="l", ylab = "Dependent variable", xlab="Independent variable", main = "Higher Degree (4)")
abline(h=0)
abline(v=0)


linear = function(x) {
  a*x
}

x = -10:10
plot(x, linear(x), type = "l", ylab = "Dependent variable", xlab="Independent variable", main = "Linear", ylim=c(-10,10))
abline(h=0)
abline(v=0)



x <- seq(0, pi * 2, 0.1)
sin_x <- sin(x)
y <- sin_x + rnorm(n = length(x), mean = 0, sd = sd(sin_x / 2))
Sample_data <- data.frame(y, x)


library(ggplot2)
ggplot(Sample_data, aes(x, y)) +
  geom_point() +
  theme_classic()

