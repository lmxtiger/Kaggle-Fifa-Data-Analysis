x <- seq(1, 10, by=1)
d <- dbinom(x, size=10, prob = 0.5)
# binomial distribution: dbinom, rbinom, qbinom, pbinom
# Poisson 
d <- dpois(x, lambda = 4)
# Exponential
d <- dexp(x, rate = 4)

# get the working directory
getwd()

# library()

# Defining a func
tr <- function(A) {
  d <- diag(A)
  t <- sum(d)
  return(t)
}

A <- matrix(1:9, nrow = 3)
tr(A)

# Use list to return multi-values
powers <- function(x) {
  list(ptwo=x^2, pfive=x^5)
}

p <- powers(3)
p$ptwo
p$pfive

# Plots
x <- seq(-5, 5, by = 0.1)
y <- 0.5*dnorm(x, mean=-2, sd=1)+ 0.5*dnorm(x, mean = 2, sd = 1)
plot(x, y)
lines(x, y)
plot(x, y, type = "l")
plot(x, y,
     type = "l", # line type
     col = "red", # line color
     lty="dashed", # line type
     lwd=2, # line width
     xlab="X", # x-axis label
     ylab="Density" # y-axis label
)
title("Mixed Gaussian")
legend("topright")

# Save to png
png("plotted.png")
plot(x, y)
dev.off()

?legend

# boxplot
input <- mtcars[, c('mpg', 'cyl')]
print(head(input))

# Plot the boxplot in R
boxplot(mpg ~ cyl, data = mtcars, xlab = "Numbers of Cylinders", 
        ylab = "Miles Per Gallon", main = "Mileage data")
png(file = "boxplot.png")

# Table1
install.packages("table1")
require(devtools)
require(table1)
require(survival)

dat <- subset(survival::pbc, !is.na(trt)) # Exclude subjects not random
help(pbc)
help("is.na") # function

dat$trt <- factor(dat$trt, levels = 1:2, labels = c('a', 'b', 'p'))
dat$stage <- factor(dat$stage, levels = 1:4, labels = paste("Stage", 1:4))
# Numericals to Booleans
dat$spiders <- as.logical(dat$spiders)
# Changing labels to a specific column
labels(dat$age) <- "Age (y)"

indexD = which(dat$trt == "a")
par(mfrow = c(1,2))
plot(density(dat$age[indexD]), main = "density plot of age in a treatment")
counts = table(dat$sex, dat$trt)
# "main = " arg: main title

# Package ggplot2
install.packages('ggplot2')
require(ggplot2)
# R Markdown (for FINAL PROJECT)
install.packages('rmarkdown')
