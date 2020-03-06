## MATH189 Week2 discussion session
## R intro - continued

# get working directory
getwd()

# change working directory 
setwd("/Users/wangyuyao/Documents/UCSD/TA/20W_MATH189/week2/discussion")

## openintro
# https://github.com/OI-Biostat/oi_biostat_text
# https://github.com/OI-Biostat/oi_biostat_labs
# https://github.com/OI-Biostat/oi_biostat_data
#To install the 'oibiostat' package:
install.packages("devtools")
devtools::install_github("OI-Biostat/oi_biostat_data")
library(oibiostat)
data("LEAP")
help(LEAP)

#Section:Defining a function
tr <- function(A) {  
  d <- diag(A)  
  t <- sum(d) 
  return(t)
}

A <- matrix(1:9, nrow=3)
tr(A)


# Use list to return multi-values
powers <- function(x) {
  list(ptwo=x^2, pfive=x^5)
}

p <- powers(2)
p$ptwo
p$pfive

## Section: Plots
x <- seq(-5, 5, by=0.1)

y <- 0.5 * dnorm(x, mean=-2, sd=1) + 0.5 * dnorm(x, mean=2, sd=1)

plot(x, y)
lines(x, y)

plot( x, y, type = "l")


plot(x, y,
     
     type="l", # line type
     
     col="red",  # line color
     
     lty="dashed", # line type
     
     lwd=2, # line width
     
     xlab="X",  # x-axis label
     
     ylab="Density" # y-axis label
     
     #main = "Mixed Gaussian"   # another way to add title
     
)

title("Mixed Gaussian")

legend("topright",     # location
       
       legend="Mixed", # text
       
       # col, lty, lwd as in plot:
       
       col="red",
       
       lty="dashed",
       
       lwd=2
       
)



# save to png

png("plotted.png")

plot(x,y)

dev.off()



# For more information:

?plot

?par

?title

?legend


# boxplot
# Interquantile range (IQR) = Q3 - Q1
# outlier: < Q1 - 1.5 * IQR, or > Q3 + 1.5 * IQR
? boxplot
input <- mtcars[,c('mpg','cyl')]
print(head(input))

# plot the boxplot in R
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")

# create a file and save the plot in that file
# Give the chart file a name.
png(file = "boxplot.png")
# Plot the chart.
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")
# Save the file.
dev.off()


# Learn by yourself
# Other types of boxplot: 
# e.g. Boxplot with Notch, Violin Plots, Bagplot - A 2D Boxplot Extension
# https://www.statmethods.net/graphs/boxplot.html

# e.g., boxplot with notch
boxplot(mpg ~ cyl, data = mtcars, 
        xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", 
        main = "Mileage Data",
        notch = TRUE, 
        varwidth = TRUE, 
        col = c("green","yellow","purple"),
        names = c("High","Medium","Low")
)



#### Introduction to several useful packages

### package "table1"

# table1 R documentation:
# https://www.rdocumentation.org/packages/table1/versions/1.1
# (How to learn a package in R that you do not know.)

## Installation
# Install from CRAN
install.packages("table1")

# Install the latested development version directly from GitHub:
require(devtools)
devtools::install_github("benjaminrich/table1")

## Get Started
# An introduction to the package with examples is provided in :
# https://benjaminrich.github.io/table1/vignettes/table1-examples.html

## Example
# For this example, we will use data from the Mayo Clinic trial in 
# primary biliary cirrhosis (PBC) of the liver found in the survival package.
require(table1)
require(survival)

dat <- subset(survival::pbc, !is.na(trt))  # Exclude subjects not randomized
help(pbc)  # see the description of the dataset
help(is.na)  # learn the is.na function


dat$trt     <- factor(dat$trt, levels=1:2, labels=c("D-penicillamine", "Placebo"))
dat$sex     <- factor(dat$sex, levels=c("m", "f"), labels=c("Male", "Female"))
dat$stage   <- factor(dat$stage, levels=1:4, labels=paste("Stage", 1:4))
dat$edema   <- factor(dat$edema, levels=c(0, 0.5, 1),
                      labels=c("No edema",
                               "Untreated or successfully treated",
                               "Edema despite diuretic therapy"))
dat$spiders <- as.logical(dat$spiders)
dat$hepato  <- as.logical(dat$hepato)
dat$ascites <- as.logical(dat$ascites)

label(dat$stage)    <- "Histologic stage of disease"
label(dat$edema)    <- "Edema status"
label(dat$spiders)  <- "Blood vessel malformations in the skin"
label(dat$hepato)   <- "Presence of hepatomegaly or enlarged liver"
label(dat$ascites)  <- "Presence of ascites"
label(dat$platelet) <- "Platelet count (&times; 10<sup>9</sup> per liter)"
label(dat$protime)  <- "Standardised blood clotting time"
label(dat$albumin)  <- "Serum albumin (g/dL)"
lalabel(dat$chol)     <- "Serum cholesterol (mg/dL)"
label(dat$copper)   <- "Urine copper (&mu;g/day)"
label(dat$trig)     <- "Triglycerides (mg/dL)"
label(dat$age)      <- "Age (y)"
label(dat$sex)      <- "Sex"
bel(dat$alk.phos) <- "Alkaline phosphotase (U/L)"
label(dat$ast)      <- "Aspartate aminotransferase (U/mL)"
label(dat$bili)     <- "Serum bilirubin (mg/dL)"

table1(~ age + sex + stage + edema + spiders + hepato + ascites +
         platelet + protime + albumin + alk.phos + ast + bili + chol +
         copper + trig | trt, data=dat)

indexD=which(dat$trt=="D-penicillamine")
par(mfrow=c(1,2)) #set the plotting area into a 1*2 array
hist(dat$age[indexD], breaks=10, main="Histogram of age in D-penicillamine group")
hist(dat$age[-indexD], breaks=10, main="Histogram of age in placebo group")
plot(density(dat$age[indexD]), main="Density plot of age in D-penicillamine group")
plot(density(dat$age[-indexD]), main="Density plot of age in placebo group")
dev.off() #set par to the default
boxplot(age~trt, data=dat, main="Boxplot of age by treatment groups")
counts=table(dat$sex,dat$trt)
counts
barplot(counts, col=c("darkblue","red"), legend=rownames(counts), main="Patient distribution by treatment and sex")



### Pacakge ggplot2
install.packages('ggplot2')
require(ggplot2)
# installation only need to be done once
# require/library is needed every time

# ggplot2 R documentation:
# https://www.rdocumentation.org/packages/ggplot2/versions/3.2.1



### Package R Markdown

# install R Markdown package
install.packages("rmarkdown")

# R Markdown: The Definitive Duide
# https://bookdown.org/yihui/rmarkdown/

# R Markdown cheatsheet 
# https://rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf
# R Markdown reference guide
# https://rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf
# RMarkdown Basics
# https://rmarkdown.rstudio.com/authoring_basics.html



