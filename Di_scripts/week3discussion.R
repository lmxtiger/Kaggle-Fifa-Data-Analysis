require(table1)
require(survival)

dat <- subset(survival::pbc, !is.na(trt))  # Exclude subjects not randomized
help(pbc)  # see the description of the dataset
help(is.na)  # learn the is.na function

# Prepare table1
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
label(dat$chol)     <- "Serum cholesterol (mg/dL)"
label(dat$copper)   <- "Urine copper (&mu;g/day)"
label(dat$trig)     <- "Triglycerides (mg/dL)"
label(dat$age)      <- "Age (y)"
label(dat$sex)      <- "Sex"
label(dat$alk.phos) <- "Alkaline phosphotase (U/L)"
label(dat$ast)      <- "Aspartate aminotransferase (U/mL)"
label(dat$bili)     <- "Serum bilirubin (mg/dL)"

table1(~ age + sex + stage + edema + spiders + hepato + ascites +
         platelet + protime + albumin + alk.phos + ast + bili + chol +
         copper + trig | trt, data=dat)

indexD=which(dat$trt=="D-penicillamine")

#side by side hisograms
par(mfrow=c(1,2)) #set the plotting area into a 1*2 array
hist(dat$age[indexD], breaks=10, main="Histogram of age in D-penicillamine group", 
     xlab="age")
hist(dat$age[-indexD], breaks=10, main="Histogram of age in placebo group", xlab="age")
dev.off() #set par to the default

#Overlapping hisograms
help(rgb)
hist(dat$age[indexD], breaks=10, main="Histogram of age", xlab="age", col=rgb(1,0,0,1/4), 
     xlim=range(dat$age), ylim=c(0,30))
hist(dat$age[-indexD], breaks=10, xlab="age", col=rgb(0,0,1,1/4), add=T)
legend("topright", c("D-penicillamine","placebo"), fill=c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)))

#side by side density plots
par(mfrow=c(1,2))
plot(density(dat$age[indexD]), main="Density plot of age in D-penicillamine group")
plot(density(dat$age[-indexD]), main="Density plot of age in placebo group")
dev.off() 

#Overlapping density plots
plot(density(dat$age[indexD]), main="Density plot of age", ylim=c(0,0.04), col="red")
lines(density(dat$age[-indexD]), col="blue")
legend("topright", c("D-penicillamine","placebo"), fill=c("red","blue"))

#boxplot
boxplot(age~trt, data=dat, main="Boxplot of age by treatment groups", 
        col=c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), xlab="treatment")

#barplot of counts, with stacked bars
counts=table(dat$sex,dat$trt)
counts
barplot(counts, col=c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), legend=rownames(counts), 
        main="Patient distribution (counts) by treatment and sex", ylim = c(0, 200))

#barplot of proportions, with juxtaposed bars
proportions=prop.table(counts,2) # column percentages
proportions
par(mfrow = c(1,2))
barplot(proportions, col=c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), legend=rownames(proportions),
        beside=TRUE, main="Patient distribution (proportions) by treatment and sex")
barplot(proportions, col=c(rgb(1,0,0,1/4),rgb(0,0,1,1/4)), legend=rownames(proportions),
        beside=FALSE, main="Patient distribution (proportions) by treatment and sex")

#colors
require(RColorBrewer)
display.brewer.all()
help(brewer.pal)
counts2=table(dat$stage,dat$trt)
proportions2=prop.table(counts2,2)
display.brewer.pal(n=6, name="Reds")
barplot(proportions2, col=brewer.pal(n=4,name="Blues"), beside=TRUE, 
        main="Patient distribution (proportions) by treatment and stage", ylim = c(0, 0.45))
legend("topleft", rownames(proportions2), fill=brewer.pal(n=4,name="Blues"), cex=0.75)
