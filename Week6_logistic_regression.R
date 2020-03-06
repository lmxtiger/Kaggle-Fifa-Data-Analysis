# Week 6: logistic regression on >1 predictors
print(getwd() )
setwd("C:/Users/Minxuan Liu/Desktop/2019 winter math189")
print(getwd() )

library(dplyr)
library(ggplot2)

# Load data
dat <- read.csv("C:/Users/Minxuan Liu/Desktop/2019 winter math189/Fifa data/Fifa_agegp_bal_na_omitted.csv",
                stringsAsFactors = FALSE)
w <- ggplot(dat, aes(x=wage_gp, y=stat(count), fill=wage_gp)) + geom_bar(color="black", fill="orange", width = 0.8)
w + scale_x_discrete(labels = c("Low Wage Group", "High Wage Group")) +
  geom_text(stat = 'count', aes(label=..count..), vjust = -1) +
  ggtitle("Distribution of Outcome Variable Wage Group")

dat$Value %>% head()
# Process "Value" col entries: Value: 250
raw_value <- dat$Value
for(i in 1:length(raw_value)) {
  raw_value[i] <- substr(raw_value[i], 4, nchar(raw_value[i])-1)
}
dat$Value <- as.numeric(raw_value)  # as.numeric(): "Value" entries aren't all integers
dat$Value %>% head()

write.csv(dat, "C:/Users/Minxuan Liu/Desktop/2019 winter math189/Fifa data/Fifa_agegp_bal_na_omitted.csv")

# a)    Describe the distribution of the outcome variable, identify a main predictor that
# you're interested in studying its effect on the outcome;

# b)    Identify other variables (i.e. predictors, often called covariates) that might be related to
# the outcome or the main predictor, discuss these variables in the context of part 2) above of this assignment;

# c)    Carry out univariate logistic regression of the outcome on each of
# the predictors including the main predictor, interpret the results in terms of odds ratio etc.
# Odds ratio
Overall.fit <- glm((wage_gp - 1) ~ Overall, family = binomial(), data = dat)
summary(Overall.fit)
Overall.odds_ra <- exp(Overall.fit$coefficients[2])
Overall.odds_ra

age_gp2.fit <- glm((wage_gp - 1) ~ age_gp2, family = binomial(), data = dat)
summary(age_gp2.fit)
age_gp2.odds_ra <- exp(age_gp2.fit$coefficients[2])
age_gp2.odds_ra

Reactions.fit <- glm((wage_gp - 1) ~ Reactions, family = binomial(), data = dat)
summary(Reactions.fit)
Reactions.odds_ra <- exp(Reactions.fit$coefficients[2])
Reactions.odds_ra

Value.fit <- glm((wage_gp - 1) ~ Value, family = binomial(), data = dat)
summary(Value.fit)
Value.odds_ra <- exp(Value.fit$coefficients[2])
Value.odds_ra

# d)    Fit a multiple logistic regression model by including more than one predictors,
# interpret the results in terms of conditional odds ratio etc.
dat.fit <- glm((wage_gp - 1) ~ Overall + age_gp2 + Reactions + Value, family = binomial(), data = dat)
summary(dat.fit)
exp(dat.fit$coefficients)

# In order for a selected predictor to be significant, in other words, to matter in affecting the dependent
# variable (Y: wage group), its corresponding p-value in the "Pr(>|z|)" column needs to be less than 2.5%. Equivalently,
# its corresponding value in the "z value" column needs to be > 1.96 or < -1.96. 
# (Assuming a 0.05 level of significance.)
# Intercept: 
# Overall (rating): As its p-value is very small, it is a significant variable. An odd ratio of 1.71 means 
# we expect to see 71% increase in the odds of being in the high-wage group for one-unit increase in his overall rating.

# age_gp2 (binary age group): As its p-value is very small, it is a significant variable. An odd ratio of 0.76 means
# the odds of being in the high-wage group is 
# Reactions:
# Value: 