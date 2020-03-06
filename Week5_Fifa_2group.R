print(getwd() )
setwd("C:/Users/Minxuan Liu/Desktop/2019 winter math189")
print(getwd() )
require("table1")
require("ggplot2")

# Week 5: Two Group 
# Continue from your data set from week 3, keep the description, reduce to 2 groups if you had more,
# by either excluding the extra group(s) or combining them into 2 groups. Identify an outcome variable 
# that you are interested in comparing between these 2 groups. Make sure that your outcome is binary by
# dichotomizing it if it is not binary initially.

whole_data_age_gp <- read.csv("Fifa data/data_with_age_group.csv", stringsAsFactors = FALSE)
# Process Value string: Wage: "350"
head_wage <- whole_data_age_gp$Wage
for (i in 1:length(head_wage)) {
  head_wage[i] <- substr(head_wage[i], 4, nchar(head_wage[i])-1)
}
whole_data_age_gp$Wage <- strtoi(head_wage)

# Avg mean wage (rm NA)
avg_wage <- mean(whole_data_age_gp$Wage, na.rm = TRUE)
dat <- whole_data_age_gp
# Drop rows with wage == NA
dat <- dat[!is.na(dat$Wage), ]
# Randomly drop rows with Age > 24 (old group)
old_row_num <- which(dat$Age > 24)
young_row_num <- which(dat$Age <= 24)
old_row_del <- old_row_num[sample(1:length(old_row_num), length(old_row_num)-length(young_row_num))]
dat <- dat[-old_row_del, ]

# Categorize data: young_low_wage, young_high_wage, old_low_wage, old_high_wage
# Note: "&&" WON'T work!
dat$age_wage_bi[dat$age_group %in% c(0,1) & dat$Wage < avg_wage] <- 1 # "young_low_wage"
dat$age_wage_bi[dat$age_group %in% c(0,1) & dat$Wage > avg_wage] <- 2 # "young_high_wage"
dat$age_wage_bi[dat$age_group %in% c(2,3) & dat$Wage < avg_wage] <- 3 # "old_low_wage"
dat$age_wage_bi[dat$age_group %in% c(2,3) & dat$Wage > avg_wage] <- 4 # "old_high_wage"
dat$age_wage_bi <- strtoi(dat$age_wage_bi)
#young_low_count <- sum(dat$age_wage_bi == 1)
#young_high_count <- sum(dat$age_wage_bi == 2)
#old_low_count <- sum(dat$age_wage_bi == 3)
#old_high_count <- sum(dat$age_wage_bi == 4)

# Create new VAR age_gp2: >24 vs <=24
dat$age_gp2[dat$age_group %in% c(0,1)] <- 1
dat$age_gp2[dat$age_group %in% c(2,3)] <- 2
# Create new VAR wage_gp: >avg_wage vs < avg_wage
dat$wage_gp[dat$Wage < avg_wage] <- 1
dat$wage_gp[dat$Wage > avg_wage] <- 2

# Output to .csv
write.csv(dat, "C:/Users/Minxuan Liu/Desktop/2019 winter math189/Fifa data/Fifa_agegp_bal_na_omitted.csv")

# Create a 2*2 table: row_label: age, col_label: wage
age_wage_mat <- matrix(tabulate(dat$age_wage_bi), nrow = 2, dimnames = 
                         list(Wage_range=c("Below Avg Wage(9.86K)", "Above Avg Wage(9.86K)"),
                              Age_range=c("<=24", ">24")))
age_wage_mat <- t(age_wage_mat)
age_wage_mat

# a)    State the research question of interest;
# b)    Discuss why it is reasonable to assume that the observations are i.i.d.
# c)    Set up the null and alternative hypothesis (introduce the random variables,
# distribution(s) and parameter(s) first), use a two-sided significant level of 0.05;
p_young_high <- age_wage_mat[1,2]/sum(age_wage_mat[1,])
p_old_high <- age_wage_mat[2,2]/sum(age_wage_mat[2,])
p_young_high
p_old_high

# d)    Find the 95% confidence intervals for p1, p2, the risk difference, risk ratio, and odds ratio;
n = nrow(dat)/2
# CI for p1, p2
rad_p_young_high <- qnorm(0.975)*sqrt(p_young_high*(1-p_young_high)/n)
CI_p_young_high <- p_young_high + c(-rad_p_young_high, rad_p_young_high)
rad_p_old_high <- qnorm(0.975)*sqrt(p_old_high*(1-p_old_high)/n)
CI_p_old_high <- p_old_high + c(-rad_p_old_high, rad_p_old_high)

# CI for risk difference
rad_risk_diff <- qnorm(0.975)*sqrt(p_young_high*(1-p_young_high)/n + p_old_high*(1-p_old_high)/n)
risk_diff <- (p_young_high - p_old_high)
CI_risk_diff <- risk_diff + c(-rad_risk_diff, rad_risk_diff)

# CI for Risk ratio
rad_risk_ratio <- qnorm(0.975)*sqrt((1 - p_young_high)/n/p_young_high + (1 - p_old_high)/n/p_old_high)
risk_ratio <- log(p_young_high/p_old_high)
CI_risk_ratio <- risk_ratio + c(-rad_risk_ratio, rad_risk_ratio)
CI_risk_ratio <- exp(CI_risk_ratio)

# CI for odds ratio
rad_odds_ratio <- qnorm(0.975)*sqrt(1/(n*p_young_high*(1-p_young_high)) + 1/(n*p_old_high*(1-p_old_high)))
odds_ratio <- log(p_young_high/(1-p_young_high)/p_old_high*(1-p_old_high))
CI_odds_ratio <- odds_ratio + c(-rad_odds_ratio, rad_odds_ratio)
CI_odds_ratio <- exp(CI_odds_ratio)

# Insert CIs into a table
CI_df <- data.frame(CI_p_young_high = CI_p_young_high, CI_p_old_high = CI_p_old_high, CI_risk_diff = CI_risk_diff,
                    CI_risk_ratio = CI_risk_ratio, CI_odds_ratio = CI_odds_ratio, row.names = c("Lower", "Upper"))
CI_df[nrow(CI_df)+1, ] <- c(p_young_high, p_old_high, risk_diff, exp(risk_ratio), exp(odds_ratio))
row.names(CI_df) <- c("Lower", "Upper", "Middel point")
CI_df

#install.packages(epi.2by2)
#require(eqi.2by2)
#epi.2by2(age_wage_mat)

#odds.ratio(age_wage_mat, conf.level = 0.95)

# e)    Carry out both the Chi-squared and Fisher's exact test, and discuss their 
# suitability to your data.
chisq.test(age_wage_mat)
fisher.test(age_wage_mat)

