print(getwd() )
setwd("C:/Users/Minxuan Liu/Desktop/2019 winter math189")
print(getwd() )

#install.packages("table1")
require("table1")
#install.packages("ggplot2")
require("ggplot2")

# Start reading CSV file
whole_data <- read.csv("Fifa data/Fifa_players_data.csv", stringsAsFactors = FALSE)
whole_data_age_gp <- read.csv("Fifa data/data_with_age_group.csv", stringsAsFactors = FALSE)
num_of_cols <- length(whole_data)
num_of_cols_age <- length(whole_data_age_gp)

# Process Wage string
head_wage <- whole_data_age_gp$Wage
for (i in 1:length(head_wage)) {
  head_wage[i] <- substr(head_wage[i], 4, nchar(head_wage[i])-1)
}
whole_data_age_gp$Wage <- strtoi(head_wage)
# Process release clause string
head_rel_clau <- whole_data_age_gp$Release.Clause
for (i in 1:length(head_rel_clau)) {
  head_rel_clau[i] <- substr(head_rel_clau[i], 4, nchar(head_rel_clau[i])-1)
}
whole_data_age_gp$Release.Clause <- head_rel_clau
# Process Value string
head_value <- whole_data_age_gp$Value
for (i in 1:length(head_value)) {
  head_value[i] <- substr(head_value[i], 4, nchar(head_value[i])-1)
}
whole_data_age_gp$Value <- head_value
# Process Weight string
head_weight <- whole_data_age_gp$Weight
whole_data_age_gp$Weight <- strtoi(substr(head_weight, 1, 3) )

# For convertiable cols: string to numerics
whole_data_age_gp <- transform(whole_data_age_gp, 
                               Wage = as.numeric(Wage), 
                               Release.Clause = as.numeric(Release.Clause), 
                               Value = as.numeric(Value)) 
                               
# Construct a table1
# cols: 4 age groups
# rows: left/right_foot, Body_type, international rep, Overall(rating)
labels <- list(
  variable = list(Preferred.Foot="Preferred Foot",
                  Overall="Overall Rating",
                  Work.Rate="Work Rate",
                  Body.Type="Body Type",
                  Release.Clause="Release Clause",
                  Weight="Weight(lbs)",
                  Composure="Composure",
                  Stamina="Stamina"),
                  Wage="Wage(K)")

# Customize the contents using custom renders
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("","Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

# Nice label for age_group
whole_data_age_gp$age_group <- 
  factor(whole_data_age_gp$age_group,
         levels=c(0,1,2,3),
         labels=c("<21", "21-24", "25-28", ">28"))

# Nice labels for the leftmost col
label(whole_data_age_gp$Preferred.Foot) <- "Preferred Foot"
label(whole_data_age_gp$Overall) <- "Overall Rating"
label(whole_data_age_gp$Work.Rate) <- "Work Rate"
label(whole_data_age_gp$Body.Type) <- "Body Type"
label(whole_data_age_gp$Release.Clause) <- "Release Clause"
units(whole_data_age_gp$Wage) <- "K"
units(whole_data_age_gp$Value) <- "M"
units(whole_data_age_gp$Release.Clause) <- "M"
units(whole_data_age_gp$Weight) <- "lbs"

# Create and save table as an image
t1 <- table1(~ Wage + Release.Clause + Value + Work.Rate + Body.Type + Preferred.Foot + Weight + Stamina
       + Composure + Overall | age_group, 
       rowlabelhead = "Characteristic\\Age Group (Years)", data = whole_data_age_gp, overall = "Total",
       topclass = "Rtable1-zebra Rtable1-grid Rtable1-shade")



