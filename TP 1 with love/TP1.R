# TD1
# Section 1: Data Visualization

# Task 1: Galton Inheredity

# install.packages("HistData") for the first time you need to install the package
library(HistData)
data(Galton)
Galton <- data.frame(Galton)

# a. Reconstruct the contingency table between the height of 928 adults children and the average height of
# their 205 set of parents.
# Load the data
head(Galton)

# Inspect the data
str(Galton) # 928 observations, 2 variables (parent vs. child)
summary(Galton) 
# parent: avg height of both parent in inches
# child: height of grown child in inches

# Create height intervals (binning data/bins)
child_breaks <- seq(60, 75, by = 0.5)  
parent_breaks <- seq(63, 72, by = 0.5)  

# Convert the interval labels to midpoints (avg)
child_midpoints <- (child_breaks[-length(child_breaks)] + child_breaks[-1]) / 2
parent_midpoints <- (parent_breaks[-length(parent_breaks)] + parent_breaks[-1]) / 2

Galton$child_group <- cut(Galton$child, breaks = child_breaks, labels = round(child_midpoints, 2))
Galton$parent_group <- cut(Galton$parent, breaks = parent_breaks, labels = round(parent_midpoints, 2))

# Create contigency table
table <- table(Galton$parent_group, Galton$child_group)
table

# b. Reconstruct the scatter plot of and regression line between the height of 
# children and average height of their parents.

# Fit linear regression model
model <- lm(child ~ parent, data = Galton)

# Get regression coefficients
slope <- coef(model)[2]
intercept <- coef(model)[1]

# Create scatter plot with regression line
plot(Galton$parent, Galton$child,
     main = "Height of Children VS. Average Height of Parents",
     xlab = "Average Parent Height (inches)",
     ylab = "Child Height (inches)",
     pch = 16,
     col = rgb(0, 0, 0, 0.5), # Semi-transparent black points
     cex = 0.8
     )

# Add regression line
abline(model, col = "red", lwd = 2)

# Add equation and R-squared to the plot
r_squared <- summary(model)$r.squared
equation <- paste("y =", round(intercept, 3), "+", round(slope, 3), "* x")
r2_text <- paste("R^2 =", round(r_squared, 4))

legend ("topleft", legend = c(equation, r2_text),
        col = c("red", "black"), lty = c(1, NA),
        bty = "n", cex = 0.9
        )

# Print model summary
"=== Regression Summary ===\n"
summary(model)

"\n=== Key Statistics ===\n"
cat("Number of observations: ", nrow(Galton), "\n")
cat("Slope: ", round(slope, 4), "\n")
cat("Intercept: ", round(intercept, 4), "\n")
cat("R-squared: ", round(r_squared, 4), "\n")
cat("Correlation: ", round(cor(Galton$parent, Galton$child), 4), "\n")

# Task 2: Munich Rent Index of 1999

# install.packages("gamlss.data") for the first time you need to install the package
library(gamlss.data)
data(rent99)
rent99 <- data.frame(rent99)
# install.packages("dplyr")
library(dplyr)
glimpse(rent99)

# a. Reconstruct the histograms and kernel density estimates below
# Load and inspect dataset
head(rent99)
summary(rent99)

# Reconstruct a histogram of net rent
hist(rent99$rent,
     breaks = 30,
     freq = FALSE,
     col = "gray",
     border = "black",
     main = "Histogram and Kernel Density of Net Rent",
     xlab = "net rent in Euro",
     ylab = "estimated density"
     )

lines(density(rent99$rent),
      col = "red",
      lwd = 2)

# Net rent per sqm
hist(rent99$rentsqm,
     breaks = 30,
     freq = FALSE,
     col = "gray",
     border = "black",
     main = "Histogram and Kernel Density of Net Rent per sqm",
     xlab = "net rent per sqm in Euro",
     ylab = "estimated density"
)
lines(density(rent99$rentsqm),
      col = "red",
      lwd = 2)

# area
hist(rent99$area,
     breaks = 30,
     freq = FALSE,
     col = "gray",
     border = "black",
     main = "Histogram and Kernel Density of area",
     xlab = "area in sqm",
     ylab = "estimated density"
)
lines(density(rent99$area),
      col = "red",
      lwd = 2)

# Year of Construction
hist(rent99$yearc,
     breaks = 30,
     freq = FALSE,
     col = "gray",
     border = "black",
     main = "Histogram and Kernel Density of year",
     xlab = "year of construction",
     ylab = "estimated density"
)
lines(density(rent99$yearc),
      col = "red",
      lwd = 2)

# b. Reconstruct the scatter plots below
# net rent vs. area
plot(rent99$area, rent99$rent,
     main = "Scatterplot: net rent vs. area",
     xlab = "area in sqm",
     ylab = "net rent in Euro")

# net rent per sqm vs. area
plot(rent99$area, rent99$rentsqm,
     main = "Scatterplot: net rent per sqm vs. area",
     xlab = "area in sqm",
     ylab = "net rent per sqm in Euro")

# net rent vs. year
plot(rent99$yearc, rent99$rent,
     main = "Scatterplot: net rent vs. year of construction",
     xlab = "year of construction",
     ylab = "net rent in Euro")

# net rent per sqm vs. year
plot(rent99$yearc, rent99$rentsqm,
     main = "Scatterplot: net rent per sqm vs. year of construction",
     xlab = "year of construction",
     ylab = "net rent per sqm in Euro")

# c. Reconstruct the cluster scatter plot below
# install.packages("ggplot2")
library(ggplot2)
# Compute average values per cluster
rent_area <- rent99 %>%
  group_by(area) %>%
  summarise(
    mean_rent = mean(rent, na.rm = TRUE),
    sd_rent = sd(rent, na.rm = TRUE),
    mean_rentsqm = mean(rentsqm, na.rm = TRUE),
    sd_rentsqm = sd(rentsqm, na.rm = TRUE)
  )
rent_yearc <- rent99 %>%
  group_by(yearc) %>%
  summarise(
    mean_rent = mean(rent, na.rm = TRUE),
    sd_rent = sd(rent, na.rm = TRUE),
    mean_rentsqm = mean(rentsqm, na.rm = TRUE),
    sd_rentsqm = sd(rentsqm, na.rm = TRUE)
  )

# Cluster scatterplots
ggplot(rent_area, aes(x = area, y = mean_rent)) +
  geom_point(shape = 21, fill = "black", size = 2) +
  geom_errorbar(aes(ymin = mean_rent - sd_rent, ymax = mean_rent + sd_rent),
                width = 0.5, color = "black") +
  labs(title = "Average net rent vs. area",
       x = "area in sqm", y = "average net rent") +
  theme_bw()

ggplot(rent_area, aes(x = area, y = mean_rentsqm)) +
  geom_point(shape = 21, fill = "black", size = 2) +
  geom_errorbar(aes(ymin = mean_rentsqm - sd_rentsqm, ymax = mean_rentsqm + sd_rentsqm),
                width = 0.5, color = "black") +
  labs(title = "Average net rent per sqm vs. area",
       x = "area in sqm", y = "average net rent per sqm") +
  theme_bw()

ggplot(rent_yearc, aes(x = yearc, y = mean_rent)) +
  geom_point(shape = 21, fill = "black", size = 2) +
  geom_errorbar(aes(ymin = mean_rent - sd_rent, ymax = mean_rent + sd_rent),
                width = 0.5, color = "black") +
  labs(title = "Average net rent vs. year of construction",
       x = "year of construction", y = "average net rent") +
  theme_bw()

ggplot(rent_yearc, aes(x = yearc, y = mean_rentsqm)) +
  geom_point(shape = 21, fill = "black", size = 2) +
  geom_errorbar(aes(ymin = mean_rentsqm - sd_rentsqm, ymax = mean_rentsqm + sd_rentsqm),
                width = 0.5, color = "black") +
  labs(title = "Average net rent per sqm vs. year of construction",
       x = "year of construction", y = "average net rent per sqm") +
  theme_bw()

# d. Reconstruct the box plots and smooth density estimators below.
# boxplot
plot(rent99$location, rent99$rentsqm,
     xlab = "location", ylab = "net rent per sqm")

# smooth density estimator
# label location categories
rent99$Location <- factor(rent99$location, 
                          levels = c(1, 2, 3), 
                          labels = c("average", "good", "top"))

# Extract net rent per sqm variable by location
net_rent_average <- rent99$rentsqm[rent99$Location == "average"]
net_rent_good <- rent99$rentsqm[rent99$Location == "good"]
net_rent_top <- rent99$rentsqm[rent99$Location == "top"]


plot(density(net_rent_average), 
     main = "",
     xlab = "net rent per sqm",
     ylab = "estimated density",
     xlim = c(0, 18),
     ylim = c(0, 0.16),
     lwd = 2,
     col = "black",
     lty = 1)
lines(density(net_rent_good), lwd = 2, col = "blue", lty =2)
lines(density(net_rent_top), lwd = 2, col = "red", lty =3 )
legend("topright", 
       legend = c("average", "good", "top"),
       lty = c(1, 2, 3),
       col = c("black", "blue", "red"),
       lwd = 2,
       bty = "n")

# Task 3: Fuel Consumption
# The goal of this task is to understand how fuel consumption varies over 
# the 50 United States and the District of Columbia (Federal Highway Administration, 2001).
# Variables in the Fuel Consumption Data:
# • Drivers: number of licensed drivers in the state
# • FuelC: gasoline sold for road use, thousand of gallons
# • Income: per person personal information for the year 2001, in thousands of dollars
# • Miles: miles of Federal-aid highway miles in the state
# • Pop: 2001 population age 16 and over
# • Tax: gasoline state tax rate, cents per gallon
# install.packages("alr4") for the first time you need to install the package
# options(timeout = 300)
# install.packages("alr4")
library(alr4)
data(fuel2001)
fuel2001 <- data.frame(fuel2001)

# a. Create 3 more following variables and add to the fuel data consumption
# • Fuel: 1000×FuelC/Pop
fuel2001$Fuel <- (fuel2001$FuelC*1000)/fuel2001$Pop
fuel2001$Fuel
# • Dlic: 1000×Drivers/Pop
fuel2001$Dlic <- (fuel2001$Drivers*1000)/fuel2001$Pop
fuel2001$Dlic
# • log(Miles): natural logarithm of Miles
fuel2001$log_Miles <- log10(fuel2001$Miles)
fuel2001$log_Miles

# b. Based on the goal of the task

# • Define response variable
# Goal: to understand how fuel consumption varies over the 50 United States and the District of Columbia
# Based on the goal, the response variable should be FuelC which represents gas sold for road use
y <- fuel2001$FuelC
# Examine the response variable
summary(fuel2001$FuelC)
# Visualize distribution of fuel comsumption across all states
hist(fuel2001$FuelC,
     main = "Distribution of Fuel Comsumption Across All States",
     xlab = "Fuel Comsumption (thousands of gallons)",
     col = "lightblue",
     breaks = 15
     )

# • Study the overview of each variable by using initial descriptive and graphical univariate analysis
# install.packages("psych")
library(psych)
describe(fuel2001)

# Drivers
hist(fuel2001$Drivers,
     main = "Distribution of Licensed Drivers",
     xlab = "Number of Drivers",
     col = "lightblue",
     breaks = 20
     )
boxplot(fuel2001$Drivers,
        main = "Boxplot: Licensed Drivers",
        ylab = "Number of Drivers",
        col = "lightblue",
        horizontal = FALSE
        )
# FuelC
hist(fuel2001$FuelC,
     main = "Distribution of Fuel Consumption",
     xlab = "gas sold",
     col = "lightblue",
     breaks = 20
)
boxplot(fuel2001$FuelC,
        main = "Boxplot: Fuel Consumption",
        ylab = "gas sold",
        col = "lightblue"
        )
# Income
hist(fuel2001$Income,
     main = "Distribution of Income",
     xlab = "Income per person",
     col = "lightblue",
     breaks = 20
    )
boxplot(fuel2001$Income,
        main = "Boxplot: Income",
        ylab = "Income per person",
        col = "lightblue",
        horizontal = FALSE
        )
# Miles
hist(fuel2001$Miles,
     main = "Distribution of Miles",
     xlab = "Miles of Federal-aid Highway",
     col = "lightblue",
     breaks = 20
)
boxplot(fuel2001$Income,
        main = "Boxplot: Miles",
        ylab = "Miles of Federal-aid Highway",
        col = "lightblue",
        horizontal = FALSE
)

# Pop
hist(fuel2001$Pop,
     main = "Distribution of Population",
     xlab = "Population of age 16 and over",
     col = "lightblue",
     breaks = 20
)
boxplot(fuel2001$Pop,
        main = "Boxplot: Population",
        ylab = "Population of age 16 and over",
        col = "lightblue",
        horizontal = FALSE
)
# Tax
hist(fuel2001$Tax,
     main = "Distribution of Tax",
     xlab = "Tax rate for gasoline",
     col = "lightblue",
     breaks = 20
)
boxplot(fuel2001$Income,
        main = "Boxplot: Tax",
        ylab = "Tax rate for gasoline",
        col = "lightblue",
        horizontal = FALSE
)

# • Construct the correlation plots across the variables
# install.packages("corrplot")
# install.packages("PerformanceAnalytics")
# install.packages("GGally")
library(corrplot)
library(PerformanceAnalytics)
library(GGally)
library(ggplot2)

# Correlation Matrix
cor_matrix <- cor(fuel2001)
round(cor_matrix, 3)

# Correlation Plot
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.8,
         title = "Correlation Matrix",
         col = colorRampPalette(c("blue", "white", "red"))(200)
         )

# Scatterplot Matrix
pairs(fuel2001,
      main = "Scatterplot Matrix - All Variables",
      lower.panel = NULL
      )

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * abs(r) * 2)
}
pairs(fuel2001,
      lower.panel = panel.smooth,
      upper.panel = panel.cor,
      main = "Scatterplot Matrix with Correlation")

# • Visualize the relation between response variables and predictor variables.

# FuelC vs Drivers
ggplot(fuel2001, aes(x = Drivers, y = FuelC)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  labs(title = "Fuel Consumption vs Drivers",
       x = "Licensed Drivers",
       y = "Fuel Consumption (1000s gallons)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# FuelC vs Income
ggplot(fuel2001, aes(x = Income, y = FuelC)) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  labs(title = "Fuel Consumption vs Income",
       x = "Per Capita Income ($1000s)",
       y = "Fuel Consumption (1000s gallons)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# FuelC vs Miles
ggplot(fuel2001, aes(x = Miles, y = FuelC)) +
  geom_point(color = "orange", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  labs(title = "Fuel Consumption vs Highway Miles",
       x = "Federal-aid Highway Miles",
       y = "Fuel Consumption (1000s gallons)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# FuelC vs Pop
ggplot(fuel2001, aes(x = Pop, y = FuelC)) +
  geom_point(color = "purple", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  labs(title = "Fuel Consumption vs Population",
       x = "Population (16+)",
       y = "Fuel Consumption (1000s gallons)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# FuelC vs Tax
ggplot(fuel2001, aes(x = Tax, y = FuelC)) +
  geom_point(color = "firebrick", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2) +
  labs(title = "Fuel Consumption vs Gas Tax",
       x = "Gas Tax Rate (cents/gallon)",
       y = "Fuel Consumption (1000s gallons)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Section 2: the nassCDS data
# In this section of the exam, we focus on the nassCDS data which is a US data from police-reported car
# crashes (1997-2002) in which there is a harmful event (people or property).

# Load and inspect dataset
# install.packages("DAAG")
library("DAAG")
data(nassCDS)
names(nassCDS)
head(nassCDS)
str(nassCDS)

# Question 1
# In this question we focus on the accident’s outcome (the variable dead) and seatbelt usage (the variable seatbelt).
# 1. How many individuals used seatbelt?
seatbelt_counts <- table(nassCDS$seatbelt)
print(seatbelt_counts)

# 2. What is the distribution of seatbelt usage across the accident’s outcome factor ? 
# Produce a 2X2 table that shows the number of seatbelt users (belted/none) and accident’s outcome (alive/dead)?
contigency_table <- table(nassCDS$seatbelt, nassCDS$dead)
print(contigency_table)

# 3. Write a function that can be used to conduct inference for proportions in two independent populations.
# The null hypothesis is that there is no difference between the proportions in the two populations. Test
# the null hypothesis against a two sided alternative. The input of the function should be the 2X2 table
# in the previous item (Question 1.2) and the output should be the test statistic and the p value. Apply
# your function to test the null hypothesis that the proportion of deaths among individuals who used
# seatbelt is equal to the proportion of deaths among the individuals who did not use seatbelt.

# H_0: p_belted = p_none (proportions of deaths are equal)
# H_a: p_belted != p_none(proprotions of deaths are different)
two_proportion_test <- function(contigency_table){
  # Check if table has correct dimensions
  if(!all(dim(contigency_table) == c(2, 2))){
    stop("Input must be a 2x2 table")
  }
  
  # Extract counts
  n1_dead <- contigency_table[1, 2] # belted & dead
  n1_total <- sum(contigency_table[1, ]) # total belted
  
  n2_dead <- contigency_table[2, 2] # none & dead
  n2_total <- sum(contigency_table[2, ]) # total none
  
  # Calculate sample proportions
  p1 <- n1_dead / n1_total # Proportion of deaths among belted
  p2 <- n2_dead / n2_total # Proportion of deaths among none
  
  # Calculate pooled proportion under H_0
  p_pooled <- (n1_dead+n2_dead)/(n1_total+n2_total)
  
  # Calculate standard error under H_0
  se <- sqrt(p_pooled*(1-p_pooled)*(1/n1_total+1/n2_total))
  
  # Calculate z-test statistic
  z_stat <- (p1 - p2)/se
  
  # Calculate two-sided p-value
  p_value <- 2*pnorm(-abs(z_stat))
  
  result <- list(
    test_statistic = z_stat,
    p_value = p_value,
    proportion_belted = p1,
    proportion_none = p2,
    pooled_proportion = p_pooled,
    sample_size_belted = n1_total,
    sample_size_none = n2_total
  )
  
  return(result)
}

test_result <-two_proportion_test(contigency_table)
test_result

if (test_result$p_value<0.05) {
  cat("Conclusion: Reject H_0 at alpha = 0.05 level.\n")
  cat("There is significant evidence that the proportion of deaths differs between those who used seatbelts and those who did not.\n")
} else {
  cat("Conclusion: Fail to Reject H_0 at alpha = 0.05 level.\n")
  cat("There is insufficient evidence to conclude that the proportions differ.\n")
}

# 4. Use a barplot to visualize the distribution of the seatbelt usage across the factor levels of the accident’s outcome.
barplot(contigency_table,
        beside = TRUE,
        main = "Seatbelt Usage by Outcome",
        xlab = "Accident Outcome",
        ylab = "Frequency",
        col = c("lightblue", "pink"),
        legend.text = TRUE,
        args.legend = list(title = "Seatbelt", x = "topright")
        )


# Question 2
# In this question we focus on the outcome of the accident (dead/alive, the variable dead) and the age of the occupant (the variable ageOFocc).
# 1. What is the mean and standard deviation of the age of occupant by accident outcome?
mean_age <- tapply(nassCDS$ageOFocc, nassCDS$dead, mean, na.rm=TRUE)
sd_age <- tapply(nassCDS$ageOFocc, nassCDS$dead, sd, na.rm=TRUE)
n_age <- tapply(nassCDS$ageOFocc, nassCDS$dead, function(x) sum(!is.na(x)))

summary_table <- data.frame(
  Outcome = names(mean_age),
  Mean = round(mean_age, 2),
  SD = round(sd_age, 2),
  N = n_age
)
summary_table

# 2. Use a boxplot to visualize the distribution of the occupants’ age by accident outcome and add the data points on the boxplot.
boxplot(ageOFocc ~ dead,
        data = nassCDS,
        main = "Distribution of Occupant Age by Accident Outcome",
        xlab = "Accident Outcome",
        ylab = "Age pf Occupant (years)",
        col = c("lightblue", "pink"),
        notch = FALSE,
        outline = TRUE
        )

# Add data point with jitter
set.seed(123) # For reproducibility of jitter
stripchart(ageOFocc ~ dead,
           data = nassCDS,
           vertical = TRUE,
           method = "jitter",
           add = TRUE,
           pch = 16,
           col = rgb(0, 0, 0, 0.2),
           cex = 0.6,
           jitter = 0.2
           ) 

# Add mean points
points(1:2, mean_age, pch = 18, col = "blue", cex = 2)

legend("topright",
       legend = c("Median", "Mean", "Data points"),
       pch = c(NA, 18, 16),
       lty = c(1, NA, NA),
       lwd = c(2, NA, NA),
       col = c("black", "blue", rgb(0, 0, 0, 0.2)),
       cex = 0.8
       )

# Add grid
grid(nx=NA, ny=NULL, col="gray", lty="dotted")

# 3. Calculate a 95% confidence interval for the mean difference of the age of occupant using t distribution.
# Extract age data for each group
age_alive <- nassCDS$ageOFocc[nassCDS$dead == "alive"]
age_dead <- nassCDS$ageOFocc[nassCDS$dead == "dead"]

# Remove NA values
age_alive <- age_alive[!is.na(age_alive)]
age_dead <- age_dead[!is.na(age_dead)]

# Calculate sample statistics
n1 <- length(age_alive)
n2 <- length(age_dead)
mean1 <- mean(age_alive)
mean2 <- mean(age_dead)
sd1 <- sd(age_alive)
sd2 <- sd(age_dead)
var1 <- var(age_alive)
var2 <- var(age_dead)

# Mean difference (Dead - Alive)
mean_diff <- mean2 - mean1

"Group Statistics: \n"
cat("Alive: n =", n1, ", mean =", round(mean1, 2), ", sd = ", round(sd1, 2))
cat("Dead: n =", n2, ", mean =", round(mean2, 2), ", sd = ", round(sd2, 2))

# Calculate pooled standard deviation and standard error
# Using Welch's approach (unequal variances)
se_diff <- sqrt(var1/n1+var2/n2)

# Degree of freedom using Welch-Statterthwaite equation
df <- (var1/n1+var2/n2)^2/((var1/n1)^2/(n1-1)+(var2/n2)^2/(n2-1))

# Calculate 95% CI using Welch's t-test approach
t_critical <- qt(0.975, df = df)
ci_lower <- mean_diff - t_critical * se_diff
ci_upper <- mean_diff + t_critical * se_diff

"Welch's t-test (unequal variances assumed)\n"
cat("Degrees of freedom:", round(df, 2))
cat("Standard error:", round(se_diff, 4))
cat("t-critical value:", round(t_critical, 4))
cat("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]")

# Interpretation
cat("The 95% CI for the mean age difference (Dead - Alive) using Welch's method is [", round(ci_lower, 2), ",", round(ci_upper, 2), "] years old")
if (ci_lower > 0 && ci_upper > 0){
  cat("Since the entire CI is positive, occupants who died were significantly older than those who survived.")
} else if (ci_lower < 0 && ci_upper > 0){
  cat("Since the entire CI is negative, occupants who died were significantly younger on average than those who survived.")
} else {
  cat("Since CI includes zero, there is no significant difference in mean age between those who died and those who survived at the 95% confidence level.")
}


# Question 3
# 1. Visualize the distribution of the occupant age by sex.
boxplot(ageOFocc ~ sex,
        data = nassCDS,
        main = "Age Distribution by Sex",
        xlab = "Sex",
        ylab = "Age of Occupant (years)",
        col = c("lightblue", "lightpink")
        )

# Add mean points
mean_age_sex <- tapply(nassCDS$ageOFocc, nassCDS$sex, mean, na.rm=TRUE)
points(1:2, mean_age_sex, pch = 18, col = "red", cex = 2)

# Histogram for females
hist(nassCDS$ageOFocc[nassCDS$sex == "f"],
     main = "Age Distibution - Females",
     xlab = "Age (years)",
     ylab = "Frequency",
     col = "lightpink"
     )

hist(nassCDS$ageOFocc[nassCDS$sex == "m"],
     main = "Age Distibution - Males",
     xlab = "Age (years)",
     ylab = "Frequency",
     col = "lightblue"
)

# 2. How many occupants over the age of 50 years old survived the accident?
over_50_survived <- sum(nassCDS$ageOFocc > 50 & nassCDS$dead == "alive", na.rm = TRUE)
over_50_survived

# 3. Add a binary variable AgeOFocc_class that takes the value of 1 when 
# the occupant age is over 50 years and 0 for when the occupant age is 50 years or less.
nassCDS$AgeOFocc_class <- ifelse(nassCDS$ageOFocc > 50, 1, 0)
table(nassCDS$AgeOFocc_class, useNA = "ifany")

# 4. Create a data frame, nassCDS_o50, containing occupants older than 50 years old. This data frame
# should contain the variables dead, airbag, weight, and injSeverity. Remove the observations with missing values.
nassCDS_50 <- nassCDS[nassCDS$ageOFocc > 50 & !is.na(nassCDS$ageOFocc),
                      c("dead", "airbag", "weight", "injSeverity")]
table(nassCDS_50)

# 5. What is the dimension of the new data ?
dim(nassCDS_50)
str(nassCDS_50)


# 6. Among the occupants who are older than 50 years old, use a barplot to visualize the distribution of
# airbag across the levels of the accident outcome (dead/alive). The variable dead should be on the x-axis.
airbag_outcome_table <- table(nassCDS_50$dead, nassCDS_50$airbag)
airbag_outcome_table

barplot(t(airbag_outcome_table),
        beside = TRUE,
        main = "Airbag Deployment by Outcome\n(Occupants > 50 years old)",
        xlab = "Accident Outcome",
        ylab = "Frequency",
        col = c("lightgreen", "orange","red"),
        legend.text = TRUE,
        args.legend = list(title = "Airbag",
                           x = "topright",
                           cex = 0.8
                           )
        )

# 7. Among the occupants who are older than 50 years old visualize the distribution of 
# airbag across the level of injury sevirity (the variable injSeverity).
airbag_injury_table <- table(nassCDS_50$injSeverity, nassCDS_50$airbag)
airbag_injury_table

# Question 4
# Write a R function that receives as an input the nassCDS dataset. The function should conduct the following analysis:
analyze_mortality <- function(dataset){
  cat("    MORTALITY ANALYSIS FUNCTION\n")
  
  # 1. Select only the observations for which the accident outcome is “dead”
  dead_data <- dataset[dataset$dead == "dead" & !is.na(dataset$dead), ]
  cat("Step 1: Filtered dataset for 'dead' outcomes\n")
  cat("  Number of deaths:", nrow(dead_data), "\n\n")
  
  # 2. Calculate percentage of deaths out of the overall number of observations
  total_obs <- nrow(dataset)
  num_deaths <- nrow(dead_data)
  pct_deaths <- (num_deaths / total_obs) * 100
  
  cat("Step 2: Percentage of deaths\n")
  cat("  Total observations:", total_obs, "\n")
  cat("  Deaths:", num_deaths, "\n")
  cat("  Percentage:", round(pct_deaths, 2), "%\n\n")
  
  # 3. Calculate the percentages of females and males among the occupants who died in the accident
  sex_counts <- table(dead_data$sex)
  sex_percentages <- (sex_counts / num_deaths) * 100
  
  pct_female <- sex_percentages["f"]
  pct_male <- sex_percentages["m"]
  
  cat("Step 3: Sex distribution among deaths\n")
  cat("  Females:", sex_counts["f"], "(", round(pct_female, 2), "%)\n")
  cat("  Males:", sex_counts["m"], "(", round(pct_male, 2), "%)\n\n")
  
  # 4. Show the most frequent severity of their injuries
  injury_counts <- table(dead_data$injSeverity)
  most_frequent_injury <- names(which.max(injury_counts))
  most_frequent_count <- max(injury_counts)
  
  cat("Step 4: Most frequent injury severity\n")
  cat("  Injury severity distribution:\n")
  print(injury_counts)
  cat("\n  Most frequent:", most_frequent_injury, 
      "(n =", most_frequent_count, ")\n\n")
  
  # 5. Calculate the minimum and maximum age of the occupant (the variable ageOFocc)
  min_age <- min(dead_data$ageOFocc, na.rm = TRUE)
  max_age <- max(dead_data$ageOFocc, na.rm = TRUE)
  mean_age <- mean(dead_data$ageOFocc, na.rm = TRUE)
  median_age <- median(dead_data$ageOFocc, na.rm = TRUE)
  
  cat("Step 5: Age statistics\n")
  cat("  Minimum age:", min_age, "years\n")
  cat("  Maximum age:", max_age, "years\n")
  cat("  Mean age:", round(mean_age, 2), "years\n")
  cat("  Median age:", median_age, "years\n\n")
  
  numerical_output <- data.frame(
    Metric = c("Percentage of Deaths (%)",
               "Percentage Female among Deaths (%)",
               "Percentage Male among Deaths (%)",
               "Most Frequent Injury Severity",
               "Minimum Age (years)",
               "Maximum Age (years)"),
    Value = c(round(pct_deaths, 2),
              round(pct_female, 2),
              round(pct_male, 2),
              most_frequent_injury,
              min_age,
              max_age)
  )
  
  # 6. produce a histogram with the severity of injuries on the x axis and the occupant’s age on the y axis
  cat("Step 6: Creating histogram\n\n")
  
  # Remove NA values for plotting
  plot_data <- dead_data[!is.na(dead_data$injSeverity) & 
                           !is.na(dead_data$ageOFocc), ]
  
  # Get unique injury severity levels
  injury_levels <- sort(unique(plot_data$injSeverity))
  n_levels <- length(injury_levels)
  
  # Set up the plotting area
  par(mfrow = c(1, 1), mar = c(8, 5, 4, 2))
  
  # Create a grouped histogram
  # We'll use barplot with injury severity on x-axis and mean/median age on y-axis
  
  # Calculate statistics for each injury severity level
  age_by_severity <- tapply(plot_data$ageOFocc, plot_data$injSeverity, 
                            function(x) c(mean = mean(x, na.rm = TRUE),
                                          median = median(x, na.rm = TRUE),
                                          n = length(x)))
  
  # Extract means and counts
  mean_ages <- sapply(age_by_severity, function(x) x[1])
  counts <- sapply(age_by_severity, function(x) x[3])
  
  # Create barplot (histogram) with injury severity on x-axis
  barplot_obj <- barplot(mean_ages,
                         names.arg = names(mean_ages),
                         main = "Mean Age by Injury Severity\n(Among Fatalities)",
                         xlab = "",
                         ylab = "Mean Age of Occupant (years)",
                         col = rainbow(n_levels),
                         border = "black",
                         las = 2,
                         ylim = c(0, max(mean_ages) * 1.2),
                         cex.names = 0.7)
  
  # Add x-axis label with more space
  mtext("Injury Severity", side = 1, line = 6.5, cex = 1)
  
  # Add sample sizes on top of each bar
  text(x = barplot_obj, 
       y = mean_ages + max(mean_ages) * 0.05, 
       labels = paste0("n=", counts),
       cex = 0.8,
       col = "blue",
       font = 2)
  
  # Add value labels on bars
  text(x = barplot_obj, 
       y = mean_ages/2, 
       labels = round(mean_ages, 1),
       cex = 0.8,
       col = "white",
       font = 2)
  
  # Add grid for better readability
  grid(nx = NA, ny = NULL, col = "gray", lty = "dotted")
  
  # Add overall mean line
  abline(h = mean_age, col = "red", lwd = 2, lty = 2)
  legend("topright", 
         legend = c("Mean age per severity", "Overall mean age"),
         fill = c("gray", NA),
         border = c("black", NA),
         lty = c(NA, 2),
         lwd = c(NA, 2),
         col = c(NA, "red"),
         cex = 0.8,
         bg = "white")
  
  
  cat("ANALYSIS COMPLETE\n")
  cat("Numerical Output Table:\n")
  print(numerical_output, row.names = FALSE)
  cat("\n")
  
  # Return both outputs as a list
  return(invisible(list(
    numerical_output = numerical_output,
    plot_created = TRUE,
    dead_data = dead_data,
    summary_stats = list(
      total_observations = total_obs,
      number_of_deaths = num_deaths,
      percentage_deaths = pct_deaths,
      sex_distribution = sex_percentages,
      injury_distribution = injury_counts,
      age_min = min_age,
      age_max = max_age,
      age_mean = mean_age,
      age_median = median_age
    )
  )))
}

analyze_mortality(nassCDS)


# This SINGLE Function should return two outputs:
#   • Numerical output: 4.2,4.3,4.4 and 4.5 as a table.
#   • Graphical output: 4.6 as a plot.


# Question 5
# 1. Create a new data frame which contains only occupants who used seatbelt.
seatbelt_users <- nassCDS[nassCDS$seatbelt == "belted" & !is.na(nassCDS$seatbelt), ]
names(seatbelt_users)

# 2. How many occupants used seatbelt ?
n_seatbelt_users <- nrow(seatbelt_users)
n_seatbelt_users

total_occupants <- nrow(nassCDS)
total_occupants

pct_seatbelt <- (n_seatbelt_users/total_occupants) * 100
pct_seatbelt

# 3. Among the individuals who used seatbelt, how many died and how many survived the accident ?
outcome_table <- table(seatbelt_users$dead)
n_died <- outcome_table["dead"]
n_survived <- outcome_table["alive"]

barplot(outcome_table,
        main = "Accident Outcome\n(Seatbelt Users)",
        ylab = "Number of Occupants",
        col = c("lightgreen", "lightcoral"),
        border = "black",
        ylim = c(0, max(outcome_table)*1.1)
        )

text(x=c(0.7, 1.9),
     y = outcome_table + max(outcome_table)*0.05,
     labels = outcome_table
     )

pie(outcome_table,
   main = "Accident Outcome\n(Seatbelt Users)",
   col = c("lightgreen", "lightcoral"),
   labels = paste0(names(outcome_table), "\n", outcome_table, "(", round(outcome_table/sum(outcome_table)*100, 1), "%)")
   )

# 4. Among the individuals who used seatbelt, how many were drivers among 
# the individuals who died and how many were passengers among the individuals 
# who survived the accident (use the variable occRole to identify drivers/passengers) ?
died_seatbelt <- seatbelt_users[seatbelt_users$dead == "dead" & !is.na(seatbelt_users$dead), ]
drivers_died <- sum(died_seatbelt$occRole == "driver", na.rm = TRUE)
drivers_died

role_died_table <- table(died_seatbelt$occRole)
role_died_table

survived_seatbelt <- seatbelt_users[seatbelt_users$dead == "alive" & !is.na(seatbelt_users$dead), ]
passengers_survived <- sum(survived_seatbelt$occRole == "pass", na.rm = TRUE)
passengers_survived

role_survived_table <- table (survived_seatbelt$occRole)
role_survived_table

summary_role <- data.frame(
  Outcome = c("Died", "Survived"),
  Drivers = c(sum(died_seatbelt$occRole == "driver", na.rm=TRUE),
              sum(survived_seatbelt$occRole == "driver", na.rm=TRUE)),
  Passengers = c(sum(died_seatbelt$occRole == "pass", na.rm=TRUE),
                 sum(survived_seatbelt$occRole == "pass", na.rm=TRUE))
)

summary_role

barplot(t(as.matrix(summary_role[, -1])),
        beside = TRUE,
        names.arg = summary_role$Outcome,
        main = "Role by Outcome\n(Seatbelt Users)",
        ylab = "Number of Occupants",
        xlab = "Accident Outcome",
        col = c("steelblue", "orange"),
        legend.text = c("Drivers", "Passengers"),
        args.legend = list(x="topright", cex=0.8)
        )

prop_matrix <- prop.table(as.matrix(summary_role[, -1]), margin = 1)
barplot(t(prop_matrix),
        beside = FALSE,
        names.arg = summary_role$Outcome,
        main = "Role Distribution by Outcome\n(Seatbelt Users)",
        ylab = "Proportion",
        xlab = "Accident Outcome",
        col = c("steelblue", "orange"),
        legend.text = c("Drivers", "Passengers"),
        args.legend = list(x = "topright", cex = 0.8),
        ylim = c(0, 1))


# 5. Sort the data frame according to the injury’s severity and the occupant age.
seatbelt_users_sorted <- seatbelt_users[order(seatbelt_users$injSeverity, seatbelt_users$ageOFocc), ]
head(seatbelt_users_sorted[, c("injSeverity", "ageOFocc", "dead", "occRole", "weight")], 10)
tail(seatbelt_users_sorted[, c("injSeverity", "ageOFocc", "dead", "occRole", "weight")], 10)


# 6. Print the 25 occupants with the highest weight.
seatbelt_users_by_weight <- seatbelt_users[order(seatbelt_users$weight, decreasing=TRUE), ]
top_25_weight <- head(seatbelt_users_by_weight, 25)

display_table <- data.frame(
  Rank = 1:25,
  Weight = top_25_weight$weight,
  Age = top_25_weight$ageOFocc,
  Sex = top_25_weight$sex,
  Outcome = top_25_weight$dead,
  Role = top_25_weight$occRole,
  Injury = top_25_weight$injSeverity,
  Airbag = top_25_weight$airbag
)

display_table


# Question 6
# 1. Produce the figure below.

# Prepare the data - remove missing values for the variables we need
plot_data <- nassCDS[!is.na(nassCDS$injSeverity) & 
                       !is.na(nassCDS$ageOFocc) & 
                       !is.na(nassCDS$airbag) & 
                       !is.na(nassCDS$seatbelt), ]
plot_data$injSeverity <- factor(plot_data$injSeverity)

p <- ggplot(plot_data, aes(x = injSeverity, y = ageOFocc, fill = injSeverity)) +
  geom_boxplot(outlier.size = 1.5, outlier.alpha = 0.5) +
  facet_grid(vars(seatbelt), vars(airbag)) +
  scale_fill_manual(values = c("0" = "#08306B", 
                               "1" = "#08519C",
                               "2" = "#2171B5", 
                               "3" = "#4292C6",
                               "4" = "#6BAED6",
                               "5" = "#9ECAE1",
                               "6" = "#C6DBEF"),
                    name = "injSeverity") +
  labs(x = "injSeverity",
       y = "ageOFocc",
       title = NULL) +
  theme_minimal() +
  theme(
    strip.background = element_rect(fill = "lightgray", colour = "gray50"),
    strip.text = element_text(size = 11, face = "bold", colour = "black"),
    panel.spacing = unit(0.5, "lines"),
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_line(colour = "gray95"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 9),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), limits = c(0, 100))

# Display the plot
print(p)







