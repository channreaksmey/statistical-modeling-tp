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

