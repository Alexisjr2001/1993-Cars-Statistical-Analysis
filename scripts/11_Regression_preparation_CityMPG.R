# ==============================================================================
# SCRIPT: REGRESSION MODELING FOR COUNT DATA (CityMPG) - Preparation
# Methodology:
# 1. Distribution Fitting (Poisson vs Negative Binomial)
# 2. Factor Analysis (Means)
# 3. Interaction Discovery (Trees)
# 4. Stepwise Regression (GLM)
# ==============================================================================

library(fitdistrplus) # For fitting distributions
library(MASS)         # For glm.nb (Negative Binomial)
library(tree)         # For interaction trees
library(ggplot2)

# Load data
cars_data <- read.csv("cleaned_93Cars.csv", stringsAsFactors = TRUE)

# *Re-apply Factor Order for Cylinders* (CSV does not store factor order metadata)
cars_data$NumberOfCylinders <- factor(cars_data$NumberOfCylinders, 
                                      levels = c("3", "4", "5", "6", "8", "rotary"))

# *Re-apply Factor Order for AirBags (Ordinal Variable)*
# Hierarchy: none < driver only < driver & passenger
cars_data$AirBags <- factor(cars_data$AirBags, 
                            levels = c("none", "driver only", "driver & passenger"), 
                            ordered = TRUE)

# PREPARATION: Select potential predictors
# We exclude Price variables and HighwayMPG (target leakage)
count_data <- subset(cars_data, select = -c(Manufacturer, Model, 
                                            MinimumPrice, MidrangePrice, MaximumPrice, 
                                            HighwayMPG, TotalAvgMPG))

# Define the target variable
target <- cars_data$CityMPG


# ------------------------------------------------------------------------------
# STEP 1: FIND THE DISTRIBUTION THAT FITS THE DATA
# ------------------------------------------------------------------------------
print(">>> STEP 1: Distribution Fitting <<<")

# Fit Poisson
fit_pois <- fitdist(target, "pois")

# Fit Negative Binomial
# We use fitdist but need to specify the method often for discrete data
fit_nbinom <- fitdist(target, "nbinom")

# Compare CDF Plots
par(mfrow = c(1, 1))
cdfcomp(list(fit_pois, fit_nbinom), 
        legendtext = c("Poisson", "Negative Binomial"), 
        fitlty = 1, 
        main = "CDF - Poisson vs NegBinomial for CityMPG")

# Goodness of Fit Statistics
gof_res <- gofstat(list(fit_pois, fit_nbinom), 
                   fitnames = c("Poisson", "Negative Binomial"))

print("--- Goodness of Fit Statistics ---")
print(gof_res$aic)
print(gof_res$chisqpvalue)

# DECISION LOGIC:
# We check which model has the lower AIC.
if(gof_res$aic["Poisson"] < gof_res$aic["Negative Binomial"]) {
  chosen_family <- "poisson"
  print(">>> DECISION: Poisson distribution fits better (Lower AIC).")
} else {
  chosen_family <- "negbin"
  print(">>> DECISION: Negative Binomial fits better (Lower AIC).")
}


# ------------------------------------------------------------------------------
# STEP 2: RELATIONSHIPS BETWEEN DEPENDENT & FACTORS
# ------------------------------------------------------------------------------
print(">>> STEP 2: Factor Analysis (tapply) <<<")

# Check Mean CityMPG by DriveTrain
print("--- Mean MPG by DriveTrain ---")
print(tapply(cars_data$CityMPG, cars_data$DriveTrainType, mean))

# Check Mean MPG by Manual Transmission
print("--- Mean MPG by Manual Transmission ---")
print(tapply(cars_data$CityMPG, cars_data$ManualTrans, mean))

# Check Mean MPG by Cylinders
print("--- Mean MPG by Cylinders ---")
print(tapply(cars_data$CityMPG, cars_data$NumberOfCylinders, mean))

# Check Mean MPG by Type
print("--- Mean MPG by Car Type ---")
print(tapply(cars_data$CityMPG, cars_data$Type, mean))


# ------------------------------------------------------------------------------
# STEP 3: DISCOVER INTERACTIONS USING TREES
# ------------------------------------------------------------------------------
print(">>> STEP 3: Interaction Tree <<<")

# Fit tree using all remaining variables
tree_interact <- tree(CityMPG ~ ., data = count_data)

# Plot
plot(tree_interact)
text(tree_interact, pretty = 0)
title("Regression Tree for CityMPG Interactions")

print("--- Variables used in Tree ---")
print(summary(tree_interact))