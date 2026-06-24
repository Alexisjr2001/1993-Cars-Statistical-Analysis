# ==============================================================================
# SCRIPT 6 : CORRELATION
# Research Question 4: Correlation between Horsepower and HighwayMPG
# ==============================================================================

# Load necessary libraries
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

# ------------------------------------------------------------------------------
# RQ4: CORRELATION ANALYSIS (Horsepower vs. HighwayMPG)
# ------------------------------------------------------------------------------
print(">>> RQ4: Horsepower vs. HighwayMPG <<<")

# --- 1. VISUALIZATION (Scatterplot) ---
# We use a scatterplot to inspect linearity and outliers.
p4 <- ggplot(cars_data, aes(x = Horsepower, y = HighwayMPG)) +
  geom_point(color = "darkred", alpha = 0.7, size = 2) +
  labs(title = "Scatterplot: Horsepower vs. Highway MPG", 
       subtitle = "Checking for negative association",
       x = "Horsepower", y = "Highway MPG") +
  theme_minimal()
print(p4)

# --- 2. CORRELATION TEST ---
# H0: Correlation = 0
# H1: Correlation != 0
# We use Pearson's correlation (assuming linearity).
cor_test <- cor.test(cars_data$Horsepower, cars_data$HighwayMPG, method = "pearson")
print("--- Pearson's Correlation Test ---")
print(cor_test)