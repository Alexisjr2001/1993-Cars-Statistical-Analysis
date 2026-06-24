# ==============================================================================
# SCRIPT: ANCOVA
# RQ5: ANCOVA - Type effect on MPG controlling for Horsepower
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
# RQ5: ANCOVA (HighwayMPG ~ Type + Horsepower)
# ------------------------------------------------------------------------------
print(">>> RQ5: ANCOVA (Type & Horsepower on HighwayMPG) <<<")

# --- 1. VISUALIZATION (Slopes by Group) ---
# We check if the lines for each Type look roughly parallel.
p5 <- ggplot(cars_data, aes(x = Horsepower, y = HighwayMPG, color = Type)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + # Add linear lines for each group
  labs(title = "ANCOVA: MPG vs HP by Car Type",
       subtitle = "Checking for Parallel Slopes") +
  theme_minimal()
print(p5)

# --- 2. CHECK FOR INTERACTION (Homogeneity of Slopes) ---
# H0: Slopes are equal (No interaction).
# H1: Slopes are different (Interaction exists).
# We fit a model WITH interaction (*) first.
interaction_model <- aov(HighwayMPG ~ Horsepower * Type, data = cars_data)
print("--- Interaction Test (Slopes) ---")
print(summary(interaction_model))

# --- 3. RUN ANCOVA (Main Effects) ---
# If the interaction p-value > 0.05, we drop the interaction term (+)
# and test the main effects.
ancova_model <- aov(HighwayMPG ~ Horsepower + Type, data = cars_data)

print("--- ANCOVA Table (Main Effects) ---")
# We look at Type p-value to see if it matters after controlling for HP.
print(summary(ancova_model))