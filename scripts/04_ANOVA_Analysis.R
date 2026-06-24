# ==============================================================================
# SCRIPT 4: ONE-WAY ANOVA
# Research Question 2: City Fuel Efficiency by Drive Train Type
# ==============================================================================

# Load necessary libraries
library(ggplot2)
library(DescTools)

# Load data
cars_data <- read.csv("cleaned_93Cars.csv", stringsAsFactors = TRUE)

print(">>> RQ2: CityMPG vs. DriveTrainType <<<")

# --- 1. VISUALIZATION (Boxplot) ---
# Slide 11/13: Boxplots allow us to observe the distribution for each factor level.
p2 <- ggplot(cars_data, aes(x = DriveTrainType, y = CityMPG, fill = DriveTrainType)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "City MPG Distribution by Drive Train", 
       y = "City MPG", x = "Drive Train Type") +
  theme_minimal()
print(p2)

# --- 2. FIT THE ANOVA MODEL ---
# Slide 9/10: We use aov() to create the ANOVA model.
# H0: Mean CityMPG is the same for all DriveTrainTypes.
anova_model <- aov(CityMPG ~ DriveTrainType, data = cars_data)

# --- 3. ANOVA SUMMARY ---
# Slide 10: We interpret the F-value and Pr(>F).
print("--- One-Way ANOVA Summary ---")
print(summary(anova_model))

# --- 4. POST-HOC TESTING (Tukey HSD) ---
# Slide 20: Comparison of all possible differences between groups.
# We look for p-values (p adj) < 0.05 to find significant pairs.
print("--- Tukey HSD Post-hoc Test ---")
tukey_res <- PostHocTest(aov(CityMPG ~ DriveTrainType, data = cars_data),method="hsd")
print(tukey_res)