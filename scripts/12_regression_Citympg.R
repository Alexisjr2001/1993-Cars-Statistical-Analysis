# ==============================================================================
# SCRIPT 8d: STEPWISE NEGATIVE BINOMIAL REGRESSION (Final Step)
# Dependent Variable: CityMPG
# Family: Negative Binomial (Selected via AIC)
# ==============================================================================

library(MASS) # Required for glm.nb

# Load data (ensure clean load)
cars_data <- read.csv("cleaned_93Cars.csv", stringsAsFactors = TRUE)

# *Re-apply Factor Order for Cylinders* (CSV does not store factor order metadata)
cars_data$NumberOfCylinders <- factor(cars_data$NumberOfCylinders, 
                                      levels = c("3", "4", "5", "6", "8", "rotary"))

# *Re-apply Factor Order for AirBags (Ordinal Variable)*
# Hierarchy: none < driver only < driver & passenger
cars_data$AirBags <- factor(cars_data$AirBags, 
                            levels = c("none", "driver only", "driver & passenger"), 
                            ordered = TRUE)

# Filter data to exclude Price/Target Leakage as agreed
count_data <- subset(cars_data, select = -c(Manufacturer, Model, 
                                            MinimumPrice, MidrangePrice, MaximumPrice, 
                                            HighwayMPG, TotalAvgMPG))

# --- STEP 4: FIT & REFINE MODEL ---
print(">>> STEP 4: Stepwise Negative Binomial Regression <<<")

# A. DEFINE STARTING MODEL
# We include the variables found in your TREE: 
#   Weight, Length, ERM, NumberOfCylinders, FuelTankCapacity
# PLUS the key factors from your tapply analysis:
#   ManualTrans, DriveTrainType, Type
# Note: We use glm.nb() because Negative Binomial was better than Poisson.

start_model <- glm.nb(CityMPG ~ Weight + Length + ERM + NumberOfCylinders + 
                        FuelTankCapacity + ManualTrans + DriveTrainType + Type, 
                      data = count_data)

print("--- Starting Model Summary (Before Stepwise) ---")
print(summary(start_model))

# B. RUN STEPWISE SELECTION (AIC Optimization)
# Direction = "both" tries adding/removing variables to find the best fit.
final_model <- step(start_model, direction = "both", trace = 0)

print("--- Final Stepwise Model Summary ---")
print(summary(final_model))

# C. INTERPRETATION (Multiplicative Effects)
# Coefficients in GLM are on the log scale. We exponentiate them.
# Example: If exp(coef) for Weight is 0.999, then +1 lb weight = 99.9% of original MPG.
print("--- Multiplicative Effects (exp(coef)) ---")
print(round(exp(coef(final_model)), 5))

# D. GOODNESS OF FIT
# Check Residual Deviance vs Degrees of Freedom
print(paste("Residual Deviance:", round(final_model$deviance, 2)))
print(paste("Degrees of Freedom:", final_model$df.residual))

# Pseudo R-Squared (1 - Deviance/Null Deviance)
pseudo_r2 <- 1 - (final_model$deviance / final_model$null.deviance)
print(paste("Pseudo R-Squared:", round(pseudo_r2, 4)))

# E. VISUALIZATION OF PREDICTIONS
# Plot Actual vs Predicted
predictions <- predict(final_model, type = "response")

library(ggplot2)
p_final <- ggplot(data.frame(Actual = count_data$CityMPG, Predicted = predictions), 
                  aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "City MPG: Actual vs Predicted (Negative Binomial)",
       subtitle = paste("Pseudo R2:", round(pseudo_r2, 3)),
       x = "Actual City MPG", y = "Predicted City MPG") +
  theme_minimal()
print(p_final)