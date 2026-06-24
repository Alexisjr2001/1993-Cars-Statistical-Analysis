# ==============================================================================
# SCRIPT 8b: MULTIPLE LINEAR REGRESSION (Stepwise with Log-Transform)
# Dependent Variable: log(MinimumPrice)
# Predictors: Selected via Regression Tree + Interaction
# ==============================================================================

library(ggplot2)
library(car) # For VIF check if available

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

# --- 1. TRANSFORMATION ---
# Apply Log transformation to fix right-skewness
cars_data$log_Price <- log(cars_data$MinimumPrice)

# Histogram of Transformed Variable (Check for Normality)
p_log <- ggplot(cars_data, aes(x = log_Price)) +
  geom_histogram(binwidth = 0.2, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of log(Minimum Price)", x = "log(Price)") +
  theme_minimal()
print(p_log)

# --- 2. TRAIN / TEST SPLIT (70% - 30%) ---
set.seed(123) # Ensure reproducibility
n <- nrow(cars_data)
train_indices <- sample(1:n, size = 0.7 * n)

train_data <- cars_data[train_indices, ]
test_data  <- cars_data[-train_indices, ]

print(paste("Training Set:", nrow(train_data), "| Test Set:", nrow(test_data)))


# --- 3. MODEL SELECTION (Stepwise AIC) ---
# We start with the variables identified by your Tree Analysis.
# We also include the interaction Horsepower * LuggageCapa as observed.
start_model <- lm(log_Price ~ Horsepower + CityMPG + Type + WheelBase + 
                    Weight + LuggageCapa + Horsepower:LuggageCapa, 
                  data = train_data)

print("--- Starting Model Summary ---")
print(summary(start_model)$adj.r.squared)

# Run Stepwise Regression (Backwards/Both) to remove non-significant terms
# This minimizes AIC (Akaike Information Criterion)
final_model <- step(start_model, direction = "both", trace = 0) # trace=0 hides the step-by-step output

print("--- Final Stepwise Model Summary ---")
print(summary(final_model))

# Check Variance Inflation Factors (VIF) for Multicollinearity
# Ideally VIF < 5 or 10.
if(require(car)) {
  print("--- VIF (Multicollinearity Check) ---")
  print(vif(final_model))
}


# --- 4. RESIDUAL DIAGNOSTICS ---
# Check Linearity and Normality of residuals
par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))

# Normality test of residuals
shapiro_test <- shapiro.test(residuals(final_model))
print(paste("Shapiro-Wilk p-value (Residuals):", round(shapiro_test$p.value, 4)))


# --- 5. VALIDATION ON TEST SET ---
# Predict log values
pred_log <- predict(final_model, newdata = test_data)

# Transform back to original scale ($1000s) using exp()
pred_original <- exp(pred_log)
actual_original <- test_data$MinimumPrice

# Calculate Error Metrics
mse <- mean((actual_original - pred_original)^2)
rmse <- sqrt(mse)
mae <- mean(abs(actual_original - pred_original))

print("--- Model Performance on Test Set ---")
print(paste("RMSE (Root Mean Squared Error):", round(rmse, 2), "($1000s)"))
print(paste("MAE  (Mean Absolute Error):    ", round(mae, 2), "($1000s)"))

# Visual Validation
p_val <- ggplot(data.frame(Actual = actual_original, Predicted = pred_original), 
                aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Price (Test Set)",
       subtitle = paste("RMSE:", round(rmse, 2)),
       x = "Actual Minimum Price", y = "Predicted Minimum Price") +
  theme_minimal()
print(p_val)