# ==============================================================================
# SCRIPT 8: REGRESSION ANALYSIS - STEP 1 (Exploratory Tree)
# Goal: Use Regression Tree to find key variables/interactions for MinimumPrice
# ==============================================================================

# Install 'tree' if not already installed (uncomment if needed)
# install.packages("tree")

library(tree)
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

# --- 1. DATA PREPARATION FOR REGRESSION ---
# We must REMOVE 'MidrangePrice' and 'MaximumPrice' because they are 
# directly correlated with MinimumPrice (Leakage).
# We also remove 'Model' and 'Manufacturer' as they have too many levels for a simple tree.
reg_data <- subset(cars_data, select = -c(MidrangePrice, MaximumPrice, Model, Manufacturer))

# --- 2. DECIDING THE REGRESSION MODEL (Distribution Check) ---
# We check if MinimumPrice is skewed. If it's highly skewed, we might need log().
print(">>> Distribution Check for MinimumPrice <<<")
summary(reg_data$MinimumPrice)

# Histogram
p_dist <- ggplot(reg_data, aes(x = MinimumPrice)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Minimum Price", x = "Minimum Price ($1000s)") +
  theme_minimal()
print(p_dist)

# --- 3. FITTING THE REGRESSION TREE ---
# Formula: MinimumPrice predicted by ALL other remaining variables
tree_model <- tree(MinimumPrice ~ ., data = reg_data)

# --- 4. VISUALIZATION ---
print(">>> Regression Tree Plot <<<")
# Basic plot as requested
plot(tree_model)
text(tree_model, pretty = 0)

# Print the summary to see which variables were actually used in the tree
print("--- Variables used in Tree Construction ---")
summary(tree_model)