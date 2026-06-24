# ==============================================================================
# SCRIPT 7: ONE-WAY ANOVA (Airbags vs Price)
# Research Question 6: Max Price by Airbag Availability
# ==============================================================================

# Load necessary libraries
library(ggplot2)
library(DescTools)

# Load data
cars_data <- read.csv("cleaned_93Cars.csv", stringsAsFactors = TRUE)

# Ensure AirBags is ordered correctly
cars_data$AirBags <- factor(cars_data$AirBags, 
                            levels = c("none", "driver only", "driver & passenger"), 
                            ordered = TRUE)

print(">>> RQ6: Max_Price vs. AirBags <<<")

# --- 1. VISUALIZATION (Boxplot) ---
# We expect cars with more airbags to be more expensive.
# The boxplot will show if the medians increase step-by-step.
p6 <- ggplot(cars_data, aes(x = AirBags, y = MaximumPrice, fill = AirBags)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Maximum Price Distribution by Airbag Type", 
       y = "Maximum Price ($1000s)", x = "Airbag Configuration") +
  theme_minimal() +
  scale_fill_brewer(palette = "Oranges")
print(p6)

# --- 2. FIT THE ANOVA MODEL ---
# H0: Mean Max_Price is EQUAL across all airbag groups.
# H1: At least one group is different.
anova_price <- aov(MaximumPrice ~ AirBags, data = cars_data)

print("--- One-Way ANOVA Summary ---")
print(summary(anova_price))

# --- 3. POST-HOC TESTING (Tukey HSD) ---
# Check which specific levels differ (e.g., is 'Driver Only' significantly pricier than 'None'?)
print("--- Tukey HSD Post-hoc Test ---")
tukey_res <- PostHocTest(aov(MaximumPrice ~ AirBags, data = cars_data),method="hsd")
print(tukey_res)