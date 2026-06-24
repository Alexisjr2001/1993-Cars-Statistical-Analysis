# ==============================================================================
# SCRIPT 5: CHI-SQUARE TEST OF INDEPENDENCE
# Research Question 3: Association between Origin and Airbags
# ==============================================================================

# Load data (ensure previous cleaning script has run)
cars_data <- read.csv("cleaned_93Cars.csv", stringsAsFactors = TRUE)

# *Re-apply Factor Order for Cylinders* (CSV does not store factor order metadata)
cars_data$NumberOfCylinders <- factor(cars_data$NumberOfCylinders, 
                                      levels = c("3", "4", "5", "6", "8", "rotary"))

# *Re-apply Factor Order for AirBags (Ordinal Variable)*
# Hierarchy: none < driver only < driver & passenger
cars_data$AirBags <- factor(cars_data$AirBags, 
                            levels = c("none", "driver only", "driver & passenger"), 
                            ordered = TRUE)

print(">>> RQ3: AirBags vs. Domestic <<<")

# --- 1. CONTINGENCY TABLE ---
# Create the observed frequency table
# Slide 29: We start by creating a contingency table (crosstab)
observed_table <- table(cars_data$AirBags, cars_data$Domestic)
print("--- Observed Frequencies ---")
print(observed_table)

# --- 2. VISUALIZATION (Stacked Bar Plot) ---
library(ggplot2)
p3 <- ggplot(cars_data, aes(x = Domestic, fill = AirBags)) +
  geom_bar(position = "fill") + # "fill" creates a 100% stacked bar
  labs(title = "Airbag Availability by Origin", 
       y = "Proportion", x = "Origin") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")
print(p3)

# --- 3. CHI-SQUARE TEST ---
# H0: Variables are Independent (No association).
# H1: Variables are Dependent (Association exists).
chisq_res <- chisq.test(observed_table)

print("--- Pearson's Chi-squared Test ---")
print(chisq_res)

# --- 4. CHECK ASSUMPTIONS (Expected Counts) ---
# Slide 30: Warning appears if expected frequencies < 5.
print("--- Expected Frequencies ---")
print(round(chisq_res$expected, 2))

# --- 5. FISHER'S EXACT TEST (If Assumption Violated) ---
# Slide 31: Use Fisher's test if Chi-square assumption is violated (counts < 5).
if(any(chisq_res$expected < 5)) {
  print("--- Assumption Violated (Expected < 5). Running Fisher's Exact Test... ---")
  fisher_res <- fisher.test(observed_table)
  print(fisher_res)
} else {
  print("--- Assumptions met. Fisher's test not strictly necessary but shown for completeness. ---")
  print(fisher.test(observed_table))
}