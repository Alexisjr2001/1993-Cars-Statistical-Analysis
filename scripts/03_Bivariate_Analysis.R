# ==============================================================================
# SCRIPT 3: BIVARIATE ANALYSIS
# Research Question 1: Price Difference by Origin (Domestic vs Foreign)
# ==============================================================================

# Load necessary libraries
library(ggplot2)

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

print(">>> RQ1: MidrangePrice vs. Domestic <<<")

# --- 1. VISUALIZATION (Boxplot) ---
# This gives us the first visual clue about medians and spread.
p1 <- ggplot(cars_data, aes(x = Domestic, y = MidrangePrice, fill = Domestic)) +
  geom_boxplot(alpha = 0.6) +
  labs(title = "Price Distribution: Domestic vs. Foreign", y = "Price ($1000s)") +
  theme_minimal()
print(p1)

# --- 2. ASSUMPTION CHECK: NORMALITY ---
# T-test assumes data follows a normal distribution.

# ==============================================================================
# 1. CHECK SAMPLE SIZES BY GROUP
# ==============================================================================

cat(strrep("=", 78), "\n")
cat("SAMPLE SIZE CHECK FOR TWO-SAMPLE T-TEST\n")
cat(strrep("=", 78), "\n\n")

# Count observations in each group
sample_sizes <- table(cars_data$Domestic)
print(sample_sizes)

cat("\n")

# Extract individual group sizes
n_usa <- sum(cars_data$Domestic == "U.S. manufacturer")
n_non_usa <- sum(cars_data$Domestic == "Non-U.S. manufacturer")

cat("Sample size for USA cars:     n1 =", n_usa, "\n")
cat("Sample size for non-USA cars: n2 =", n_non_usa, "\n\n")

# ==============================================================================
# 2. CHECK IF CLT APPLIES (n > 30)
# ==============================================================================

cat("Central Limit Theorem (CLT) Check:\n")
cat(strrep("-", 40), "\n")

if (n_usa > 30) {
  cat("✓ USA group (n1 = ", n_usa, ") > 30: CLT APPLIES\n", sep = "")
} else {
  cat("✗ USA group (n1 = ", n_usa, ") ≤ 30: CLT does NOT apply\n", sep = "")
  cat("  → Need to check normality assumption\n")
}

if (n_non_usa > 30) {
  cat("✓ non-USA group (n2 = ", n_non_usa, ") > 30: CLT APPLIES\n", sep = "")
} else {
  cat("✗ non-USA group (n2 = ", n_non_usa, ") ≤ 30: CLT does NOT apply\n", sep = "")
  cat("  → Need to check normality assumption\n")
}

cat("\n")


# --- 3. ASSUMPTION CHECK: EQUALITY OF VARIANCES (F-Test) ---
# Before running the T-test, we need to know if variances are equal (Homoscedasticity).
# This decides if we use Student's t-test (var.equal=TRUE) or Welch's t-test (var.equal=FALSE).
print("--- F-test for Equality of Variances ---")
var_test <- var.test(MidrangePrice ~ Domestic, data = cars_data)
print(var_test)


# --- 4. HYPOTHESIS TESTING ---
# H0: The mean price is EQUAL for Domestic and Foreign cars.
# H1: The mean price is DIFFERENT.

# Parametric Test (T-test)
# Note: Set var.equal to TRUE or FALSE based on the F-test result above.
# For now, I default to FALSE (Welch's) as it is safer, but check the F-test result!
print("--- Independent Samples T-test ---")
t_test_res <- t.test(MidrangePrice ~ Domestic, data = cars_data, var.equal = FALSE)
print(t_test_res)