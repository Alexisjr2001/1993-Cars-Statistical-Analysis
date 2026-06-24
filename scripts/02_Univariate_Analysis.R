# ==============================================================================
# SCRIPT 2: UNIVARIATE STATISTICAL ANALYSIS
# Input:  cleaned_93Cars.csv
# Structure: Analysis by Conceptual Clusters
# ==============================================================================

# --- 1. SETUP & LIBRARIES ---
library(ggplot2)
library(gridExtra)    # For arranging plots
library(psych)        # For Skewness, Kurtosis
library(pastecs)      # For detailed descriptive stats
library(reshape2)     # For reshaping data for plotting
library(summarytools) # For frequency tables

# --- 2. LOAD CLEANED DATA ---
# We read the standard CSV we created in Script 1
cars_data <- read.csv("cleaned_93Cars.csv", stringsAsFactors = TRUE)

# *Re-apply Factor Order for Cylinders* (CSV does not store factor order metadata)
cars_data$NumberOfCylinders <- factor(cars_data$NumberOfCylinders, 
                                      levels = c("3", "4", "5", "6", "8", "rotary"))

# *Re-apply Factor Order for AirBags (Ordinal Variable)*
# Hierarchy: none < driver only < driver & passenger
cars_data$AirBags <- factor(cars_data$AirBags, 
                            levels = c("none", "driver only", "driver & passenger"), 
                            ordered = TRUE)


# ==============================================================================
# CLUSTER 1: IDENTIFICATION & CLASSIFICATION
# Variables: Manufacturer, Model, Type, Domestic
# ==============================================================================
print(">>> CLUSTER 1: IDENTIFICATION <<<")

# --- Manufacturer --- Categorical (Nominal)

# Create a frequency table (counts per manufacturer)
freq_table <- table(cars_data$Manufacturer)

# Sort it from highest to lowest to make it readable
sorted_freq <- sort(freq_table, decreasing = TRUE)
print("All Manufacturers by Count:")
sorted_freq

p_man <- ggplot(cars_data, aes(x = reorder(Manufacturer, Manufacturer, function(x)-length(x)))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution by Manufacturer", x = "Manufacturer", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display plot
print(p_man)

# --- Model --- Categorical (Nominal)
# Check Uniqueness
# If number of unique models == number of rows, it acts as an ID.
num_models <- length(unique(cars_data$Model))
num_rows <- nrow(cars_data)

print(paste("Unique Models:", num_models))
print(paste("Total Rows:", num_rows))

if(num_models == num_rows) {
  print("Conclusion: 'Model' acts as a unique identifier for the Sample Units. No visualization needed.")
} else {
  print("Conclusion: There are duplicate model names.")
}

# --- Type ---Categorical (Nominal)
print("Frequency of Car Types:")
print(freq(cars_data$Type))

p_type <- ggplot(cars_data, aes(x = Type, fill = Type)) +
  geom_bar(color = "black", alpha = 0.8) +
  labs(title = "Distribution by Car Type", y = "Count") +
  theme_minimal() + scale_fill_brewer(palette = "Pastel1") + theme(legend.position = "none")

# --- Domestic --- Categorical (Binary)
p_dom <- ggplot(cars_data, aes(x = Domestic, fill = Domestic)) +
  geom_bar() +
  labs(title = "Origin (Domestic vs Foreign)", y = "Count") +
  theme_minimal() + scale_fill_brewer(palette = "Set1") + theme(legend.position = "none")

# Display plot
grid.arrange(p_type, p_dom, ncol = 2)


# ==============================================================================
# CLUSTER 2: PRICE
# Variables: MinimumPrice, MidrangePrice, MaximumPrice
# ==============================================================================
print(">>> CLUSTER 2: PRICE <<<")

price_vars <- cars_data[, c("MinimumPrice", "MidrangePrice", "MaximumPrice")]

# Consistency check:
# MidrangePrice should be the average of MinimumPrice and MaximumPrice
# Allowing for small numerical tolerance (0.1)
price_diff <- abs(
  cars_data$MidrangePrice -
    (cars_data$MinimumPrice + cars_data$MaximumPrice) / 2
)

print(
  paste(
    "Data consistency check:",
    "Any MidrangePrice values differing by more than 0.1?",
    any(price_diff > 0.1)
  )
)

# Statistics
options(scipen=100)
print(round(stat.desc(price_vars, basic = FALSE, norm = TRUE), 3))

# Visualization Function
# Function to generate histogram + boxplot
create_price_plots <- function(data, var_label, var_col, use_log_scale = FALSE) {
  
  # Mean and SD
  var_mean <- mean(data[[var_col]], na.rm = TRUE)
  var_sd   <- sd(data[[var_col]], na.rm = TRUE)
  
  # Histogram with normal fit
  p_hist <- ggplot(data, aes(x = .data[[var_col]])) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 15,
      fill = "lightblue",
      color = "black"
    ) +
    labs(
      title = paste("Distribution of", var_label, 
                    ifelse(use_log_scale, "(Log Scale)", "")),
      x = ifelse(use_log_scale, "Price ($1000s, log scale)", "Price ($1000s)"),
      y = "Density"
    ) +
    theme_minimal()
  
  # Add normal curve only if NOT using log scale
  if (!use_log_scale) {
    p_hist <- p_hist +
      stat_function(
        fun = dnorm,
        args = list(mean = var_mean, sd = var_sd),
        color = "red",
        linewidth = 1,
        linetype = "dashed"
      )
  }
  
  # Add log scale if requested
  if (use_log_scale) {
    p_hist <- p_hist + scale_x_log10()
  }
  
  # Boxplot
  p_box <- ggplot(data, aes(y = .data[[var_col]])) +
    geom_boxplot(
      fill = "orange",
      alpha = 0.6,
      outlier.colour = "red",
      outlier.size = 3
    ) +
    labs(
      title = paste("Boxplot of", var_label,
                    ifelse(use_log_scale, "(Log Scale)", "")),
      y = ifelse(use_log_scale, "Price ($1000s, log scale)", "Price ($1000s)")
    ) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  # Add log scale if requested
  if (use_log_scale) {
    p_box <- p_box + scale_y_log10()
  }
  
  list(p_hist, p_box)
}

# Generate plots - NORMAL SCALE
plots_min_price <- create_price_plots(cars_data, "Minimum Price", "MinimumPrice")
plots_mid_price <- create_price_plots(cars_data, "Midrange Price", "MidrangePrice")
plots_max_price <- create_price_plots(cars_data, "Maximum Price", "MaximumPrice")

# Arrange plots (3 rows x 2 columns)
grid.arrange(
  plots_min_price[[1]], plots_min_price[[2]],
  plots_mid_price[[1]], plots_mid_price[[2]],
  plots_max_price[[1]], plots_max_price[[2]],
  ncol = 2
)

# Generate plots - LOG SCALE
plots_min_price_log <- create_price_plots(cars_data, "Minimum Price", "MinimumPrice", use_log_scale = TRUE)
plots_mid_price_log <- create_price_plots(cars_data, "Midrange Price", "MidrangePrice", use_log_scale = TRUE)
plots_max_price_log <- create_price_plots(cars_data, "Maximum Price", "MaximumPrice", use_log_scale = TRUE)

# Arrange plots (3 rows x 2 columns) - LOG SCALE
grid.arrange(
  plots_min_price_log[[1]], plots_min_price_log[[2]],
  plots_mid_price_log[[1]], plots_mid_price_log[[2]],
  plots_max_price_log[[1]], plots_max_price_log[[2]],
  ncol = 2
)

# ==============================================================================
# CLUSTER 3: FUEL EFFICIENCY
# Variables: CityMPG, HighwayMPG, TotalAvgMPG
# ==============================================================================
print(">>> CLUSTER 3: FUEL EFFICIENCY <<<")
mpg_vars <- cars_data[, c("CityMPG", "HighwayMPG", "TotalAvgMPG")]


# Consistency check:
# TotalAvgMPG should be the average of CityMPG and HighwayMPG
# Allowing for small numerical tolerance (0.1)
mpg_diff <- abs(
  cars_data$TotalAvgMPG -
    (cars_data$CityMPG + cars_data$HighwayMPG) / 2
)

cat("Mean Absolute Error (Simple Mean):", mean(mpg_diff), "\n")

# Statistics
print(psych::describe(mpg_vars))

# NORMALITY CHECK: Shapiro-Wilk Test
sw_city    <- shapiro.test(mpg_vars$CityMPG)
sw_highway <- shapiro.test(mpg_vars$HighwayMPG)
sw_total   <- shapiro.test(mpg_vars$TotalAvgMPG)

sw_results <- data.frame(
  Variable    = c("CityMPG", "HighwayMPG", "TotalAvgMPG"),
  W_statistic = round(c(sw_city$statistic, sw_highway$statistic, sw_total$statistic), 4),
  p_value     = round(c(sw_city$p.value,   sw_highway$p.value,   sw_total$p.value),   4),
  Normal      = ifelse(c(sw_city$p.value, sw_highway$p.value, sw_total$p.value) > 0.05, "Yes", "No")
)
print("===== Shapiro-Wilk Normality Test (α = 0.05) =====")
print(sw_results, row.names = FALSE)

# Correlation Plot
mpg_corr <- cor(cars_data$CityMPG, cars_data$HighwayMPG)
p_corr <- ggplot(cars_data, aes(x = CityMPG, y = HighwayMPG)) +
  geom_point(color = "darkgreen", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +
  labs(title = "City vs Highway MPG", 
       subtitle = paste("Correlation r =", round(mpg_corr, 3))) +
  theme_minimal()

# Convert boxplot to ggplot2
library(tidyr)
mpg_long <- mpg_vars %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "MPG")

p_boxplot <- ggplot(mpg_long, aes(x = Variable, y = MPG, fill = Variable)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("CityMPG" = "lightgreen", 
                               "HighwayMPG" = "lightblue", 
                               "TotalAvgMPG" = "yellow")) +
  labs(title = "Boxplot of Fuel Efficiency", 
       y = "Miles Per Gallon (MPG)", 
       x = "") +
  theme_minimal() +
  theme(legend.position = "none")

# Arrange plots side by side
grid.arrange(p_corr, p_boxplot, ncol = 2)

# Investigate on outliers

# Function to identify upper outliers using the IQR method
identify_upper_outliers <- function(data, column_name) {
  # Calculate quartiles and IQR
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  # Upper fence (Tukey's method)
  upper_fence <- Q3 + 1.5 * IQR_val
  
  # Identify observations above the upper fence
  outlier_indices <- which(data[[column_name]] > upper_fence)
  
  # Return results
  list(
    upper_fence = upper_fence,
    outlier_indices = outlier_indices,
    n_outliers = length(outlier_indices)
  )
}

# Identify exceptionally fuel-efficient cars in City driving
city_outliers <- identify_upper_outliers(cars_data, "CityMPG")
cat("City MPG Analysis:\n")
cat("Upper fence (Q3 + 1.5*IQR):", city_outliers$upper_fence, "mpg\n")
cat("Number of exceptionally efficient cars:", city_outliers$n_outliers, "\n\n")

# Display the exceptionally fuel-efficient cars (City)
if (city_outliers$n_outliers > 0) {
  cat("Exceptionally fuel-efficient cars in CITY driving:\n")
  print(cars_data[city_outliers$outlier_indices, 
                  c("Manufacturer", "Model", "Type", "CityMPG", "HighwayMPG")])
  cat("\n")
}

# Identify exceptionally fuel-efficient cars in Highway driving
highway_outliers <- identify_upper_outliers(cars_data, "HighwayMPG")
cat("Highway MPG Analysis:\n")
cat("Upper fence (Q3 + 1.5*IQR):", highway_outliers$upper_fence, "mpg\n")
cat("Number of exceptionally efficient cars:", highway_outliers$n_outliers, "\n\n")

# Display the exceptionally fuel-efficient cars (Highway)
if (highway_outliers$n_outliers > 0) {
  cat("Exceptionally fuel-efficient cars in HIGHWAY driving:\n")
  print(cars_data[highway_outliers$outlier_indices, 
                  c("Manufacturer", "Model", "Type", "CityMPG", "HighwayMPG")])
  cat("\n")
}

# Identify cars that are exceptionally efficient in BOTH conditions
both_outliers <- intersect(city_outliers$outlier_indices, 
                           highway_outliers$outlier_indices)

if (length(both_outliers) > 0) {
  cat("Cars exceptionally fuel-efficient in BOTH driving conditions:\n")
  print(cars_data[both_outliers, 
                  c("Manufacturer", "Model", "Type", "CityMPG", "HighwayMPG")])
} else {
  cat("No cars are exceptionally fuel-efficient in both conditions.\n")
}


# ==============================================================================
# CLUSTER 4: ENGINE & POWERTRAIN
# Variables: Cylinders, EngineSize, Horsepower, RPM, ERM
# ==============================================================================
print(">>> CLUSTER 4: ENGINE & POWERTRAIN <<<")

# Tip: A car's powertrain is the complete assembly of components that generate power
# and deliver it to the road surface to make the vehicle move

# ==========================================
# 1. Cylinder Bar Chart
# ==========================================
# Custom color palette per cylinder group
cyl_colors <- c("3" = "#2ecc71", "4" = "#3498db", "5" = "#9b59b6", 
                "6" = "#e67e22", "8" = "#e74c3c", "rotary" = "#1abc9c")

p_cyl <- ggplot(cars_data, aes(x = NumberOfCylinders, fill = NumberOfCylinders)) +
  geom_bar(color = "black", linewidth = 0.6, width = 0.6) +
  geom_text(stat = "count", aes(label = after_stat(count)), 
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = cyl_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Cylinder Distribution",
    subtitle = "Note: Mazda RX-7 (Rotary Engine) included as separate category",
    x = "Cylinder Type",
    y = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey50", size = 9),
    axis.title    = element_text(color = "grey40"),
    panel.grid.major.x = element_blank()
  )


# ==========================================
# 2. Engine Stats Histogram Matrix
# ==========================================
eng_vars <- cars_data[, c("EngineSize", "Horsepower", "RPM", "ERM")]
eng_long <- melt(eng_vars)

# Custom labels for facet panels
eng_labels <- c(
  EngineSize = "Engine Size (Liters)",
  Horsepower = "Horsepower (HP)",
  RPM        = "RPM at Max HP",
  ERM        = "Engine Revs per Mile"
)

# Calculate means per variable
eng_means <- aggregate(value ~ variable, data = eng_long, FUN = mean)

# Custom color per variable
eng_colors <- c(EngineSize = "#3498db", Horsepower = "#e74c3c", 
                RPM = "#2ecc71", ERM = "#9b59b6")

p_eng_matrix <- ggplot(eng_long, aes(x = value, fill = variable)) +
  geom_histogram(bins = 15, color = "white", linewidth = 0.5) +
  geom_vline(data = eng_means, aes(xintercept = value),
             color = "black", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~variable, scales = "free", ncol = 2, 
             labeller = labeller(variable = eng_labels)) +
  scale_fill_manual(values = eng_colors, guide = "none") +
  labs(
    title    = "Engine Characteristics Distribution",
    subtitle = "Dashed line indicates the mean",
    x        = NULL,
    y        = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey50", size = 9),
    axis.title.y  = element_text(color = "grey40"),
    strip.text    = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank()
  )

# ==========================================
# 3. Arrange Side by Side
# ==========================================
grid.arrange(p_cyl, p_eng_matrix, ncol = 2,
             widths = c(1, 1.4))


# ==============================================================================
# CLUSTER 5: TRANSMISSION & DRIVETRAIN
# Variables: DriveTrainType, ManualTrans
# ==============================================================================
print(">>> CLUSTER 5: TRANSMISSION <<<")

# ==========================================
# 1. Drivetrain Plot
# ==========================================
drive_colors <- c("all wheel drive"   = "#2ecc71", 
                  "front wheel drive" = "#3498db", 
                  "rear wheel drive"  = "#e74c3c")

p_drive <- ggplot(cars_data, aes(x = DriveTrainType, fill = DriveTrainType)) +
  geom_bar(color = "black", linewidth = 0.6, width = 0.55) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, fontface = "bold", size = 3.8) +
  scale_fill_manual(values = drive_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Drivetrain Configuration",
    subtitle = "Distribution across all 93 cars",
    x        = "Drive Type",
    y        = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey50", size = 9),
    axis.title         = element_text(color = "grey40"),
    panel.grid.major.x = element_blank()
  )

# ==========================================
# 2. Manual Transmission Plot
# ==========================================
p_manual <- ggplot(cars_data, aes(x = ManualTrans, fill = ManualTrans)) +
  geom_bar(color = "black", linewidth = 0.6, width = 0.45) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, fontface = "bold", size = 3.8) +
  scale_fill_manual(values = c("No"  = "#e74c3c", 
                               "Yes" = "#2ecc71"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Manual Transmission",
    subtitle = "Availability across all 93 cars",
    x        = "Manual Transmission",
    y        = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey50", size = 9),
    axis.title         = element_text(color = "grey40"),
    panel.grid.major.x = element_blank()
  )

# ==========================================
# 3. Arrange
# ==========================================
grid.arrange(p_drive, p_manual, ncol = 2)


# ==============================================================================
# CLUSTER 6: SAFETY
# Variables: AirBags
# ==============================================================================
print(">>> CLUSTER 6: SAFETY <<<")

airbag_colors <- c("none"                = "#e74c3c",
                   "driver only"         = "#f39c12",
                   "driver & passenger"  = "#2ecc71")

p_air <- ggplot(cars_data, aes(x = AirBags, fill = AirBags)) +
  geom_bar(color = "black", linewidth = 0.6, width = 0.55) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.5, fontface = "bold", size = 3.8) +
  scale_fill_manual(values = airbag_colors, guide = "none") +
  scale_x_discrete(limits = c("none", "driver only", "driver & passenger")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Airbag Configuration",
    subtitle = "Safety equipment across all 93 cars",
    x        = "Airbag Type",
    y        = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey50", size = 9),
    axis.title         = element_text(color = "grey40"),
    panel.grid.major.x = element_blank()
  )

print(p_air)


# ==============================================================================
# CLUSTER 7: SIZE & DIMENSIONS
# Variables: Length, Width, WheelBase, U_turn, Weight
# ==============================================================================
print(">>> CLUSTER 7: DIMENSIONS <<<")

dim_vars <- cars_data[, c("Length", "Width", "WheelBase", "U_turn", "Weight")]
print(round(stat.desc(dim_vars, basic = FALSE), 2))

# ==========================================
# Matrix Plot
# ==========================================
dim_long <- melt(dim_vars)

# Custom colors per variable
dim_colors <- c(Length    = "#3498db",
                Width     = "#e74c3c",
                WheelBase = "#2ecc71",
                U_turn    = "#9b59b6",
                Weight    = "#f39c12")

# Custom labels with units
dim_labels <- c(Length    = "Length (inches)",
                Width     = "Width (inches)",
                WheelBase = "Wheelbase (inches)",
                U_turn    = "U-Turn Space (feet)",
                Weight    = "Weight (lbs)")

# Calculate means per variable for vertical lines
dim_means <- aggregate(value ~ variable, data = dim_long, FUN = mean)

p_dim <- ggplot(dim_long, aes(x = value, fill = variable)) +
  geom_histogram(bins = 15, color = "white", linewidth = 0.5) +
  geom_vline(data = dim_means, aes(xintercept = value),
             color = "black", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~variable, scales = "free", ncol = 3,
             labeller = labeller(variable = dim_labels)) +
  scale_fill_manual(values = dim_colors, guide = "none") +
  labs(
    title    = "Vehicle Dimensions Distribution",
    subtitle = "Dashed line indicates the mean",
    x        = NULL,
    y        = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey50", size = 9),
    axis.title.y       = element_text(color = "grey40"),
    strip.text         = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank()
  )

print(p_dim)


# ==============================================================================
# CLUSTER 8: INTERIOR SPACE & CAPACITY
# Variables: Passengers, Rear_seat, LuggageCapa, FuelTankCapacity
# ==============================================================================
print(">>> CLUSTER 8: CAPACITY <<<")

# Statistics
cap_vars <- cars_data[, c("Passengers", "Rear_seat", "LuggageCapa", "FuelTankCapacity")]
print(psych::describe(cap_vars))

# ==========================================
# Visualization
# ==========================================
cap_long <- melt(cap_vars)

# Custom colors per variable
cap_colors <- c(Passengers       = "#3498db",
                Rear_seat         = "#e74c3c",
                LuggageCapa       = "#2ecc71",
                FuelTankCapacity  = "#f39c12")

# Custom labels with units
cap_labels <- c(Passengers       = "Passengers (persons)",
                Rear_seat         = "Rear Seat Room (inches)",
                LuggageCapa       = "Luggage Capacity (cu. ft.)",
                FuelTankCapacity  = "Fuel Tank Capacity (gallons)")

# Calculate means per variable
cap_means <- aggregate(value ~ variable, data = cap_long, FUN = mean)

p_cap <- ggplot(cap_long, aes(x = value, fill = variable)) +
  geom_histogram(bins = 15, color = "white", linewidth = 0.5) +
  geom_vline(data = cap_means, aes(xintercept = value),
             color = "black", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~variable, scales = "free", ncol = 2,
             labeller = labeller(variable = cap_labels)) +
  scale_fill_manual(values = cap_colors, guide = "none") +
  labs(
    title    = "Interior Space & Capacity Distribution",
    subtitle = "Dashed line indicates the mean",
    x        = NULL,
    y        = "Count"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey50", size = 9),
    axis.title.y       = element_text(color = "grey40"),
    strip.text         = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank()
  )

print(p_cap)

# ==============================================================================
# END OF ANALYSIS
# ==============================================================================