# ==============================================================================
# SCRIPT 1: DATA CLEANING & PREPARATION
# Input:  93Cars_labels.csv (Raw Data)
# Output: cleaned_93Cars.csv (Ready for Analysis)
# ==============================================================================

# 1. Load Raw Data
# Note: Original file uses semi-colon delimiter and comma for decimals
raw_data <- read.csv("93Cars_labels.csv", sep = ";", dec = ",", stringsAsFactors = TRUE)

# Create a copy to work on
cars_data <- raw_data

# ==========================================
# 1. FIX TYPOS (Manufacturer)
# ==========================================
cars_data$Manufacturer <- as.character(cars_data$Manufacturer)
cars_data$Manufacturer[cars_data$Manufacturer == "Chrylser"] <- "Chrysler"
cars_data$Manufacturer[cars_data$Manufacturer == "Mercedes-Be"] <- "Mercedes-Benz"
cars_data$Manufacturer <- as.factor(cars_data$Manufacturer)

# ==========================================
# 2. STANDARDIZE LABELS (Type)
# ==========================================
levels(cars_data$Type)[levels(cars_data$Type)=="Compac"] <- "Compact"
levels(cars_data$Type)[levels(cars_data$Type)=="Midsiz"] <- "Midsize"

# ==========================================
# 3. IMPUTATION: HighwayMPG (Regression)
# ==========================================
# Row 55 is missing HighwayMPG. 
# We use the strong correlation with CityMPG to predict it.
mpg_model <- lm(HighwayMPG ~ CityMPG, data = cars_data)
predicted_val <- predict(mpg_model, newdata = cars_data[55, ])

# Fill and convert to integer
cars_data$HighwayMPG[55] <- round(predicted_val)
cars_data$HighwayMPG <- as.integer(cars_data$HighwayMPG)

# ==========================================
# 4. FIX STRUCTURAL ANOMALY (Cylinders)
# ==========================================
# Row 57 (Mazda RX-7) is "rotary" (no cylinders).
cars_data$NumberOfCylinders <- as.character(cars_data$NumberOfCylinders)
cars_data$NumberOfCylinders[57] <- "rotary"

# Convert to Factor with logical order
cars_data$NumberOfCylinders <- factor(cars_data$NumberOfCylinders, 
                                      levels = c("3", "4", "5", "6", "8", "rotary"))

# ==========================================
# 5. IMPUTATION: Capacity Specs
# ==========================================
# A. Rear_seat: 2-Seaters (Corvette, RX-7) have no rear seat -> Set to 0
cars_data$Rear_seat[is.na(cars_data$Rear_seat)] <- 0

# B. LuggageCapa:
# - Sports Cars (Corvette, RX-7) -> Set to 0
cars_data$LuggageCapa[cars_data$Model %in% c("Corvette", "RX-7")] <- 0

# - Vans: Use mean of 'Large' cars as proxy for cargo space
mean_large_luggage <- mean(cars_data$LuggageCapa[cars_data$Type == "Large"], na.rm = TRUE)
cars_data$LuggageCapa[cars_data$Type == "Van"] <- round(mean_large_luggage)

# ==========================================
# 6. SAVE CLEANED DATA
# ==========================================
# We save as standard CSV (comma separator, point decimal) for easier loading later
write.csv(cars_data, "cleaned_93Cars.csv", row.names = FALSE)

print("Data cleaning complete. File saved as 'cleaned_93Cars.csv'.")