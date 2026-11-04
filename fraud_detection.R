
# Financial Fraud Detection
# Stage 1: Load and inspect the dataset

library(data.table)

data <- fread("PS_20174392719_1491204439457_log.csv")

# Display dataset information
cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")

# Display column names
print(names(data))

# Display the first six transactions
print(head(data))

# Check missing values
print(colSums(is.na(data)))

# Count genuine and fraudulent transactions
print(table(data$isFraud))
