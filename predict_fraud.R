
# Financial Fraud Detection
# Predict one transaction
library(randomForest)

model <- readRDS("fraud_detection_model.rds")

cat("=== Financial Fraud Detection ===\n")

# Collect transaction details
step <- as.numeric(readline("Transaction step (e.g. 100): "))
type <- toupper(readline(
  "Transaction type (CASH_IN, CASH_OUT, DEBIT, PAYMENT, TRANSFER): "
))
amount_prompt <- paste0(type, " transaction amount: ")
amount <- as.numeric(readline(amount_prompt))
oldbalanceOrg <- as.numeric(readline("Original balance before transaction: "))
newbalanceOrig <- as.numeric(readline("Original balance after transaction: "))
oldbalanceDest <- as.numeric(readline("Destination balance before transaction: "))
newbalanceDest <- as.numeric(readline("Destination balance after transaction: "))

# Validate inputs
if (any(is.na(c(step, amount, oldbalanceOrg, newbalanceOrig,
                oldbalanceDest, newbalanceDest))) ||
    any(c(step, amount, oldbalanceOrg, newbalanceOrig,
          oldbalanceDest, newbalanceDest) < 0)) {
  stop("Please enter valid, non-negative numbers.")
}

allowed_types <- c("CASH_IN", "CASH_OUT", "DEBIT", "PAYMENT", "TRANSFER")

if (!type %in% allowed_types) {
  stop("Invalid transaction type.")
}

# Calculate features used when training the model
origBalanceError <- oldbalanceOrg - amount - newbalanceOrig
destBalanceError <- oldbalanceDest + amount - newbalanceDest

# Create input data with the expected feature names
transaction <- data.frame(
  step = step,
  type = factor(type, levels = model$forest$xlevels$type),
  amount = amount,
  oldbalanceOrg = oldbalanceOrg,
  newbalanceOrig = newbalanceOrig,
  oldbalanceDest = oldbalanceDest,
  newbalanceDest = newbalanceDest,
  origBalanceError = origBalanceError,
  destBalanceError = destBalanceError
)

if (is.na(transaction$type)) {
  stop("This transaction type was not used when training the model.")
}


# Predict the transaction class
prediction <- predict(
  model,
  newdata = transaction,
  type = "response"
)

# Get predicted class probabilities
probabilities <- predict(
  model,
  newdata = transaction,
  type = "prob"
)

# Extract probability of fraud (class "1")
fraud_probability <- probabilities[1, "1"] * 100

cat("\n=== Prediction Result ===\n")
cat("Predicted class:", as.character(prediction), "\n")
cat(sprintf("Estimated fraud probability: %.2f%%\n",
            fraud_probability))

if (as.character(prediction) == "1") {
  cat("Warning: Potential fraudulent transaction.\n")
} else {
  cat("Result: Transaction classified as genuine.\n")
}

