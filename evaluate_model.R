
# Financial Fraud Detection
# Stage 4: Evaluate the saved Random Forest model

library(randomForest)

# Load the existing trained model
model <- readRDS("fraud_detection_model.rds")

cat("=== Random Forest Model Evaluation ===\n\n")

# Display the model's out-of-bag confusion matrix
cat("Out-of-bag confusion matrix:\n")
print(model$confusion)

# Extract counts
cm <- model$confusion

TN <- cm["0", "0"]
FN <- cm["1", "0"]
FP <- cm["0", "1"]
TP <- cm["1", "1"]

# Calculate evaluation metrics safely
precision <- if ((TP + FP) > 0) TP / (TP + FP) else 0
recall    <- if ((TP + FN) > 0) TP / (TP + FN) else 0
f1_score  <- if ((precision + recall) > 0) {
  2 * precision * recall / (precision + recall)
} else {
  0
}
accuracy <- (TP + TN) / (TP + TN + FP + FN)

cat("\n=== Evaluation Metrics ===\n")
cat(sprintf("True Negatives (TN):  %.0f\n", TN))
cat(sprintf("False Positives (FP): %.0f\n", FP))
cat(sprintf("False Negatives (FN): %.0f\n", FN))
cat(sprintf("True Positives (TP):  %.0f\n", TP))

cat(sprintf("\nAccuracy:  %.2f%%\n", 100 * accuracy))
cat(sprintf("Precision: %.2f%%\n", 100 * precision))
cat(sprintf("Recall:    %.2f%%\n", 100 * recall))
cat(sprintf("F1-score:  %.2f%%\n", 100 * f1_score))

