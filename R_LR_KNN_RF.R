# Load Required Libraries
install.packages('tidyverse')
install.packages('caret')
install.packages('pROC')

library(tidyverse)
library(caret)
library(pROC)
library(FNN)

# Load and Preprocess Data
data <- read.csv("C:/Users/USER/Documents/Personal Project/Comprehensive Diabetes Clinical Dataset/diabetes_dataset.csv")
data_2 <- data  # Copy the original data

# Convert categorical variables to numeric
data_2$gender <- as.numeric(factor(data_2$gender)) - 1  # Male = 0, Female = 1
data_2$location <- as.numeric(factor(data_2$location))
data_2$smoking_history <- as.numeric(factor(data_2$smoking_history))
data_2$smoking_history <- ifelse(is.na(data_2$smoking_history) | data_2$smoking_history == "", -1, data_2$smoking_history)

# Cap BMI values at 45
data_2$bmi <- ifelse(data_2$bmi > 45, 45, data_2$bmi)

# Convert 'diabetes' to numeric (0 and 1)
data_2$diabetes <- as.numeric(data_2$diabetes)

# Feature Engineering: Polynomial Features and Interactions
data_2 <- data_2 %>%
  mutate(
    age_squared = age^2,
    bmi_squared = bmi^2,
    blood_glucose_level_squared = blood_glucose_level^2,
    hbA1c_level_squared = hbA1c_level^2,
    glucose_hba1c_interaction = blood_glucose_level * hbA1c_level,
    high_bmi_flag = ifelse(bmi > 30, 1, 0),
    high_glucose_flag = ifelse(blood_glucose_level > 140, 1, 0),
    age_over_50_flag = ifelse(age > 50, 1, 0)
  )

# Scale Continuous Features
data_2 <- data_2 %>%
  mutate(across(
    c(age, bmi, blood_glucose_level, hbA1c_level, age_squared, bmi_squared, 
      blood_glucose_level_squared, hbA1c_level_squared, glucose_hba1c_interaction),
    scale
  ))

# Train-Test Split
set.seed(123)
train_indices <- createDataPartition(data_2$diabetes, p = 0.8, list = FALSE)
train_data <- data_2[train_indices, ]
test_data <- data_2[-train_indices, ]

# Logistic Regression Model
set.seed(123)
logistic_model <- glm(diabetes ~ ., data = train_data, family = binomial)
summary(logistic_model)

# Predict Probabilities on Test Data
test_data$pred_prob <- predict(logistic_model, test_data, type = "response")
test_data$predicted <- ifelse(test_data$pred_prob > 0.5, "Yes", "No")

# K-Fold Cross-Validation
set.seed(123)
k <- 10
folds <- createFolds(train_data$diabetes, k = k, list = TRUE)

# Initialize Metrics
RSE_results <- numeric(k)
accuracy_results <- numeric(k)

# Define RSE Function
my_RSE <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Perform K-Fold Cross-Validation
for (i in 1:k) {
  # Split into training and validation sets
  fold_train <- train_data[-folds[[i]], ]
  fold_test <- train_data[folds[[i]], ]
  
  # Train logistic regression model
  fold_model <- glm(diabetes ~ ., data = fold_train, family = binomial)
  
  # Predict probabilities on validation set
  fold_test$pred_prob <- predict(fold_model, fold_test, type = "response")
  fold_test$predicted <- ifelse(fold_test$pred_prob > 0.5, "Yes", "No")
  
  # Calculate RSE for this fold
  fold_test$actual_numeric <- ifelse(fold_test$diabetes == "Yes", 1, 0)
  RSE_results[i] <- my_RSE(fold_test$actual_numeric, fold_test$pred_prob)
  
  # Calculate Accuracy for this fold
  confusion_matrix <- table(Predicted = fold_test$predicted, Actual = fold_test$diabetes)
  accuracy_results[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

# Compute Mean Metrics
mean_RSE <- mean(RSE_results)
mean_accuracy <- mean(accuracy_results)

# Print Cross-Validation Results
cat("\nCross-Validation Results:\n")
cat("Mean RSE across folds:", round(mean_RSE, 3), "\n")
cat("Mean Accuracy across folds:", round(mean_accuracy, 3), "\n")

# Evaluate on Test Set
final_model <- glm(diabetes ~ ., data = train_data, family = binomial)
test_data$pred_prob <- predict(final_model, test_data, type = "response")
test_data$predicted <- ifelse(test_data$pred_prob > 0.5, "Yes", "No")

# Confusion Matrix and Metrics
confusion_matrix_test <- table(Predicted = test_data$predicted, Actual = test_data$diabetes)
cat("\nConfusion Matrix (Test Set):\n")
print(confusion_matrix_test)

# Compute Precision and Accuracy
TP <- confusion_matrix_test["Yes", "1"]  # True Positives
FP <- confusion_matrix_test["Yes", "0"]  # False Positives
TN <- confusion_matrix_test["No", "0"]   # True Negatives
FN <- confusion_matrix_test["No", "1"]   # False Negatives

precision <- TP / (TP + FP)
accuracy <- (TP + TN) / (TP + TN + FP + FN)

# Print Metrics
cat("\nTest Set Metrics:\n")
cat("Precision:", round(precision, 3), "\n")
cat("Accuracy:", round(accuracy, 3), "\n")



#HYPERPARAMETER TUNING

# K-Fold Cross-Validation with Significant Variables
set.seed(123)
k <- 10
folds <- createFolds(train_data$diabetes, k = k, list = TRUE)

# Initialize Results Storage
results <- data.frame(
  Model = character(),
  Threshold = numeric(),
  Poly_Degree = integer(),
  Mean_RSE = numeric(),
  Mean_Accuracy = numeric(),
  stringsAsFactors = FALSE
)

# Define RSE Function
my_RSE <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Thresholds and Polynomial Degrees to Test
thresholds <- seq(0.3, 0.7, by = 0.1)
poly_degrees <- 1:3  # Test polynomial degrees 1, 2, and 3

for (degree in poly_degrees) {
  for (threshold in thresholds) {
    # Initialize metrics for this configuration
    RSE_results <- numeric(k)
    accuracy_results <- numeric(k)
    
    for (i in 1:k) {
      # Split into training and validation sets
      fold_train <- train_data[-folds[[i]], ]
      fold_test <- train_data[folds[[i]], ]
      
      # Create polynomial features for significant variables
      fold_train <- fold_train %>%
        mutate(
          age_poly = age^degree,
          hbA1c_poly = hbA1c_level^degree,
          glucose_poly = blood_glucose_level^degree
        )
      
      fold_test <- fold_test %>%
        mutate(
          age_poly = age^degree,
          hbA1c_poly = hbA1c_level^degree,
          glucose_poly = blood_glucose_level^degree
        )
      
      # Train Logistic Regression Model with Significant Variables
      fold_model <- glm(
        diabetes ~ gender + age_poly + race.AfricanAmerican + hypertension +
          heart_disease + smoking_history + hbA1c_poly + glucose_poly +
          age_squared + bmi_squared + blood_glucose_level_squared +
          hbA1c_level_squared + glucose_hba1c_interaction + high_bmi_flag +
          high_glucose_flag + age_over_50_flag,
        data = fold_train, family = binomial
      )
      
      # Predict Probabilities
      fold_test$pred_prob <- predict(fold_model, fold_test, type = "response")
      
      # Classify Based on the Current Threshold
      fold_test$predicted <- ifelse(fold_test$pred_prob > threshold, "Yes", "No")
      
      # Calculate RSE for this fold
      fold_test$actual_numeric <- ifelse(fold_test$diabetes == "Yes", 1, 0)
      RSE_results[i] <- my_RSE(fold_test$actual_numeric, fold_test$pred_prob)
      
      # Calculate Accuracy for this fold
      confusion_matrix <- table(Predicted = fold_test$predicted, Actual = fold_test$diabetes)
      accuracy_results[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    }
    
    # Store Results for This Configuration
    results <- rbind(
      results,
      data.frame(
        Model = paste("Poly Degree:", degree),
        Threshold = threshold,
        Poly_Degree = degree,
        Mean_RSE = mean(RSE_results),
        Mean_Accuracy = mean(accuracy_results)
      )
    )
  }
}

# View and Sort Results
results <- results[order(results$Mean_Accuracy, decreasing = TRUE), ]
print(results)

# Plot Results (Optional)
library(ggplot2)
ggplot(results, aes(x = Threshold, y = Mean_Accuracy, color = as.factor(Poly_Degree))) +
  geom_line() +
  labs(title = "Accuracy vs. Threshold and Polynomial Degree",
       x = "Threshold",
       y = "Mean Accuracy",
       color = "Polynomial Degree")


# Find the best configuration for the tuned model
best_tuned_model <- results[which.max(results$Mean_Accuracy), ]
print("Best Tuned Model Configuration:")
print(best_tuned_model)

# Compare Original and Tuned Model
comparison <- data.frame(
  Model = c("Original", "Tuned"),
  Mean_RSE = c(mean_RSE, best_tuned_model$Mean_RSE),
  Mean_Accuracy = c(mean_accuracy, best_tuned_model$Mean_Accuracy)
)
print("Comparison of Original and Tuned Model:")
print(comparison)

# Save the better model's configuration for future use
if (best_tuned_model$Mean_Accuracy > mean_accuracy) {
  cat("\nTuned model is better. Storing the configuration.\n")
  best_model_logistic <- best_tuned_model
} else {
  cat("\nOriginal model is better. Using it for the next step.\n")
  best_model_logistic <- list(Model = "Original", Mean_RSE = mean_RSE, Mean_Accuracy = mean_accuracy)
}



#GRAPHS FOR LOGISTIC REGRESSION

# Scatter Plot of Predicted Probabilities
test_data <- test_data %>%
  arrange(pred_prob) %>%
  mutate(index = 1:nrow(test_data))

ggplot(test_data, aes(x = index, y = pred_prob)) +
  geom_point(alpha = 0.7, color = "black") +
  labs(title = "Predicted Probability Distribution", x = "Index", y = "Predicted Probability") +
  theme_minimal()



# Define threshold values
cut_vec <- seq(from = 0, to = 1, by = 0.01)
test_error_vec <- numeric(length(cut_vec))

# Loop through each threshold and calculate error rates
for (i in seq_along(cut_vec)) {
  test_data <- test_data %>% 
    mutate(pred_default = ifelse(pred_prob < cut_vec[i], 0, 1))  # Classify as 0 or 1
  
  # Calculate error rate
  test_error_vec[i] <- mean(test_data$pred_default != test_data$diabetes, na.rm = TRUE)
}

# Plot Error Rate vs. Threshold
plot(cut_vec, test_error_vec, type = "l", col = "blue", lwd = 2,
     main = "Error Rate vs. Cut Off", xlab = "Threshold", ylab = "Error Rate")


























#KNN
# Load and Preprocess Data
data <- read.csv("C:/Users/USER/Documents/Personal Project/Comprehensive Diabetes Clinical Dataset/diabetes_dataset.csv")
data_2 <- data  # Copy the original data

# Convert categorical variables to numeric
data_2$gender <- as.numeric(factor(data_2$gender)) - 1  # Male = 0, Female = 1
data_2$location <- as.numeric(factor(data_2$location))
data_2$smoking_history <- as.numeric(factor(data_2$smoking_history))
data_2$smoking_history <- ifelse(is.na(data_2$smoking_history) | data_2$smoking_history == "", -1, data_2$smoking_history)

# Cap BMI values at 45
data_2$bmi <- ifelse(data_2$bmi > 45, 45, data_2$bmi)

# Convert 'diabetes' to numeric (0 and 1)
data_2$diabetes <- as.numeric(data_2$diabetes)

# Feature Engineering: Polynomial Features and Interactions
data_2 <- data_2 %>%
  mutate(
    age_squared = age^2,
    bmi_squared = bmi^2,
    blood_glucose_level_squared = blood_glucose_level^2,
    hbA1c_level_squared = hbA1c_level^2,
    glucose_hba1c_interaction = blood_glucose_level * hbA1c_level,
    high_bmi_flag = ifelse(bmi > 30, 1, 0),
    high_glucose_flag = ifelse(blood_glucose_level > 140, 1, 0),
    age_over_50_flag = ifelse(age > 50, 1, 0)
  )

# Scale Continuous Features
data_2 <- data_2 %>%
  mutate(across(
    c(age, bmi, blood_glucose_level, hbA1c_level, age_squared, bmi_squared, 
      blood_glucose_level_squared, hbA1c_level_squared, glucose_hba1c_interaction),
    scale
  ))

# Train-Test Split
set.seed(123)
train_indices <- createDataPartition(data_2$diabetes, p = 0.8, list = FALSE)
train_data <- data_2[train_indices, ]
test_data <- data_2[-train_indices, ]



# Normalize function for numerical columns
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Identify numeric columns for normalization
numeric_cols <- sapply(train_data, is.numeric)

# Normalize only numeric columns (excluding the target variable 'diabetes')
train_data_norm <- train_data
train_data_norm[, numeric_cols] <- lapply(train_data[, numeric_cols], normalize)

# Repeat for test data
test_data_norm <- test_data
test_data_norm[, numeric_cols] <- lapply(test_data[, numeric_cols], normalize)

# Define predictor and target variables
x_train <- train_data_norm[, -which(names(train_data_norm) == "diabetes")]
y_train <- train_data_norm$diabetes

x_test <- test_data_norm[, -which(names(test_data_norm) == "diabetes")]
y_test <- test_data_norm$diabetes


# Load the class package
library(class)

# Load required library for cross-validation
library(caret)

# Set a specific value for `k`
k <- 7  # Chosen value of `k`

# Initialize vectors to store results for cross-validation
fold_rse <- numeric(10)         # Store RSE for each fold
fold_accuracy <- numeric(10)    # Store accuracy for each fold
fold_precision <- numeric(10)   # Store precision for each fold

# Define RSE function
my_rse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Perform 10-fold cross-validation
set.seed(123)
folds <- createFolds(y_train, k = 10)  # Create 10 folds

for (fold_idx in seq_along(folds)) {
  # Split the data into training and validation sets
  train_indices <- setdiff(1:nrow(x_train), folds[[fold_idx]])
  validation_indices <- folds[[fold_idx]]
  
  # Subset the training and validation data
  x_fold_train <- x_train[train_indices, ]
  y_fold_train <- y_train[train_indices]
  x_fold_val <- x_train[validation_indices, ]
  y_fold_val <- y_train[validation_indices]
  
  # Train kNN model
  knn_pred <- knn(train = x_fold_train, test = x_fold_val, cl = y_fold_train, k = k)
  
  # Convert predictions and actuals to numeric for RSE calculation
  knn_pred_numeric <- as.numeric(knn_pred) - 1
  y_fold_val_numeric <- as.numeric(y_fold_val)
  
  # Compute RSE
  fold_rse[fold_idx] <- my_rse(y_fold_val_numeric, knn_pred_numeric)
  
  # Compute Accuracy
  fold_accuracy[fold_idx] <- mean(knn_pred == y_fold_val)
  
  # Compute Precision
  confusion_mat <- table(Predicted = knn_pred, Actual = y_fold_val)
  tp <- ifelse("1" %in% colnames(confusion_mat) & "1" %in% rownames(confusion_mat), confusion_mat["1", "1"], 0)
  fp <- ifelse("1" %in% rownames(confusion_mat) & "0" %in% colnames(confusion_mat), confusion_mat["1", "0"], 0)
  fold_precision[fold_idx] <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
}

# Calculate Mean Metrics
mean_rse <- mean(fold_rse)
mean_accuracy <- mean(fold_accuracy)
mean_precision <- mean(fold_precision)

# Combine results into a data frame
results_df <- data.frame(
  k = k,
  Mean_RSE = mean_rse,
  Mean_Accuracy = mean_accuracy,
  Mean_Precision = mean_precision
)

# Print the results
print(results_df)



#HYPER PARAMETER TUNING FOR KNN
# Load required libraries
library(FNN)

# Define range of `k` values and distance metrics
k_values <- c(3, 5, 7, 9, 11)  # Example of different k values
distance_metrics <- c("euclidean", "manhattan")  # Define distance metrics

# Initialize storage for results
tuning_results <- data.frame(
  k = integer(),
  DistanceMetric = character(),
  Mean_RSE = numeric(),
  Mean_Accuracy = numeric(),
  Mean_Precision = numeric(),
  stringsAsFactors = FALSE
)

# Define RSE function
my_rse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Perform hyperparameter tuning
set.seed(123)
folds <- createFolds(y_train, k = 10)  # Create 10 folds

for (distance_metric in distance_metrics) {
  for (k in k_values) {
    # Initialize vectors for RSE, accuracy, and precision for each fold
    fold_rse <- numeric(10)
    fold_accuracy <- numeric(10)
    fold_precision <- numeric(10)
    
    for (fold_idx in seq_along(folds)) {
      # Split the data into training and validation sets
      train_indices <- setdiff(1:nrow(x_train), folds[[fold_idx]])
      validation_indices <- folds[[fold_idx]]
      
      # Subset the training and validation data
      x_fold_train <- x_train[train_indices, ]
      y_fold_train <- y_train[train_indices]
      x_fold_val <- x_train[validation_indices, ]
      y_fold_val <- y_train[validation_indices]
      
      # Apply the distance metric manually
      if (distance_metric == "manhattan") {
        x_fold_train <- as.matrix(x_fold_train)
        x_fold_val <- as.matrix(x_fold_val)
        dist_func <- function(a, b) sum(abs(a - b))  # Manhattan distance
      } else {
        dist_func <- NULL  # Default is Euclidean in FNN::knn
      }
      
      # Train kNN model with FNN::knn
      knn_pred <- knn(
        train = x_fold_train,
        test = x_fold_val,
        cl = y_fold_train,
        k = k
      )
      
      # Convert predictions and actuals to numeric for RSE calculation
      knn_pred_numeric <- as.numeric(knn_pred) - 1
      y_fold_val_numeric <- as.numeric(y_fold_val)
      
      # Compute RSE
      fold_rse[fold_idx] <- my_rse(y_fold_val_numeric, knn_pred_numeric)
      
      # Compute Accuracy
      fold_accuracy[fold_idx] <- mean(knn_pred == y_fold_val)
      
      # Compute Precision
      confusion_mat <- table(Predicted = knn_pred, Actual = y_fold_val)
      tp <- ifelse("1" %in% colnames(confusion_mat) & "1" %in% rownames(confusion_mat), confusion_mat["1", "1"], 0)
      fp <- ifelse("1" %in% rownames(confusion_mat) & "0" %in% colnames(confusion_mat), confusion_mat["1", "0"], 0)
      fold_precision[fold_idx] <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    }
    
    # Store mean results for this configuration
    tuning_results <- rbind(
      tuning_results,
      data.frame(
        k = k,
        DistanceMetric = distance_metric,
        Mean_RSE = mean(fold_rse),
        Mean_Accuracy = mean(fold_accuracy),
        Mean_Precision = mean(fold_precision)
      )
    )
  }
}

# Print tuning results
print("Hyperparameter Tuning Results:")
print(tuning_results)

# Find the best configuration from tuning results
best_tuned_knn <- tuning_results[which.max(tuning_results$Mean_Accuracy), ]
cat("\nBest Tuned kNN Model Configuration:\n")
print(best_tuned_knn)


# Print comparison results
print("Comparison of Original and Tuned kNN Models:")
print(best_tuned_knn)
print(results_df)

# Store the best kNN model configuration in a variable
if (best_tuned_knn$Mean_Accuracy > results_df$Mean_Accuracy) {
  cat("\nTuned kNN model is better. Storing it as the best model.\n")
  best_model_knn <- best_tuned_knn
} else {
  cat("\nOriginal kNN model is better. Storing it as the best model.\n")
  best_model_knn <- results_df
}

# Print the best kNN model configuration
cat("\nBest kNN Model Configuration:\n")
print(best_model_knn)

#GRAPH FOR KNN
# Initialize variables for accuracy
knn_predictions <- list()
knn_accuracies <- numeric()

# Define range of k values to test
k_values <- 1:50

# Loop over k values
for (k in k_values) {
  # Train kNN model for each k
  knn_predictions[[k]] <- knn(
    train = x_train,
    test = x_test,
    cl = y_train,
    k = k
  )
  
  # Calculate accuracy for the current k
  knn_accuracies[k] <- sum(knn_predictions[[k]] == y_test) / length(y_test) * 100
}

# Plot Accuracy vs. Choice of k
plot(
  k_values, knn_accuracies, type = "b", col = "dodgerblue",
  xlab = "k, Number of Neighbors", ylab = "Classification Accuracy (%)",
  main = "Accuracy vs. Neighbors"
)

# Add vertical line indicating k with best accuracy
best_k <- which.max(knn_accuracies)
abline(v = best_k, col = "orange", lwd = 1.5)

# Add horizontal line for max accuracy
max_accuracy <- max(knn_accuracies)
abline(h = max_accuracy, col = "red", lty = 2)

# Add horizontal line for min accuracy (optional for visual reference)
min_accuracy <- min(knn_accuracies)
abline(h = min_accuracy, col = "grey", lty = 2)

# Annotate the plot with the best k value and corresponding accuracy
text(
  x = best_k, y = max_accuracy + 2,
  labels = paste("Best k =", best_k, "\nAccuracy =", round(max_accuracy, 2), "%"),
  col = "darkgreen"
)




# Compare Original (k = 7, Euclidean) and Tuned (k = 7, Manhattan) Models
comparison <- data.frame(
  Model = c("Original kNN (k = 7, Euclidean)", "Tuned kNN (k = 7, Manhattan)"),
  Mean_RSE = c(results_df$Mean_RSE, best_tuned_knn$Mean_RSE),
  Mean_Accuracy = c(results_df$Mean_Accuracy, best_tuned_knn$Mean_Accuracy),
  Mean_Precision = c(results_df$Mean_Precision, best_tuned_knn$Mean_Precision),
  Mean_MSE = c(results_df$Mean_MSE, best_tuned_knn$Mean_MSE),
  Mean_AUC = c(results_df$Mean_AUC, best_tuned_knn$Mean_AUC),
  Mean_F1_Score = c(results_df$Mean_F1_Score, best_tuned_knn$Mean_F1_Score)
)

print("\nModel Comparison:")
print(comparison)

# Visualization for Model Comparison
library(ggplot2)
ggplot(comparison, aes(x = Model, y = Mean_Accuracy)) +
  geom_bar(stat = "identity", fill = c("skyblue", "orange"), alpha = 0.8) +
  geom_text(aes(label = round(Mean_Accuracy, 3)), vjust = -0.5) +
  labs(title = "kNN Model Comparison: Accuracy", x = "Model", y = "Mean Accuracy") +
  theme_minimal()

# Visualize Precision
ggplot(comparison, aes(x = Model, y = Mean_Precision)) +
  geom_bar(stat = "identity", fill = c("lightgreen", "purple"), alpha = 0.8) +
  geom_text(aes(label = round(Mean_Precision, 3)), vjust = -0.5) +
  labs(title = "kNN Model Comparison: Precision", x = "Model", y = "Mean Precision") +
  theme_minimal()






####################  forest random  ################################

# Feature Engineering
data_2 <- data_2 %>%
  mutate(
    # Existing features
    age_squared = age^2,
    bmi_squared = bmi^2,
    blood_glucose_level_squared = blood_glucose_level^2,
    hbA1c_level_squared = hbA1c_level^2,
    glucose_hba1c_interaction = blood_glucose_level * hbA1c_level,
    high_bmi_flag = ifelse(bmi > 30, 1, 0),
    high_glucose_flag = ifelse(blood_glucose_level > 140, 1, 0),
    age_over_50_flag = ifelse(age > 50, 1, 0),
    
    # New features
    bmi_category = cut(bmi, breaks = c(-Inf, 18.5, 25, 30, Inf), 
                       labels = c("Underweight", "Normal", "Overweight", "Obese")),
    age_category = cut(age, breaks = c(-Inf, 20, 40, 60, Inf), 
                       labels = c("Youth", "Adult", "Middle-aged", "Senior"))
  )

# Convert categorical features to factors
data_2$bmi_category <- as.factor(data_2$bmi_category)
data_2$age_category <- as.factor(data_2$age_category)

# Train-Test Split
set.seed(123)
train_indices <- createDataPartition(data_2$diabetes, p = 0.8, list = FALSE)
train_data <- data_2[train_indices, ]
test_data <- data_2[-train_indices, ]

# Convert target variable to factors
train_data$diabetes <- factor(train_data$diabetes, levels = c(0, 1), labels = c("No", "Yes"))
test_data$diabetes <- factor(test_data$diabetes, levels = c(0, 1), labels = c("No", "Yes"))

### 1. Base Random Forest Model ###
set.seed(123)
base_rf_model <- randomForest(diabetes ~ ., data = train_data, ntree = 100, importance = TRUE)

# Base Model Prediction and Evaluation
base_rf_predictions <- predict(base_rf_model, newdata = test_data)
base_rf_cm <- confusionMatrix(base_rf_predictions, test_data$diabetes)

base_rf_accuracy <- base_rf_cm$overall["Accuracy"]
base_rf_auc <- auc(roc(test_data$diabetes, predict(base_rf_model, newdata = test_data, type = "prob")[, "Yes"]))
base_rf_mse <- mean((as.numeric(base_rf_predictions) - as.numeric(test_data$diabetes))^2)
base_rf_f1 <- 2 * (base_rf_cm$byClass["Precision"] * base_rf_cm$byClass["Recall"]) /
  (base_rf_cm$byClass["Precision"] + base_rf_cm$byClass["Recall"])

# Print Base Model Results
cat("\nBase Random Forest Model:\n")
cat("Accuracy:", round(base_rf_accuracy * 100, 2), "%\n")
cat("AUC:", round(base_rf_auc, 3), "\n")
cat("MSE:", round(base_rf_mse, 4), "\n")
cat("F1-Score:", round(base_rf_f1, 3), "\n")

### 2. Fine-Tuning (mtry and ntree) ###
# Define tuning grid for `mtry` and `ntree`
tune_grid <- expand.grid(
  mtry = c(2, 4, 6),   # Number of variables randomly sampled at each split
  ntree = c(50, 100, 150, 200)  # Different tree numbers
)

# Custom Training Function for `mtry` and `ntree`
tune_results <- expand.grid(mtry = c(2, 4, 6), ntree = c(50, 100, 150, 200))
tune_results$AUC <- NA

set.seed(123)
for (i in 1:nrow(tune_results)) {
  mtry_val <- tune_results$mtry[i]
  ntree_val <- tune_results$ntree[i]
  
  model <- randomForest(diabetes ~ ., data = train_data, mtry = mtry_val, ntree = ntree_val)
  pred_prob <- predict(model, newdata = test_data, type = "prob")[, "Yes"]
  roc_curve <- roc(test_data$diabetes, pred_prob)
  tune_results$AUC[i] <- auc(roc_curve)
}

# Find the best combination
best_tune <- tune_results[which.max(tune_results$AUC), ]
cat("\nBest Tuned Parameters:\n")
print(best_tune)

### 3. Fine-Tuned Random Forest Model ###
set.seed(123)
fine_rf_model <- randomForest(
  diabetes ~ ., 
  data = train_data, 
  mtry = best_tune$mtry, 
  ntree = best_tune$ntree, 
  importance = TRUE
)

# Fine-Tuned Model Prediction and Evaluation
fine_rf_predictions <- predict(fine_rf_model, newdata = test_data)
fine_rf_cm <- confusionMatrix(fine_rf_predictions, test_data$diabetes)

fine_rf_accuracy <- fine_rf_cm$overall["Accuracy"]
fine_rf_auc <- auc(roc(test_data$diabetes, predict(fine_rf_model, newdata = test_data, type = "prob")[, "Yes"]))
fine_rf_mse <- mean((as.numeric(fine_rf_predictions) - as.numeric(test_data$diabetes))^2)
fine_rf_f1 <- 2 * (fine_rf_cm$byClass["Precision"] * fine_rf_cm$byClass["Recall"]) /
  (fine_rf_cm$byClass["Precision"] + fine_rf_cm$byClass["Recall"])

# Print Fine-Tuned Model Results
cat("\nFine-Tuned Random Forest Model:\n")
cat("Accuracy:", round(fine_rf_accuracy * 100, 2), "%\n")
cat("AUC:", round(fine_rf_auc, 3), "\n")
cat("MSE:", round(fine_rf_mse, 4), "\n")
cat("F1-Score:", round(fine_rf_f1, 3), "\n")

### 4. Feature Importance ###
cat("\nFeature Importance:\n")
varImpPlot(fine_rf_model, main = "Random Forest - Feature Importance")

### Performance Comparisons ###
comparison <- data.frame(
  Model = c("Base RF Model", "Fine-Tuned RF Model"),
  Accuracy = c(base_rf_accuracy, fine_rf_accuracy),
  AUC = c(base_rf_auc, fine_rf_auc),
  F1_Score = c(base_rf_f1, fine_rf_f1),
  MSE = c(base_rf_mse, fine_rf_mse)
)

cat("\nModel Comparison:\n")
print(comparison)

### Visualization ###
# ROC Curve Comparison
roc_base <- roc(test_data$diabetes, predict(base_rf_model, newdata = test_data, type = "prob")[, "Yes"])
roc_fine <- roc(test_data$diabetes, predict(fine_rf_model, newdata = test_data, type = "prob")[, "Yes"])

plot(roc_base, col = "blue", main = "ROC Curve Comparison", lwd = 2)
lines(roc_fine, col = "red", lwd = 2)
legend("bottomright", legend = c("Base RF Model", "Fine-Tuned RF Model"), col = c("blue", "red"), lwd = 2)

# Accuracy Comparison Chart
ggplot(comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
  labs(title = "Model Comparison: Accuracy", x = "Model", y = "Accuracy") +
  theme_minimal()

# AUC vs ntree Plot
ggplot(tune_results, aes(x = ntree, y = AUC, color = as.factor(mtry))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "AUC vs. ntree by mtry", x = "Number of Trees (ntree)", y = "AUC") +
  scale_color_discrete(name = "mtry") +
  theme_minimal()





 ############## Final train ##################



# Train-Test Split
set.seed(123)
train_indices <- createDataPartition(data_2$diabetes, p = 0.8, list = FALSE)
train_data <- data_2[train_indices, ]
test_data <- data_2[-train_indices, ]

# Convert target variable to factors
train_data$diabetes <- factor(train_data$diabetes, levels = c(0, 1), labels = c("No", "Yes"))
test_data$diabetes <- factor(test_data$diabetes, levels = c(0, 1), labels = c("No", "Yes"))

### Step 1: Train Base Model ###
set.seed(123)
base_rf_model <- randomForest(diabetes ~ ., data = train_data, ntree = 100, importance = TRUE)

# Base Model Prediction and Evaluation
base_rf_predictions <- predict(base_rf_model, newdata = test_data)
base_rf_prob <- predict(base_rf_model, newdata = test_data, type = "prob")[, "Yes"]
base_rf_auc <- auc(roc(test_data$diabetes, base_rf_prob))
base_rf_accuracy <- mean(base_rf_predictions == test_data$diabetes)
base_rf_mse <- mean((as.numeric(base_rf_predictions) - as.numeric(test_data$diabetes))^2)
base_rf_cm <- confusionMatrix(base_rf_predictions, test_data$diabetes)
base_rf_f1 <- 2 * (base_rf_cm$byClass["Precision"] * base_rf_cm$byClass["Recall"]) /
  (base_rf_cm$byClass["Precision"] + base_rf_cm$byClass["Recall"])

cat("\nBase Model Performance:\n")
cat("Accuracy:", round(base_rf_accuracy * 100, 2), "%\n")
cat("AUC:", round(base_rf_auc, 3), "\n")
cat("MSE:", round(base_rf_mse, 4), "\n")
cat("F1 Score:", round(base_rf_f1, 3), "\n")

### Step 2: Hyperparameter Tuning ###
# Create a tuning grid for `mtry` and `ntree`
tune_grid <- expand.grid(
  mtry = seq(2, sqrt(ncol(train_data) - 1), by = 2),  # Dynamic range for `mtry`
  ntree = seq(50, 200, by = 50)                      # Dynamic range for `ntree`
)

# Initialize results storage
tune_results <- expand.grid(mtry = unique(tune_grid$mtry), ntree = unique(tune_grid$ntree))
tune_results$AUC <- NA

# Perform tuning
for (i in seq_len(nrow(tune_results))) {
  set.seed(123)
  model <- randomForest(
    diabetes ~ ., 
    data = train_data, 
    mtry = tune_results$mtry[i], 
    ntree = tune_results$ntree[i]
  )
  
  prob <- predict(model, newdata = test_data, type = "prob")[, "Yes"]
  roc_curve <- roc(test_data$diabetes, prob)
  tune_results$AUC[i] <- auc(roc_curve)
}

# Find the best parameters
best_params <- tune_results[which.max(tune_results$AUC), ]
cat("\nBest Tuned Random Forest Parameters:\n")
print(best_params)

### Step 3: Train Fine-Tuned Model ###
set.seed(123)
fine_rf_model <- randomForest(
  diabetes ~ ., 
  data = train_data, 
  mtry = best_params$mtry, 
  ntree = best_params$ntree, 
  importance = TRUE
)

# Fine-Tuned Model Prediction and Evaluation
fine_rf_predictions <- predict(fine_rf_model, newdata = test_data)
fine_rf_prob <- predict(fine_rf_model, newdata = test_data, type = "prob")[, "Yes"]
fine_rf_auc <- auc(roc(test_data$diabetes, fine_rf_prob))
fine_rf_accuracy <- mean(fine_rf_predictions == test_data$diabetes)
fine_rf_mse <- mean((as.numeric(fine_rf_predictions) - as.numeric(test_data$diabetes))^2)
fine_rf_cm <- confusionMatrix(fine_rf_predictions, test_data$diabetes)
fine_rf_f1 <- 2 * (fine_rf_cm$byClass["Precision"] * fine_rf_cm$byClass["Recall"]) /
  (fine_rf_cm$byClass["Precision"] + fine_rf_cm$byClass["Recall"])

cat("\nFine-Tuned Model Performance:\n")
cat("Accuracy:", round(fine_rf_accuracy * 100, 2), "%\n")
cat("AUC:", round(fine_rf_auc, 3), "\n")
cat("MSE:", round(fine_rf_mse, 4), "\n")
cat("F1 Score:", round(fine_rf_f1, 3), "\n")

### Step 4: AUC vs ntree by mtry Plot ###
ggplot(tune_results, aes(x = ntree, y = AUC, color = as.factor(mtry), group = mtry)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "AUC vs. ntree by mtry", x = "Number of Trees (ntree)", y = "AUC", color = "mtry") +
  theme_minimal()

### Step 5: Feature Importance ###
cat("\nFeature Importance:\n")
varImpPlot(fine_rf_model, main = "Random Forest - Feature Importance")

### Step 6: Comparison Table ###
comparison <- data.frame(
  Model = c("Base Model", "Fine-Tuned Model"),
  Accuracy = c(base_rf_accuracy, fine_rf_accuracy),
  AUC = c(base_rf_auc, fine_rf_auc),
  MSE = c(base_rf_mse, fine_rf_mse),
  F1_Score = c(base_rf_f1, fine_rf_f1)
)
print(comparison)

### Step 7: Comparison Chart ###
ggplot(comparison, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.5) +
  labs(title = "Model Comparison: Accuracy", x = "Model", y = "Accuracy") +
  theme_minimal()




# Calculate Performance Metrics
library(pROC)

roc_curve_bag <- roc(test_data$diabetes, yhat_bag)
auc_bag <- auc(roc_curve_bag)
cat("\nAUC (Bagged Model):", round(auc_bag, 3), "\n")

# Plot ROC Curve
plot(roc_curve_bag, col = "blue", lwd = 2, main = "Bagged Model ROC Curve")
legend("bottomright", legend = paste("AUC =", round(auc_bag, 3)), col = "blue", lwd = 2)

# Mean Squared Error (MSE)
bagged_mse <- mean((yhat_bag - test_actual)^2)
cat("\nBagged Model MSE:", round(bagged_mse, 4), "\n")












## for other findings #########

# Filter the dataset for diabetic cases
pst_cases <- data_2 %>% filter(diabetes == 1)

# Count diabetic cases by year
diabetes_year_counts <- pst_cases %>%
  group_by(year) %>%
  summarise(count = n())

# Plot the number of diabetic cases over the years
ggplot(diabetes_year_counts, aes(x = year, y = count, fill = factor(year))) +
  geom_bar(stat = "identity") +
  scale_fill_hue() +
  labs(title = "Diabetic Cases Over the Years", x = "Year", y = "Number of Diabetic Cases") +
  theme_minimal()

# Calculate average trends by year
trends <- pst_cases %>%
  group_by(year) %>%
  summarise(
    avg_age = mean(age, na.rm = TRUE),
    avg_blood_glucose_level = mean(blood_glucose_level, na.rm = TRUE),
    avg_hbA1c_level = mean(hbA1c_level, na.rm = TRUE)
  )

# Plot trends for age, blood glucose level, and HbA1c level
# Combine the plots into a grid
library(gridExtra)

# Average Age of Diabetic Cases
p1 <- ggplot(trends, aes(x = year, y = avg_age)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Average Age of Diabetic Cases Over the Years", x = "Year", y = "Average Age") +
  theme_minimal()

# Average Blood Glucose Level of Diabetic Cases
p2 <- ggplot(trends, aes(x = year, y = avg_blood_glucose_level)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Average Blood Glucose Level of Diabetic Cases Over the Years", x = "Year", y = "Blood Glucose Level") +
  theme_minimal()

# Average HbA1c Level of Diabetic Cases
p3 <- ggplot(trends, aes(x = year, y = avg_hbA1c_level)) +
  geom_line(color = "purple", size = 1) +
  labs(title = "Average HbA1c Level of Diabetic Cases Over the Years", x = "Year", y = "HbA1c Level") +
  theme_minimal()

# Arrange plots
grid.arrange(p1, p2, p3, ncol = 2)







###### other findings 

# Load necessary libraries
library(dplyr)

# Filter data for diabetic cases
diabetic_cases <- data_2 %>% filter(diabetes == 1)

# Summarize diabetes cases by location
location_summary <- diabetic_cases %>%
  group_by(location) %>%
  summarise(total_cases = n()) %>%
  arrange(desc(total_cases))

cat("Locations with the most and least diabetic cases:\n")
print(location_summary)

# Summarize diabetes cases by race
race_summary <- diabetic_cases %>%
  summarise(
    AfricanAmerican = sum(race.AfricanAmerican, na.rm = TRUE),
    Asian = sum(race.Asian, na.rm = TRUE),
    Caucasian = sum(race.Caucasian, na.rm = TRUE),
    Hispanic = sum(race.Hispanic, na.rm = TRUE),
    Other = sum(race.Other, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Race", values_to = "Cases") %>%
  arrange(desc(Cases))

cat("Races with the most and least diabetic cases:\n")
print(race_summary)

# Find location with the highest and lowest diabetes cases
most_cases_location <- location_summary %>% slice(1)
least_cases_location <- location_summary %>% slice(n())

cat("Location with the most cases:\n")
print(most_cases_location)

cat("Location with the least cases:\n")
print(least_cases_location)

# Find race with the highest and lowest diabetes cases
most_cases_race <- race_summary %>% slice(1)
least_cases_race <- race_summary %>% slice(n())

cat("Race with the most cases:\n")
print(most_cases_race)

cat("Race with the least cases:\n")
print(least_cases_race)





# Filter top 10 locations with the most diabetic cases
top_locations <- location_summary %>% slice(1:10)

# Create bar plot for top 10 locations
library(ggplot2)

ggplot(top_locations, aes(x = reorder(location, -total_cases), y = total_cases)) +
  geom_bar(stat = "identity", fill = "dodgerblue", color = "black", alpha = 0.8) +
  labs(
    title = "Top 10 Locations with the Most Diabetic Cases",
    x = "Location",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Create a bar plot for diabetes cases by race
ggplot(race_summary, aes(x = reorder(Race, -Cases), y = Cases)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha = 0.8) +
  labs(
    title = "Diabetic Cases by Race",
    x = "Race",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Combine most and least affected locations into one dataframe
extreme_locations <- bind_rows(most_cases_location, least_cases_location)

# Create a bar plot for most and least affected locations
ggplot(extreme_locations, aes(x = reorder(location, -total_cases), y = total_cases)) +
  geom_bar(stat = "identity", fill = c("red", "green"), color = "black", alpha = 0.8) +
  labs(
    title = "Most and Least Diabetic Cases by Location",
    x = "Location",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
























