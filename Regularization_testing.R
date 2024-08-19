# Load necessary libraries
library(readr)
library(glmnet)

# Load the CSV file into 'vessels' data frame
vessels <- read_csv("Data/vessels.csv")
View(vessels)  # Use View() to inspect the data interactively in RStudio

# Si subtraction (assuming this step is to adjust X1 for "AGUIX")
vessels$X1[vessels$X3 == "AGUIX"] <- vessels$X1[vessels$X3 == "AGUIX"] * (1 - 0.0569)

# Filter data to include only non-NA values in 'y'
df <- vessels[!is.na(vessels$y), ]

# Selecting data for a specific vessel (e.g., "aorta")
df <- df[df$X4 == "aorta", ]

# Set seed for reproducibility
set.seed(123)

# Determine the size of the training set (e.g., 80% training, 20% testing)
train_size <- 0.8  # 80% training data, adjust as needed

# Create an index for splitting the data
train_index <- sample(nrow(df), size = round(train_size * nrow(df)))

# Split data into training and testing sets
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

X_train <- as.matrix(train_data[, c("X1", "X2", "X6")])  # Adjust if using more predictors
y_train <- train_data$y

X_test <- as.matrix(test_data[, c("X1", "X2", "X6")])  # Adjust if using more predictors
y_test <- test_data$y


# Fit Linear Regression (OLS) using X1
lm_model <- lm(y_train ~ X1, data = train_data)

# Predict using linear regression model (lm_model)
y_pred_lm <- predict(lm_model, newdata = test_data)

# Calculate Mean Squared Error (MSE) for the linear regression model
mse_lm <- mean((y_pred_lm - test_data$y)^2)
print(paste("Mean Squared Error (Linear Model):", mse_lm))

# Plot Linear Regression fit
plot(test_data$y, y_pred_lm, main = "Linear Regression Fit", xlab = "Actual values", ylab = "Predicted values")
abline(0, 1, col = "red")  # Add 45-degree line for perfect prediction
###

# Fit ridge regression model using training data
ridge_model <- glmnet(X_train, y_train, alpha = 0)

# Perform cross-validation to select the optimal lambda (regularization parameter)
cv_model <- cv.glmnet(X_train, y_train, alpha = 0)

# Print the optimal lambda (for information)
print(cv_model$lambda.min)

###
# Predict on test data with the initial model (before using optimal lambda)
y_pred_initial <- predict(ridge_model, newx = X_test)

# Calculate Mean Squared Error (MSE) for the initial model
mse_initial <- mean((y_pred_initial - y_test)^2)
print(paste("Mean Squared Error (Initial Model):", mse_initial))

###
# Refit ridge regression model using the optimal lambda
ridge_model_optimal <- glmnet(X_train, y_train, alpha = 0, lambda = cv_model$lambda.min)

# Predict on test data with the optimal model
y_pred_optimal <- predict(ridge_model_optimal, newx = X_test)

# Calculate Mean Squared Error (MSE) for the optimal model
mse_optimal <- mean((y_pred_optimal - y_test)^2)
print(paste("Mean Squared Error (Optimal Model):", mse_optimal))

# Optionally, customize the plot for better visualization
plot(cv_model, main = "Cross-Validation Plot for Lambda Optimization",
     xlab = "log(lambda)", ylab = "Mean Squared Error (MSE)",
     xlim = c(-2, 6))  # Adjust xlim as needed for better visualization

# Optionally, customize the plot for better visualization
plot(ridge_model, xvar = "lambda", label = TRUE,
     main = "Regularization Path",
     xlab = "log(lambda)", ylab = "Coefficients",
     xlim = c(-2, 6))  # Adjust xlim as needed for better visualization
# Custom annotation for parameter labels
legend("bottomright", legend = colnames(X_train), col = 1:ncol(X_train), lty = 1)