# risk_models.R

# Load necessary libraries
library(caret)
library(randomForest)
library(gbm)
library(rpart)
library(riskmodels) # Hypothetical package for risk modeling in insurance

# Load data management functions from data_management.R
source("data_management.R")

# Function to build risk models for different insurance products
build_risk_models <- function(data, product_type) {
  # Filter data for the specific product type
  product_data <- data %>% filter(product == product_type)
  
  # Define the outcome variable and predictors based on product type
  # This is a simplified example. In practice, the model specification would be more complex and product-specific.
  if (product_type == "auto") {
    outcome <- "claims_cost"
    predictors <- c("age", "gender", "driving_history", "vehicle_type")
  } else if (product_type == "home") {
    outcome <- "claims_cost"
    predictors <- c("property_value", "location_risk", "security_features", "claim_history")
  } else {
    stop("Unsupported product type")
  }
  
  # Prepare the data for modeling
  model_data <- product_data %>%
    select(all_of(c(outcome, predictors))) %>%
    na.omit() # Removing missing values for simplicity
  
  # Split data into training and testing sets
  set.seed(123) # For reproducibility
  trainIndex <- createDataPartition(model_data[[outcome]], p = .8, 
                                    list = FALSE, 
                                    times = 1)
  trainData <- model_data[ trainIndex,]
  testData  <- model_data[-trainIndex,]
  
  # Train a predictive model - example with random forest
  # In practice, you would explore multiple models and select the best performing one
  rf_model <- randomForest(as.formula(paste(outcome, "~", paste(predictors, collapse = "+"))), 
                           data = trainData)
  
  # Evaluate model performance on test data
  predictions <- predict(rf_model, newdata = testData)
  performance <- postResample(predictions, testData[[outcome]])
  print(paste("Model performance for", product_type, ":"))
  print(performance)
  
  # Return the trained model
  return(rf_model)
}

# Example usage
# Assuming you have a function to fetch cleaned and quality-assured data for modeling
modeling_data <- fetch_data("SELECT * FROM cleaned_insurance_data")
auto_model <- build_risk_models(modeling_data, "auto")
home_model <- build_risk_models(modeling_data, "home")

# Models can now be used for risk assessment and prediction for new data
