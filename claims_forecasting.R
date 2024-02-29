# claims_forecasting.R

# Load necessary libraries
library(forecast)
library(caret)
library(xgboost)
library(dplyr)
library(lubridate)

# Load data management and predictive analytics functions
source("data_management.R")
source("predictive_analytics.R")

# Function to forecast claims using machine learning models
forecast_claims <- function(data, product_type, forecast_horizon) {
  # Ensure data is clean and of high quality
  data <- clean_data(data)
  data <- ensure_data_quality(data)
  
  # Filter data for the specific product type
  product_data <- data %>% filter(product == product_type)
  
  # Define the outcome variable and predictors based on product type
  if (product_type == "auto") {
    outcome <- "number_of_claims"
    predictors <- c("age", "gender", "driving_history", "vehicle_type")
  } else if (product_type == "home") {
    outcome <- "number_of_claims"
    predictors <- c("property_value", "location_risk", "security_features", "claim_history")
  } else {
    stop("Unsupported product type for claims forecasting")
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
  
  # Train a machine learning model - example with XGBoost
  xgb_data <- xgb.DMatrix(data = as.matrix(trainData[predictors]), label = trainData[[outcome]])
  xgb_model <- xgboost(data = xgb_data, nrounds = 100, objective = "reg:squarederror")
  
  # Forecast future claims
  future_data <- testData[predictors]
  future_claims <- predict(xgb_model, as.matrix(future_data))
  
  # Return the forecasted claims
  return(future_claims)
}

# Function to calculate reserves based on forecasted claims
calculate_reserves <- function(forecasted_claims, reserve_multiplier) {
  # Simple calculation of reserves based on a multiplier of forecasted claims
  # This is a simplified example. In practice, the calculation could be more complex.
  reserves <- sum(forecasted_claims) * reserve_multiplier
  
  return(reserves)
}

# Example usage
# Assuming 'insurance_data' is your dataset and 'reserve_multiplier' is defined
# forecasted_claims <- forecast_claims(insurance_data, "auto", 365)
# reserves <- calculate_reserves(forecasted_claims, 1.2)
# print(reserves)

