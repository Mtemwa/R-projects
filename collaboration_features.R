# collaboration_features.R

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(pool)

# Assuming a database connection pool has been established in data_management.R
# Source the data management script to use the db connection pool
source("data_management.R")

# Function to log user activity into the database
log_activity <- function(user_id, activity_description) {
  timestamp <- Sys.time()
  activity_data <- data.frame(user_id = user_id, activity_description = activity_description, timestamp = timestamp)
  write_data(activity_data, "user_activity_log")
}

# Function to display recent activities to all users
show_recent_activities <- function() {
  query <- "SELECT * FROM user_activity_log ORDER BY timestamp DESC LIMIT 10"
  recent_activities <- fetch_data(query)
  return(recent_activities)
}

# Function to enable users to share dashboards
share_dashboard <- function(dashboard_id, user_id) {
  # Placeholder for logic to share dashboard configurations
  # This could involve writing to a 'dashboard_shares' table with dashboard_id and user_id
  print(sprintf("Dashboard %s shared with user %s", dashboard_id, user_id))
}

# Function to annotate shared dashboards or reports
add_annotation <- function(dashboard_id, user_id, annotation_text) {
  timestamp <- Sys.time()
  annotation_data <- data.frame(dashboard_id = dashboard_id, user_id = user_id, annotation_text = annotation_text, timestamp = timestamp)
  write_data(annotation_data, "dashboard_annotations")
}

# UI component for displaying recent activities
ui_recent_activities <- DT::dataTableOutput("recent_activities")

# Server logic to render recent activities
server_recent_activities <- function(input, output, session) {
  output$recent_activities <- DT::renderDataTable({
    DT::datatable(show_recent_activities(), options = list(pageLength = 5, autoWidth = TRUE))
  })
}

# Example usage in the app
# Call `server_recent_activities()` inside the server function of your Shiny app
# Add `ui_recent_activities` where you want to display the recent activities table in the UI

