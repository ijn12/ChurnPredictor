get_churn_probability <- function(dataset, customer_id) {
  # Check if customer ID exists in the dataset
  if (!customer_id %in% dataset$CustomerId) {
    stop("Error: Customer ID not found in the dataset.")
  }

  # Convert necessary columns to factors
  dataset$Exited <- as.factor(dataset$Exited)
  dataset$Gender <- as.factor(dataset$Gender)

  # Build the logistic regression model
  model <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance + NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
               data = dataset,
               family = "binomial")

  # Extracting the row for the specified customer ID
  customer_data <- dataset[dataset$CustomerId == customer_id, ]

  # Predict churn probability using the model
  churn_prob <- predict(model, newdata = customer_data, type = "response")

  return(churn_prob)
}
