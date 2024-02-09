library(data.table)
data_customer <- fread("data_customer.csv")
data_personal <- fread("data_personal.csv")

#merge the two tables by CustomerId
data_merged <- merge(data_customer, data_personal, by = "CustomerId")

test_that("Churn probability of customer 15653251 is higher than 15662641", {
  expect_gt(get_churn_probability(data_merged, 15653251),
            get_churn_probability(data_merged, 15662641))
})
