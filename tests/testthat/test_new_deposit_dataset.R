test_that("new_deposit_dataset() fails when input is not a data.frame", {
  not_df <- list(
    customer_id = "C1",
    date = Sys.Date(),
    amount = 100
  )

  expect_error(
    new_deposit_dataset(not_df, window_size = 3, z_thresh = 2),
    regexp = "transactions must be a data frame",
    fixed = TRUE
  )
})

test_that("new_deposit_dataset() fails when required columns are missing", {
  df_missing_customer <- data.frame(
    date = Sys.Date(),
    amount = 100
  )

  df_missing_date <- data.frame(
    customer_id = "C1",
    amount = 100
  )

  df_missing_amount <- data.frame(
    customer_id = "C1",
    date = Sys.Date()
  )

  expect_error(
    new_deposit_dataset(df_missing_customer, 3, 2),
    regexp = "Missing required column(s):",
    fixed = TRUE,
    ignore.case = TRUE
  )

  expect_error(
    new_deposit_dataset(df_missing_date, 3, 2),
    regexp = "Missing required column(s):",
    fixed = TRUE,
    ignore.case = TRUE
  )

  expect_error(
    new_deposit_dataset(df_missing_amount, 3, 2),
    regexp = "Missing required column(s):",
    fixed = TRUE,
    ignore.case = TRUE
  )
})




