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


test_that("rows with missing values are removed with a warning", {
  # Create a sample data frame with a missing amount
  df_missing <- data.frame(
    customer_id = c("C1", "C2", "C3"),
    date = as.Date(c("2025-10-17", "2025-10-17", "2025-10-17")),
    amount = c(100, NA, 150)
  )
  
  # Expect a warning about removed rows
  expect_warning(
    obj <- new_deposit_dataset(df_missing),
    regexp = "row\\(s\\) with missing values were removed from the transactions dataset."
  )
  
  # Check that only 2 rows remain (the NA row is gone)
  expect_equal(nrow(obj$transactions), 2)
  
  # Check that the remaining customers are C1 and C3
  expect_equal(obj$transactions$customer_id, c("C1", "C3"))
})

test_that("negative deposit amounts are removed with a warning", {
  df_neg <- data.frame(
    customer_id = c("C1", "C2"),
    date = as.Date(c("2025-10-17", "2025-10-17")),
    amount = c(100, -50)
  )
  
  expect_warning(
    obj <- new_deposit_dataset(df_neg),
    regexp = "row\\(s\\) with negative deposit amounts were removed from the transactions dataset."
  )
  
  expect_equal(nrow(obj$transactions), 1)
  expect_equal(obj$transactions$customer_id, "C1")
})


test_that("new_deposit_dataset() fails when date is not of Date class", {
  df_notdate <- data.frame(
    customer_id = rep(c("C1", "C2"), each = 3),
    date = c("01/02/2008", "02/02/2008", "02/02/2008", "03/02/2008",
             "04/02/2008", "04/02/2008"),
    amount = c(100, 102, 104, 95, 97, 99)
  )
  expect_error(
    new_deposit_dataset(df_notdate, 3, 2),
    regexp = "The 'date' column must be of class Date. Use as.Date() to convert.",
    fixed = TRUE
  )
})


test_that("new_deposit_dataset() fails when amount is not numeric", {
  df_amountnotnumeric <- data.frame(
    customer_id = rep(c("C1", "C2"), each = 3),
    date = rep(seq.Date(Sys.Date() - 2, Sys.Date(), by = "day"), 2),
    amount = c("a", "b", "c", "d", "e", "f")
  )
  expect_error(
    new_deposit_dataset(df_amountnotnumeric, 3, 2),
    regexp = "The 'amount' column must be numeric.",
    fixed = TRUE
  )
})


test_that("new_deposit_dataset() fails with invalid window_size", {
  df_ok <- data.frame(
    customer_id = rep(c("C1", "C2"), each = 3),
    date = rep(seq.Date(Sys.Date() - 2, Sys.Date(), by = "day"), 2),
    amount = c(100, 102, 104, 95, 97, 99)
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = 0, z_thresh = 2),
    regexp = "window_size must be a positive integer",
    ignore.case = TRUE
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = -5, z_thresh = 2),
    regexp = "window_size must be a positive integer",
    ignore.case = TRUE
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = "three", z_thresh = 2),
    regexp = "window_size must be a positive integer",
    ignore.case = TRUE
  )
})

test_that("new_deposit_dataset() fails with invalid z_thresh", {
  df_ok <- data.frame(
    customer_id = rep(c("C1", "C2"), each = 3),
    date = rep(seq.Date(Sys.Date() - 2, Sys.Date(), by = "day"), 2),
    amount = c(100, 102, 104, 95, 97, 99)
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = 3, z_thresh = 0),
    regexp = "z_thresh must be a positive number",
    ignore.case = TRUE
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = 3, z_thresh = -2),
    regexp = "z_thresh must be a positive number",
    ignore.case = TRUE
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = 3, z_thresh = "two"),
    regexp = "z_thresh must be a positive number",
    ignore.case = TRUE
  )
})


test_that("new_deposit_dataset() fails with invalid freq_thresh", {
  df_ok <- data.frame(
    customer_id = rep(c("C1", "C2"), each = 3),
    date = rep(seq.Date(Sys.Date() - 2, Sys.Date(), by = "day"), 2),
    amount = c(100, 102, 104, 95, 97, 99)
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = 3, freq_thresh = 0),
    regexp = "freq_thresh must be a positive number",
    ignore.case = TRUE
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = 3, freq_thresh = -2),
    regexp = "freq_thresh must be a positive number",
    ignore.case = TRUE
  )

  expect_error(
    new_deposit_dataset(df_ok, window_size = 3, freq_thresh = "two"),
    regexp = "freq_thresh must be a positive number",
    ignore.case = TRUE
  )
})

test_that("new_deposit_dataset() creates a deposit_dataset object with valid input", {
  df_ok <- data.frame(
    customer_id = rep(c("C1", "C2"), each = 3),
    date = rep(seq.Date(Sys.Date() - 2, Sys.Date(), by = "day"), 2),
    amount = c(100, 102, 104, 95, 97, 99)
  )

  result <- new_deposit_dataset(df_ok, window_size = 3, z_thresh = 2)

  expect_s3_class(result, "deposit_dataset")
  expect_equal(result$window_size, 3)
  expect_equal(result$z_thresh, 2)
  expect_equal(nrow(result$transactions), 6)
})




