#' @importFrom magrittr %>%
NULL


#' Create a new deposit dataset object
#'
#' This function constructs an object of class \code{deposit_dataset}, which stores
#' transaction data and parameters for anomaly detection. The object can be passed
#' to other functions in the package (e.g., \code{detect_anomalies}) to run analyses.
#'
#' @param transactions A data frame of transaction records, with required columns:
#'   \code{customer_id}, \code{date}, and \code{amount}.
#' @param window_size Integer. Rolling window size for calculating moving statistics
#'   (default = 30).
#' @param z_thresh Numeric. Z-score threshold for identifying anomalous transaction
#'   amounts (default = 3).
#' @param freq_thresh Numeric. Z-score threshold for identifying anomalous transaction
#'   frequencies (default = 3).
#'
#' @return An object of class \code{deposit_dataset}, containing:
#' \itemize{
#'   \item \code{transactions}: The input data frame.
#'   \item \code{window_size}: Rolling window size.
#'   \item \code{z_thresh}: Amount anomaly threshold.
#'   \item \code{freq_thresh}: Frequency anomaly threshold.
#'   \item \code{results}: Placeholder for results (initially \code{NULL}).
#' }
#'
#' @examples
#' transactions <- data.frame(
#'   customer_id = rep(c("C1", "C2"), each = 5),
#'   date = rep(seq.Date(Sys.Date() - 4, Sys.Date(), by = "day"), 2),
#'   amount = c(100, 105, 102, 300, 250, 95, 98, 97, 99, 101)
#' )
#' ds <- new_deposit_dataset(transactions, window_size = 3, z_thresh = 2, freq_thresh = 2)
#'
#' @export
new_deposit_dataset <- function(transactions,
                                window_size = 30,
                                z_thresh = 3,
                                freq_thresh = 3) {
  # Check data is a data frame
  if (!is.data.frame(transactions)) {
    stop("transactions must be a data frame", call. = FALSE)
  }

  # Required columns
  required_cols <- c("customer_id", "date", "amount")
  missing_cols <- setdiff(required_cols, names(transactions))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }


  # Check that date column is Date class
  if (!inherits(transactions$date, "Date")) {
    stop("The 'date' column must be of class Date. Use as.Date() to convert.", call. = FALSE)
  }

  # Check that amount column is numeric
  if (!is.numeric(transactions$amount)) {
    stop("The 'amount' column must be numeric.", call. = FALSE)
  }

  # Check window_size
  if (!is.numeric(window_size) || length(window_size) != 1 || window_size <= 0) {
    stop("window_size must be a positive integer", call. = FALSE)
  }

  # Check z_thresh
  if (!is.numeric(z_thresh) || length(z_thresh) != 1 || z_thresh <= 0) {
    stop("z_thresh must be a positive number", call. = FALSE)
  }

  # Check freq_thresh
  if (!is.numeric(freq_thresh) || length(freq_thresh) != 1 || freq_thresh <= 0) {
    stop("freq_thresh must be a positive number", call. = FALSE)
  }

  # Create and return object
  structure(
    list(
      transactions = transactions,
      window_size = window_size,
      z_thresh = z_thresh,
      freq_thresh = freq_thresh,
      results = NULL
    ),
    class = "deposit_dataset"
  )
}












