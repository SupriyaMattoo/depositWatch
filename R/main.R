#' @importFrom magrittr %>%
NULL


#' Create a deposit dataset object
#'
#' @param transactions A data frame of transaction records.
#' @param window_size Rolling window size (default 30).
#' @param z_thresh Z-score threshold for anomaly detection (default 3).
#' @param freq_thresh Frequency threshold for anomaly detection (default 5).
#'
#' @return An object of class \code{deposit_dataset}.
#' @export
new_deposit_dataset <- function(transactions,
                                window_size = 30,
                                z_thresh = 3,
                                freq_thresh = 5) {
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

  # Check window_size
  if (!is.numeric(window_size) || length(window_size) != 1 || window_size <= 0) {
    stop("window_size must be a positive integer", call. = FALSE)
  }

  # Check z_thresh
  if (!is.numeric(z_thresh) || length(z_thresh) != 1 || z_thresh <= 0) {
    stop("z_thresh must be a positive number", call. = FALSE)
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










