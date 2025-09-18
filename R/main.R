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

<<<<<<< HEAD
#transactions <- read.csv("D:/EMBA/DATA501/Package/Deposits.csv", stringsAsFactors = FALSE)

#head(transactions)

#ds <- new_deposit_dataset(transactions)

#otype(ds)


# Detect anomalies method
#' Detect anomalies in a deposit dataset
#'
#' Computes rolling Z-scores and flags anomalous deposits for each customer.
#' Requires a `deposit_dataset` object created by `new_deposit_dataset()`.
#'
#' @param obj An object of class \code{deposit_dataset}.
#'
#' @return The same \code{deposit_dataset} object with a \code{results}
#'   data frame containing the number of anomalies per customer.
#' @export
#' @method detect_anomalies deposit_dataset
#'
#' @examples
#' # Create a deposit dataset
#' transactions <- data.frame(
#'   customer_id = rep(c("C1", "C2"), each = 5),
#'   date = rep(seq.Date(Sys.Date() - 4, Sys.Date(), by = "day"), 2),
#'   amount = c(100, 105, 102, 300, 250, 95, 98, 97, 99, 101)
#' )
#'
#' ds <- new_deposit_dataset(transactions, window_size = 3, z_thresh = 2)
#'
#' # Detect anomalies (returns a new object)
#' ds1 <- detect_anomalies(ds)
#'
#' # Print anomaly summary
#' summary(ds1)

detect_anomalies.deposit_dataset <- function(obj) {

  results <- obj$transactions %>%
    dplyr::group_by(customer_id) %>%
    dplyr::arrange(date) %>%
    dplyr::group_modify(~ {
      stats <- rollingStats(.x$amount, obj$window_size)
      .x$roll_mean <- stats$roll_mean
      .x$roll_sd   <- stats$roll_sd
      .x$z_score   <- (.x$amount - .x$roll_mean) / .x$roll_sd
      .x$flag      <- abs(.x$z_score) > obj$z_thresh
      .x
    }) %>%
    dplyr::summarise(anomaly_count = sum(flag, na.rm = TRUE), .groups = "drop")

  # Create a new object with results
  new_obj <- obj
  new_obj$results <- results

  # Return the modified object
  return(new_obj)
}
#detect_anomalies.deposit_dataset(ds)

#' Summarize a deposit dataset
#'
#' @param object An object of class \code{deposit_dataset}.
#' @param ... Additional arguments (ignored).
#'
#' @return Prints a summary table of anomalies per customer.
#' @export
#' @method summary deposit_dataset
#' @export
summary.deposit_dataset <- function(obj) {
  if (is.null(obj$results)) stop("Run detect_anomalies() first.")
  print(obj$results)
}
=======
>>>>>>> plots









