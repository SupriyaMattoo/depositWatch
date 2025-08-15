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
#' transactions <- data.frame(
#'   customer_id = rep(c("C1", "C2"), each = 5),
#'   date = rep(seq.Date(Sys.Date() - 4, Sys.Date(), by = "day"), 2),
#'   amount = c(100, 105, 102, 300, 250, 95, 98, 97, 99, 101)
#' )
#'
#' ds <- new_deposit_dataset(transactions, window_size = 3, z_thresh = 2)
#' ds <- detect_anomalies(ds)
#' summary(ds)
detect_anomalies.deposit_dataset <- function(obj) {

  # Aggregate daily totals and counts per customer-date
  daily_data <- obj$transactions %>%
    dplyr::group_by(customer_id, date) %>%
    dplyr::summarise(
      daily_total = sum(amount, na.rm = TRUE),
      transaction_count = dplyr::n(),
      .groups = "drop_last"
    ) %>%
    dplyr::arrange(customer_id, date)

  # Apply rolling stats per customer
  flagged_data <- daily_data %>%
    dplyr::group_by(customer_id) %>%
    dplyr::group_modify(~ {
      stats <- rollingStats(.x$daily_total, obj$window_size)
      .x$roll_mean <- stats$roll_mean
      .x$roll_sd   <- stats$roll_sd
      .x$z_score   <- (.x$daily_total - .x$roll_mean) / .x$roll_sd

      # Flag 1: More than usual
      flag_amount <- abs(.x$z_score) > obj$z_thresh

      # Flag 2: High frequency but not amount flag
      flag_freq <- (!flag_amount) & (.x$transaction_count > obj$freq_thresh)

      # Combine flags
      .x$flag <- dplyr::case_when(
        flag_amount & flag_freq ~ "Both",
        flag_amount ~ "More than usual",
        flag_freq ~ "High frequency",
        TRUE ~ NA_character_
      )

      return(.x)  # <- Important! return the modified data frame
    }) %>%
    dplyr::ungroup()

  # Summarise per customer: how many times flagged and which flags occurred
  results <- flagged_data %>%
    dplyr::group_by(customer_id) %>%
    dplyr::summarise(
      flag_count = sum(!is.na(flag)),
      final_flag = paste(unique(stats::na.omit(flag)), collapse = ", "),
      .groups = "drop"
    )

  # Store results in new object
  new_obj <- obj
  new_obj$results <- results

  return(new_obj)
}

#detect_anomalies.deposit_dataset(ds)

#' Summarize a deposit dataset
#' @importFrom openxlsx write.xlsx
#' @param object An object of class \code{deposit_dataset}.
#' @param ... Additional arguments (ignored).
#'
#' @return Prints and exports a summary table of anomalies per customer.
#' @export
#' @method summary deposit_dataset
#' @export
summary.deposit_dataset <- function(obj, excel_path = NULL) {
  if (is.null(obj$results)) stop("Run detect_anomalies() first.")

  print(obj$results)

  # Export to Excel if path is provided
  if (!is.null(excel_path)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' required for Excel export. Please install it first.")
    }
    openxlsx::write.xlsx(obj$results, file = excel_path)
    message("Results exported to Excel: ", excel_path)
  }
}



#' Detect anomalies generic
#'
#' Generic function for detecting anomalies in different objects.
#'
#' @param obj Object to detect anomalies on
#' @param ... Additional arguments
#' @export
detect_anomalies <- function(obj, ...) {
  UseMethod("detect_anomalies")
}
