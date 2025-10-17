#' Detect anomalies in a deposit dataset
#'
#' Applies rolling statistical methods to identify anomalous deposit behavior
#' per customer. Anomalies are flagged based on unusually large deviations
#' in transaction amounts or unusually high transaction frequencies, relative
#' to rolling averages. Requires a \code{deposit_dataset} object created
#' by \code{\link{new_deposit_dataset}}.
#'
#' @param obj An object of class \code{deposit_dataset}, containing
#'   transaction data and anomaly detection parameters.
#'
#' @return A \code{deposit_dataset} object with an added \code{results}
#'   data frame summarising anomalies per customer per month, including:
#'   \itemize{
#'     \item \code{flag_count}: Number of anomaly flags raised.
#'     \item \code{final_flag}: Type(s) of anomaly detected
#'       ("More than usual", "High frequency", or "Both").
#'   }
#'
#' @details
#' The function computes rolling means and standard deviations for both
#' transaction amounts and frequencies. Z-scores are then calculated and
#' compared against the thresholds specified in the \code{deposit_dataset}
#' object (\code{z_thresh} and \code{freq_thresh}).
#'
#' @examples
#' transactions <- data.frame(
#'   customer_id = rep(c("C1", "C2"), each = 5),
#'   date = rep(seq.Date(Sys.Date() - 4, Sys.Date(), by = "day"), 2),
#'   amount = c(100, 105, 102, 300, 250, 95, 98, 97, 99, 101)
#' )
#'
#' ds <- new_deposit_dataset(transactions, window_size = 3, z_thresh = 2, freq_thresh = 5)
#' ds <- detect_anomalies(ds)
#' ds$results
#'
#' @seealso \code{\link{new_deposit_dataset}}, \code{\link{plot.deposit_dataset}}
#'
#' @export
#' @method detect_anomalies deposit_dataset
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

      # Rolling stats for amounts
      stats_amt <- rollingStats(.x$daily_total, obj$window_size)
      .x$roll_mean_amt <- stats_amt$roll_mean
      .x$roll_sd_amt   <- stats_amt$roll_sd
      .x$z_score_amt   <- ifelse(.x$roll_sd_amt > 0,
                                 (.x$daily_total - .x$roll_mean_amt) / .x$roll_sd_amt,0)

      # Rolling stats for transaction frequency
      stats_freq <- rollingStats(.x$transaction_count, obj$window_size)
      .x$roll_mean_freq <- stats_freq$roll_mean
      .x$roll_sd_freq   <- stats_freq$roll_sd
      .x$z_score_freq   <- ifelse(.x$roll_sd_freq > 0,
                                  (.x$transaction_count - .x$roll_mean_freq) / .x$roll_sd_freq,0)

      # Flag 1: Amount anomaly
      flag_amount <- abs(.x$z_score_amt) > obj$z_thresh

      # Flag 2: Frequency anomaly
      flag_freq <- abs(.x$z_score_freq) > obj$freq_thresh

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

  # Summarise per customer per month: how many times flagged and which flags occurred
  results <- flagged_data %>%
    dplyr::mutate(month = lubridate::floor_date(date, "month")) %>%  # create month column
    dplyr::group_by(customer_id, month) %>%
    dplyr::summarise(
      flag_count = sum(!is.na(flag)),
      final_flag = paste(unique(na.omit(flag)), collapse = ", "),
      .groups = "drop"
    )

  # Store results in new object
  new_obj <- obj
  new_obj$results <- dplyr::filter(results, flag_count > 0) # keep only rows where there was at least one flag
  new_obj$flagged_data <- flagged_data
  
  return(new_obj)
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
