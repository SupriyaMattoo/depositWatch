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
#' ds <- new_deposit_dataset(transactions, window_size = 3, z_thresh = 2, freq_thresh = 5)
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
