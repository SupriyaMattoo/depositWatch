#' Plot monthly anomalies for a deposit_dataset object
#'
#' @param obj An object of class \code{deposit_dataset} with anomalies detected.
#' @export
#' @import ggplot2
#' @importFrom lubridate floor_date
#' @method plot deposit_dataset
plot.deposit_dataset <- function(obj) {
  if (is.null(obj$results)) stop("Run detect_anomalies() first.")

  # Keep only rows where flags exist
  flagged_data <- obj$results %>% dplyr::filter(flag_count > 0)

  # Summarize per month across all customers
  results_monthly <- flagged_data %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      anomalies_count = sum(flag_count),
      unique_customers = dplyr::n_distinct(customer_id),
      .groups = "drop"
    )

  # Summarize per month and flag type
  results_monthly_flag <- flagged_data %>%
    tidyr::separate_rows(final_flag, sep = ", ") %>% # split multiple flags
    dplyr::group_by(month, final_flag) %>%
    dplyr::summarise(total_anomalies = sum(flag_count), .groups = "drop")

  # Plot: total anomalies per month by flag type
  p1 <- ggplot(results_monthly_flag, aes(x = month, y = total_anomalies, fill = final_flag)) +
    geom_col() +
    labs(title = "Total Monthly Anomalies by Flag Type",
         x = "Month", y = "Total Anomalies", fill = "Flag Type") +
    theme_minimal()

  # Plot: unique customers with anomalies per month
  p2 <- ggplot(results_monthly, aes(x = month, y = unique_customers)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    labs(title = "Unique Customers with Anomalies per Month",
         x = "Month", y = "Number of Customers") +
    theme_minimal()

  list(total_anomalies = p1, unique_customers = p2)
}

#' Plot anomalies for a deposit dataset
#'
#' @param obj A \code{deposit_dataset} object with anomaly results
#' @return A ggplot2 object (or list of ggplot2 objects)
#' @export
plot <- function(obj) {
  UseMethod("plot")
}
