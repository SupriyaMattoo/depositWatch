
# Function to plot results for customer who showed unusual behavior in their deposit accounts.

#' Plot monthly anomalies for a \code{deposit_dataset} object
#'
#' This method visualizes anomalies detected in a \code{deposit_dataset} object.
#' It generates two plots:
#' \enumerate{
#'   \item A stacked bar chart of total anomalies per month, broken down by flag type.
#'   \item A line chart showing the number of unique customers with anomalies each month.
#' }
#'
#' These plots help users quickly assess the volume and distribution of anomalies
#' over time, and identify whether anomalies are concentrated in specific months
#' or spread evenly across the dataset.
#'
#' @param obj An object of class \code{deposit_dataset} produced by
#'   \code{detect_anomalies()}, containing a \code{results} slot with anomaly
#'   flags. Must not be \code{NULL}.
#'
#' @return A named list with two \link[ggplot2]{ggplot} objects:
#' \describe{
#'   \item{\code{total_anomalies}}{A stacked column plot of monthly anomalies,
#'   grouped by flag type.}
#'   \item{\code{unique_customers}}{A line plot of the number of unique customers
#'   with anomalies per month.}
#' }
#'
#' @details
#' Internally, the function:
#' \itemize{
#'   \item Filters the anomaly results to rows with at least one flag.
#'   \item Aggregates anomaly counts by month and by flag type.
#'   \item Creates two ggplot visualizations:
#'     \enumerate{
#'       \item Stacked bar chart of anomalies by flag type.
#'       \item Line chart of unique customers with anomalies.
#'     }
#' }
#'
#' The function is primarily intended for exploratory analysis and reporting.
#'
#' @examples
#' \dontrun{
#' # Assuming `ds` is a deposit_dataset with detect_anomalies() already run:
#' plots <- plot(ds)
#' plots$total_anomalies     # view anomalies by flag type
#' plots$unique_customers    # view unique customers with anomalies
#' }
#'
#' @export
#' @import ggplot2
#' @importFrom lubridate floor_date
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise n_distinct
#' @importFrom tidyr separate_rows
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
