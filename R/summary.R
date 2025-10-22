
#' Summarize anomalies in a deposit dataset
#'
#' Provides two levels of anomaly summaries from a \code{deposit_dataset} object:
#' (1) a monthly-level summary per customer, and (2) a daily-level table of flagged transactions.
#' By default, the function prints the top \code{n} rows from each summary to the console.
#' Optionally, only the monthly summary can be exported to Excel.
#'
#' @param obj An object of class \code{deposit_dataset}, typically returned by
#'   \code{\link{detect_anomalies}}.
#' @param n Integer. Number of rows to display from each summary table. Defaults to 10.
#' @param excel_path Optional character string specifying a directory path.
#'   If provided, only the monthly summary (\code{obj$results}) will be exported
#'   as an Excel file named \code{"results.xlsx"} in that directory.  
#' @param csv_path Optional character string specifying a directory path.
#'   If provided, both monthly summary (\code{obj$results}) and daily flagged data (\code{obj$flagged_data}) will be exported
#'   as csv files named \code{"results.csv"} and \code{"flagged_data.csv"} in that directory.
#'
#' @return Invisibly returns the summary data frame (\code{obj$results}).
#'   Prints the top \code{n} rows of both the monthly summary and the daily flagged data
#'   to the console. The daily-level flagged transactions remain available within R
#'   for further analysis but are not exported due to potential dataset size.
#'
#' @details
#' The summary method provides a concise overview of anomalies detected in the dataset:
#' \itemize{
#'   \item \strong{Monthly summary} — Aggregates anomalies by customer and month, suitable for management reporting or export.
#'   \item \strong{Daily flagged data} — Lists all individual transactions identified as anomalous; retained within R for deeper analysis.
#' }
#'
#' Note: If your window_size is, say, 30 days, then the first 29 days per customer won’t have enough data points to compute a rolling mean.
#' That’s why roll_mean_amt, roll_sd_amt, and z_score_amt are \code{NA} 
#'
#' When \code{excel_path} is supplied, only the monthly summary is exported
#' for convenience and scalability reasons.
#'
#' When \code{csv_path} is supplied, both the monthly summary and daily data is exported
#'
#' @examples
#' transactions <- data.frame(
#'   customer_id = rep(c("C1", "C2"), each = 5),
#'   date = rep(seq.Date(Sys.Date() - 4, Sys.Date(), by = "day"), 2),
#'   amount = c(100, 105, 102, 300, 250, 95, 98, 97, 99, 101)
#' )
#'
#' ds <- new_deposit_dataset(transactions, window_size = 3, z_thresh = 2, freq_thresh = 2)
#' ds <- detect_anomalies(ds)
#' summary(ds)  # Prints top 10 results from both summaries
#' # summary(ds, excel_path = "D:/Results")  # Exports only monthly summary
#' # summary(ds, csv_path = "D:/Results")  # Exports both the monthly summary and daily-level data
#' 
#' @seealso \code{\link{new_deposit_dataset}}, \code{\link{detect_anomalies}}
#'
#' @importFrom openxlsx write.xlsx
#' @export
#' @method summary deposit_dataset
summary.deposit_dataset <- function(obj, n=10, excel_path = NULL, csv_path = NULL) {
  if (is.null(obj$results)) stop("Run detect_anomalies() first.")

  print(utils::head(obj$results, n))
  
  print(utils::head(obj$flagged_data, n))

  # Export to Excel if path is provided
  if (!is.null(excel_path)) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' required for Excel export. Please install it first.")
    }
    results_file <- file.path(excel_path, "results.xlsx")
    flagged_file <- file.path(excel_path, "flagged_data.xlsx")
    
    openxlsx::write.xlsx(obj$results, results_file, overwrite = TRUE)
    #openxlsx::write.xlsx(obj$flagged_data, flagged_file, overwrite = TRUE)
    message("Results exported to Excel: ", excel_path)
  }
  
  # ---- Export to CSV if path is provided ----
  if (!is.null(csv_path)) {
    results_csv <- file.path(csv_path, "results.csv")
    flagged_csv <- file.path(csv_path, "flagged_data.csv")
    
    write.csv(obj$results, results_csv, row.names = FALSE)
    write.csv(obj$flagged_data, flagged_csv, row.names = FALSE)
    message("Results exported to CSV: ", csv_path)
  }
}

