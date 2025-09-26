

#' Summarize a deposit dataset
#'
#' Provides a tabular summary of anomalies detected in a \code{deposit_dataset}
#' object. By default, the summary is printed to the console. Optionally,
#' results can be exported to an Excel file.
#'
#' @param obj An object of class \code{deposit_dataset}, typically returned by
#'   \code{\link{detect_anomalies}}.
#' @param excel_path Optional file path (character). If provided, the summary
#'   table will be exported as an Excel file at the specified location.
#' @param ... Additional arguments (ignored for this method).
#'
#' @return Invisibly returns the summary results data frame. Prints the results
#'   to the console, and optionally writes them to Excel if \code{excel_path}
#'   is supplied.
#'
#' @details
#' This method is intended to give a quick overview of anomalies per customer
#' and per month. The exported Excel file (if requested) can be used for
#' further manual inspection or sharing.
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
#' summary(ds)                        # prints results to console
#' # summary(ds, excel_path = "anomalies.xlsx")  # exports to Excel
#'
#' @seealso \code{\link{new_deposit_dataset}}, \code{\link{detect_anomalies}}
#'
#' @importFrom openxlsx write.xlsx
#' @export
#' @method summary deposit_dataset
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

