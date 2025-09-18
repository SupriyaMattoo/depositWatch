

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

