#' @title Orchestrates the export of MfE data from a SQLite to a Excel spreadsheet
#'
#' @param sqlite_fn Path to a SQLite file, typically downloaded from the NSDR Viewer app
#' @param xlsx_fn A file path to save the xlsx file
#' @param process_data Does the data needs post-processing (splining, adding NZSC, etc)? Currently not implemented and set to FALSE.
#'
#' @returns Nothing, but writes Excel spreadsheet to disk
#'
#' @author Pierre Roudier
#'
#' @include read_mfe_sqlite.R write_mfe_xlsx.R
#' @export
#'
export_mfe <- function(sqlite_fn, xlsx_fn, process_data = FALSE) {
  # Read data
  df <- read_mfe_sqlite(fn = sqlite_fn)

  # Export data to Excel
  write_mfe_xlsx(df = df, fn = xlsx_fn)

  # No need to retunr anything since the function is called for its side effect
  return(invisible())
}
