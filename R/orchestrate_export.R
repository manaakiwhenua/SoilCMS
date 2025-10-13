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
#' @include read_mfe_sqlite.R process_mfe_data.R write_mfe_xlsx.R
#' @export
#'
cms_export <- function(sqlite_fn, xlsx_fn, process_data = FALSE) {

  # Read data
  df <- cms_read(fn = sqlite_fn)

  # Process data
  if (process_data) {
    df <- cms_process(df)
  }

  # Export data to Excel
  cms_write(df = df, fn = xlsx_fn)

  # No need to retunr anything since the function is called for its side effect
  return(invisible())
}
