#' @title Read a SQLite DB into a data.frame
#'
#' @param fn Path to a SQLite file
#' @param view Name of the SQLite View to load. Defaults to "MfE_Carbon_data".
#'
#' @returns a data.frame
#'
#' @author Pierre Roudier
#'
#' @importFrom RSQLite SQLite dbConnect dbListTables  dbReadTable dbDisconnect
#' @export
read_mfe_sqlite <- function(fn, view = "MfE_Carbon_data") {

  # Initiate connection to SQLite
  con <- dbConnect(SQLite(), fn)

  # Check if requested View exists
  tbls <- dbListTables(con)

  if (! view %in% tbls) {
    stop("The requested view isn't available in the database", call. = FALSE)
  }

  # Read data View
  df <- dbReadTable(con, view)

  # Close connection
  dbDisconnect(con)

  # Returns data.frame
  return(df)
}

