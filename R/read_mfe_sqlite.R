#' @title Read a SQLite DB into a data.frame
#'
#' @param fn Path to a SQLite file
#'
#' @returns a data.frame
#'
#' @author Pierre Roudier
#'
#' @importFrom RSQLite SQLite dbConnect dbReadTable dbDisconnect
#'
read_mfe_sqlite <- function(fn) {

  # Initiate connection to SQLite
  con <- dbConnect(SQLite(), fn)

  # Read data View
  df <- dbReadTable(con, "MfE_Carbon_data")

  # Close connection
  dbDisconnect(con)

  # Returns data.frame
  return(df)
}

