#' @title Writes data.frame to Excel file
#'
#' @param df a data.frame of MfE soil carbon data
#' @param fn A file path to save the xlsx file
#'
#' @returns Nothing, but writes Excel spreadsheet to disk
#'
#' @author Pierre Roudier
#'
#' @importFrom openxlsx write.xlsx
#' @export
#'
write_mfe_xlsx <- function(df, fn) {
  write.xlsx(
    df = df,
    fn = fn,
    sheetname = "SoilCarbon",
    overwrite = TRUE
  )
}
