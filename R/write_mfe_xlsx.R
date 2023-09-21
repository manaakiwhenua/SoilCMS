#' Returns the longer header description if available
#' @noRd
.get_long_header <- function(x) {
  if (x %in% names_config$short_heading) {
    res <- names_config$long_heading[which(names_config$short_heading == x)]

    res <- unique(res)

  } else {
    # If the short header is not in the collection we return
    # an empty string
    res <- ""
  }

  return(res)
}

#' @title Writes data.frame to Excel file
#'
#' @param df a data.frame of MfE soil carbon data
#' @param fn A file path to save the xlsx file
#'
#' @returns Nothing, but writes Excel spreadsheet to disk
#'
#' @author Pierre Roudier
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook addStyle createStyle setColWidths
#' @export
write_mfe_xlsx <- function(df, fn) {

  # write.xlsx(
  #   x = df,
  #   file = fn,
  #   sheetname = "SoilCarbon",
  #   overwrite = TRUE
  # )

  # Create workbook
  wb <- createWorkbook()

  # Add worksheet
  addWorksheet(wb, 'SoilCarbon')

  # Create header
  short_headers <- names(df)
  long_headers <- do.call(c, lapply(short_headers, .get_long_header))
  header <- rbind(short_headers, long_headers)

  # Header style
  hs1 <- createStyle(
    fgFill = "#4F81BD",
    halign = "CENTER",
    textDecoration = "Bold",
    border = "Bottom",
    fontColour = "white"
  )

  # Write header to the new sheet
  writeData(
    wb = wb,
    sheet = 'SoilCarbon',
    header,
    colNames = FALSE
  )

  # Style for header lines
  addStyle(
    wb = wb,
    sheet = "SoilCarbon",
    hs1,
    rows = 1:2,
    cols = 1:ncol(df),
    gridExpand = TRUE
  )

  # Write data to the new sheet
  writeData(
    wb = wb,
    sheet = 'SoilCarbon',
    df,
    startRow = 3,
    colNames = FALSE
  )

  # Set column widths
  setColWidths(
    wb = wb,
    sheet = 'SoilCarbon',
    cols = 1:ncol(df),
    widths = "auto"
  )

  # Saving the workbook
  saveWorkbook(wb = wb, file = fn, overwrite = TRUE)
}
