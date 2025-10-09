#' @noRd
.unique <- function(x) {
  idx_exclude <- which(is.na(x) | x == "")

  if (length(idx_exclude) > 0) x <- x[-1*idx_exclude]

  res <- unique(x)

  if (length(res) == 0) res <- NA

  # If there is still more than one element with collapse it with a comma
  if (length(res) > 1) {
    res <- paste0(res, collapse = ", ")
  }

  return(res)
}

# Custom weighted mean function that weights mean based
# on the cross tabulation of values to average
#' @noRd
.weighted_mean_count <- function(x) {
  # Remove any NAs
  x <- as.numeric(na.omit(x))
  tb <- table(x)
  lv <- as.numeric(names(tb))

  weighted.mean(x = lv, w = tb)
}

#' @noRd
.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' @noRd
.add_cols <- function(df, cols, value = NA) {

  add <- cols[!cols %in% names(df)]

  if(length(add) > 0) df[add] <- value

  return(df)
}
