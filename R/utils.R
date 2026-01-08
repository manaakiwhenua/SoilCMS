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

# Function to clean and harmonise the "duration" fields
# obtained from the NSDR
#' @importFrom stringr str_remove_all str_detect str_extract_all str_extract
#' @noRd
.fixDuration <- function(s) {

  # s must be a vector of character
  if (!is.vector(s)) stop("s must be a character vector")

  # Store the original values
  s_raw <- s

  # Remove any white space
  s <- str_remove_all(s, " ")

  # If there is a decimal place in the code,
  # we remove it and everything after it
  s <- case_when(
    str_detect(s, "\\.") & str_detect(s, "[0-9]") ~ sapply(str_extract_all(str_extract(s, "^[^\\\\.]+"), "[0-9]"), paste0, collapse = ""),
    TRUE ~ s
  )

  # If there is a decimal place in the code,
  # we remove it and everything after it
  s <- case_when(
    str_detect(s, "\\.") & str_detect(s, "[0-9]") ~ sapply(str_extract_all(str_extract(s, "^[^\\\\.]+"), "[0-9]"), paste0, collapse = ""),
    TRUE ~ s
  )

  # If there is a comma, this is interpreted as a repeated value and we take the first number
  s <- case_when(
    str_detect(s, ",") ~ str_extract(s, "^[^\\\\,]+"),
    TRUE ~ s
  )

  # If there is an hyphen, this is interpreted as an interval and we take the first number
  s <- case_when(
    str_detect(s, "-") ~ str_extract(s, "^[^\\\\-]+"),
    TRUE ~ s
  )

  # If there seem to be digits in the code
  s <- case_when(
    str_detect(s, "[0-9]") ~ sapply(str_extract_all(s, "[0-9]"), paste0, collapse = ""),
    TRUE ~ s
  )

  # Cases standing for NA
  s <- case_when(
    s == ";" | s == "" ~ NA,
    TRUE ~ s
  )

  # Convert to numeric
  s <- as.numeric(s)

  # Implementing the minus/plus signs
  s = case_when(

    # Cases where we add one year
    str_detect(s_raw, "\\+") ~ s + 1,
    str_detect(s_raw, "\\*") ~ s + 1,
    str_detect(s_raw, ">") ~ s + 1,
    str_detect(s_raw, "\\.") ~ s + 1,

    # Cases where we remove one year
    # str_detect(s_raw, "\\-") ~ s - 1,
    str_detect(s_raw, "<") ~ s - 1,

    TRUE ~ s
  )

  return(s)
}
