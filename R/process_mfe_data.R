#' Calculate Carbon and Nitrogen Stocks
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#' @returns a data.frame with two additional columns for SOC and total N stocks
#'
calculate_stocks <- function(df) {
  #
  # CALCULATED FIELDS
  #

  # Total oven dry sample weight
  df$amt_coarse_airdry_g = df$amt_coarse_airdry_gv # Typo in the VIEW
  df$amt_total_oven_dry_sample_g = df$amt_coarse_airdry_g +
    (df$amt_sample_airdry_g - df$amt_coarse_airdry_g) / (1 + df$amt_airdry_water_content_p / 100)

  # Gravimetric content of coarse fraction
  df$amt_calc_coarse_fraction_pp = df$amt_coarse_airdry_g / df$amt_total_oven_dry_sample_g

  # total SOC stocks
  df$amt_calc_orgc_mgha = df$amt_orgc_p *
    df$amt_bulkdensity_total_gcm3 *
    (df$amt_sample_depth_lower_cm - df$amt_sample_depth_upper_cm) *
    (1 - df$amt_calc_coarse_fraction_pp)

  # Total nitrogen stocks
  df$amt_calc_tn_mgha = df$amt_tn_p *
    df$amt_bulkdensity_total_gcm3 *
    (df$amt_sample_depth_lower_cm - df$amt_sample_depth_upper_cm) *
    (1 - df$amt_calc_coarse_fraction_pp)

  return(df)
}

#' Process the Downloaded Data
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#' @returns a data.frame with added/processed data
#' @export
process_mfe_data <- function(df) {

  # Add SOC and TN stocks
  df <- calculate_stocks(df)

  return(df)
}
