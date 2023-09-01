#' Calculate Carbon and Nitrogen Stocks
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#' @returns a data.frame with two additional columns for SOC and total N stocks
#'
calculate_stocks <- function(df) {

  #
  # CALCULATED FIELDS
  #

  # Typo in the VIEW, this is a temporary fix
  df$amt_coarse_airdry_g = df$amt_coarse_airdry_gv

  # Test to see if we have the bare necessities
  if (
    "amt_orgc_p" %in% names(df) &
    "amt_tn_p" %in% names(df) &
    "amt_sample_depth_lower_cm" %in% names(df) &
    "amt_sample_depth_upper_cm" %in% names(df)
  ) {
    stop("Can't calculate SOC and TN stocks due to missing information about soil depth and carbon/nitrogen concentrations.", call. = FALSE)
  }

  if (
    "amt_coarse_airdry_g" %in% names(df) &
    "amt_sample_airdry_g" %in% names(df) &
    "amt_airdry_water_content_p" %in% names(df) &
    "amt_bulkdensity_total_gcm3" %in% names(df)
  ) {

    # Total oven dry sample weight
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

  } else if (
    "amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3" %in% names(df)
  ) {
    # total SOC stocks
    df$amt_calc_orgc_mgha = df$amt_orgc_p *
      df$amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3 *
      (df$amt_sample_depth_lower_cm - df$amt_sample_depth_upper_cm)

    # Total nitrogen stocks
    df$amt_calc_tn_mgha = df$amt_tn_p *
      df$amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3 *
      (df$amt_sample_depth_lower_cm - df$amt_sample_depth_upper_cm)
  } else if(
    "amt_bulkdensity_total2_gcm3"
  ) {
    # total SOC stocks
    df$amt_calc_orgc_mgha = df$amt_orgc_p *
      (df$amt_total_oven_dry_sample2_g / amt_sampled_volume_cm3) *
      (df$amt_sample_depth_lower_cm - df$amt_sample_depth_upper_cm) *
      (1 - df$amt_calc_coarse_fraction_pp)

    # Total nitrogen stocks
    df$amt_calc_tn_mgha = df$amt_tn_p *
      (df$amt_total_oven_dry_sample2_g / amt_sampled_volume_cm3) *
      (df$amt_sample_depth_lower_cm - df$amt_sample_depth_upper_cm) *
      (1 - df$amt_calc_coarse_fraction_pp)
  } else {
    stop("Can't calculate SOC and TN stocks due to missing columns.", call. = FALSE)
  }

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
