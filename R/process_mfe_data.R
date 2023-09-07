#' @noRd
.calculate_total_dry_weight <- function(
    amt_sample_airdry_g,
    amt_coarse_airdry_g,
    amt_airdry_water_content_p,
    amt_field_moist_water_content_p,
    amt_sample_wet_g
  ) {

  if (!is.na(amt_coarse_airdry_g) & !is.na(amt_sample_airdry_g) & !is.na(amt_airdry_water_content_p)) {
    # Total oven dry sample weight
    total_dry_weight <- amt_coarse_airdry_g + (amt_sample_airdry_g - amt_coarse_airdry_g) / (1 + amt_airdry_water_content_p / 100)
  } else if (!is.na(amt_field_moist_water_content_p) & !is.na(amt_sample_wet_g)) {
    # ALTERNATIVE way to compute total dry_weight
    # Calculate moisture factor
    moisture_factor <- 1 + amt_field_moist_water_content_p / 100
    total_dry_weight <-  amt_sample_wet_g / moisture_factor
  } else {
    total_dry_weight <- NA
  }

  return(total_dry_weight)
}

#' @noRd
.calculate_bd_fines <- function(
  # FIELDS USED IN BD FINES COMPUTATION
  #
  # - amt_sample_airdry_g
  # - amt_coarse_airdry_g
  # - amt_airdry_water_content_p
  # - amt_sampled_volume_cm3
  # - amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3
  amt_sampled_volume_cm3,
  amt_sample_airdry_g,
  amt_coarse_airdry_g,
  amt_airdry_water_content_p,
  amt_field_moist_water_content_p,
  amt_sample_wet_g,
  amt_fine_od_g
  ) {

  if(!is.na(amt_fine_od_g)) {
    # If we have the oven dry weight of fine fraction
    fine_bulk_density <- amt_fine_od_g / amt_sampled_volume_cm3
  } else {
    # Otherwise we have to use one of 2 methods
    # to compute the total dry weight
    total_dry_weight <- .calculate_total_dry_weight(
      amt_sample_airdry_g,
      amt_coarse_airdry_g,
      amt_airdry_water_content_p,
      amt_field_moist_water_content_p,
      amt_sample_wet_g
    )

    # Calculate total BD
    total_bulk_density <- total_dry_weight / amt_sampled_volume_cm3

    # Calculate coarse fraction
    coarse_fraction <- amt_coarse_airdry_g / total_dry_weight
    fine_fraction <- 1 - coarse_fraction

    # Calculate BD of fine fraction
    fine_bulk_density <- total_bulk_density * fine_fraction
  }

  return(fine_bulk_density)
}


#' Calculate Carbon and Nitrogen Stocks
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#'
#' @returns a data.frame with two additional columns for SOC and total N stocks
#'
#' @author Pierre Roudier
#'
#' @importFrom dplyr mutate
#' @export
calculate_stocks <- function(df) {

  res <- mutate(df,
    amt_sample_depth_upper_cm = lab_samplingdepth_minval,
    amt_sample_depth_lower_cm = lab_samplingdepth_maxval,
    # amt_fine_od_g = NA,
    # amt_sampled_volume_cm3 = 1152,
    thickness = abs(amt_sample_depth_lower_cm - amt_sample_depth_upper_cm),
    fine_bulk_density = .calculate_bd_fines(
      amt_sampled_volume_cm3,
      amt_sample_airdry_g,
      amt_coarse_airdry_g,
      amt_airdry_water_content_p,
      amt_field_moist_water_content_p,
      amt_sample_wet_g,
      amt_fine_od_g
    ),
    carbon_stocks = amt_orgc_p * thickness * fine_bulk_density,
    nitrogen_stocks = amt_tn_p * thickness * fine_bulk_density
  )

  return(res)
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
