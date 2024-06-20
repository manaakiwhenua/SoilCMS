#' @noRd
.calculate_coarse_fraction <- function(
    amt_coarse_airdry_g,
    amt_total_oven_dry_sample_g
    ) {

  # If the air dry weight of coarse fraction is recorded as zero
  # we don't need to even check the total sample weight (which may be NA)
  if (!is.na(amt_coarse_airdry_g) & amt_coarse_airdry_g == 0) {
    coarse_fraction <- 0
  } else {
    coarse_fraction <- amt_coarse_airdry_g / amt_total_oven_dry_sample_g
  }

  return(coarse_fraction)
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
  # amt_field_moist_water_content_p,
  amt_sample_wet_g,
  amt_total_oven_dry_sample_g,
  amt_calc_coarse_fraction_pp,
  amt_bulkdensity_total_gcm3,
  amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3,
  amt_fine_od_g,
  amt_field_moist_water_content_p
  ) {

  # First we check if the fine BD has been inputed
  if(!is.na(amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3)) {
    fine_bulk_density <- amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3
  }
  # Then, we check if the total BD AND fine fraction has been inputed
  else if(!is.na(amt_bulkdensity_total_gcm3)) {

    # If there is no total dry weight info we have to calculate it
    if (is.na(amt_total_oven_dry_sample_g)) {
      amt_total_oven_dry_sample_g <- amt_coarse_airdry_g +
        (amt_sample_airdry_g - amt_coarse_airdry_g) /
        (1 + amt_airdry_water_content_p / 100)
    }

    # Calculate coarse fraction
    if (is.na(amt_calc_coarse_fraction_pp)) {
      coarse_fraction <- .calculate_coarse_fraction(
        amt_coarse_airdry_g,
        amt_total_oven_dry_sample_g
      )
    } else {
      coarse_fraction <- amt_calc_coarse_fraction_pp
    }

    # Calculate BD of fine fraction (fine_fraction = 1 - coarse_fraction)
    fine_bulk_density <- amt_bulkdensity_total_gcm3 * (1 - coarse_fraction)
  }
  # Then if all has fail then we compute from scratch
  else {

    if(!is.na(amt_fine_od_g)) {
      # If we have the oven dry weight of fine fraction
      fine_bulk_density <- amt_fine_od_g / amt_sampled_volume_cm3
    } else {

      # Otherwise we have to use one of 2 methods
      # to compute the total dry weight

      # If there is no total dry weight info we have to calculate it
      if (is.na(amt_total_oven_dry_sample_g)) {

        # If we have total coarse and fractions dry weights
        if (!is.na(amt_coarse_airdry_g) & !is.na(amt_sample_airdry_g)) {
          amt_total_oven_dry_sample_g <- amt_coarse_airdry_g +
            (amt_sample_airdry_g - amt_coarse_airdry_g) /
            (1 + amt_airdry_water_content_p / 100)
        } else {
          # Otherwise we use the soil water content of field moist sample
          amt_total_oven_dry_sample_g <- amt_sample_wet_g / (1 + amt_field_moist_water_content_p/100)
        }

      }

      # Calculate total BD
      total_bulk_density <- amt_total_oven_dry_sample_g / amt_sampled_volume_cm3

      # Calculate coarse fraction
      if (is.na(amt_calc_coarse_fraction_pp)) {
        coarse_fraction <- .calculate_coarse_fraction(amt_coarse_airdry_g, amt_total_oven_dry_sample_g)
      } else {
        coarse_fraction <- amt_calc_coarse_fraction_pp
      }

      # Calculate BD of fine fraction (fine_fraction = 1 - coarse_fraction)
      fine_bulk_density <- total_bulk_density * (1 - coarse_fraction)
    }
  }

  return(fine_bulk_density)
}

#' Calculate Carbon and Nitrogen Stocks
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#'
#' @returns a data.frame with two additional columns for SOC and total N stocks
#'
#' @details
#' This function calculates organic carbon and total nitrogen stocks from the NSDR export.
#' Stocks are calculated as:
#' \eqn{stock = carbon concentration \times horizon thickness \times bulk density of fine fraction}
#'
#'
#' @author Pierre Roudier
#'
#' @export
calculate_stocks <- function(df) {

  # If we have a column from the DB with the calculated stocks,
  # we bypass the calculation
  if ("amt_calc_orgc_mgha" %in% names(df)) {
    df$carbon_stocks <- df$amt_calc_orgc_mgha
    df$nitrogen_stocks <- df$amt_tn_p * df$thickness * df$fine_bulk_density
  } else {
    # Otherwise, we calculate stocks
    df$carbon_stocks <- df$amt_orgc_p * df$thickness * df$fine_bulk_density
    df$nitrogen_stocks <- df$amt_tn_p * df$thickness * df$fine_bulk_density
  }

  return(df)
}

#' Calculate the bulk density of the fine fraction
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#' @returns a data.frame with added/processed data
#' @importFrom plyr alply
#'
calculate_fine_bd <- function(df) {
  df$fine_bulk_density <- do.call(
    c,
    plyr::alply(
      df,
      1,
      function(x) {

        res <- .calculate_bd_fines(
          amt_sampled_volume_cm3 = x$amt_sampled_volume_cm3,
          amt_sample_airdry_g = x$amt_sample_airdry_g,
          amt_coarse_airdry_g = x$amt_coarse_airdry_g,
          amt_airdry_water_content_p = x$amt_airdry_water_content_p,
          amt_sample_wet_g = x$amt_sample_wet_g,
          amt_total_oven_dry_sample_g = x$amt_total_oven_dry_sample_g,
          amt_calc_coarse_fraction_pp = x$amt_calc_coarse_fraction_pp,
          amt_bulkdensity_total_gcm3 = x$amt_bulkdensity_total_gcm3,
          amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3 = x$amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3,
          amt_fine_od_g = x$amt_fine_od_g,
          amt_field_moist_water_content_p = x$amt_field_moist_water_content_p
        )

        return(res)
      }
    )
  )

  return(df)
}

#' Calculate sample volume
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#' @returns a data.frame with added/processed data
#' @importFrom dplyr mutate case_when
calculate_volume <- function(df) {
  # PI()*(amt_core_diameter_cm/2)^2*(thickness)*n_composite

  df <- mutate(
      df,
      amt_sampled_volume_cm3 = case_when(
        # Case if NA, and core method
        (
          is.na(amt_sampled_volume_cm3) &
            type_method != "3. Quantitative pit for stony soils"
        ) ~ pi * (amt_core_diameter_cm_val/2)^2 * (thickness) * n_composite,

        # Case if value exported already
        TRUE ~ amt_sampled_volume_cm3
      )
    )

  return(df)
}

#' Calculate sample thickness
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#' @returns a data.frame with added/processed data
#'
calculate_thickness <- function(df) {

  # Calculate thickness
  df$amt_sample_depth_upper_cm <- df$depth_minval # df$lab_samplingdepth_minval
  df$amt_sample_depth_lower_cm <- df$depth_maxval # df$lab_samplingdepth_maxval
  df$thickness <- abs(df$amt_sample_depth_lower_cm - df$amt_sample_depth_upper_cm)

  return(df)
}

#' Check that all columns necessary are included in the SQLite
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#' @returns Nothing, this function is called for its side effects
#'
#' @noRd
#'
check_columns <- function(df) {

  cols_needed <- c(
    # "lab_samplingdepth_minval",
    # "lab_samplingdepth_maxval",
    "depth_minval",
    "depth_maxval",
    "amt_sampled_volume_cm3",
    # "amt_core_diameter_cm_val",
    # "n_composite",
    "amt_sample_wet_g",
    "amt_sample_airdry_g",
    "amt_coarse_airdry_g",
    "amt_airdry_water_content_p",
    "amt_calc_coarse_fraction_pp",
    "amt_fine_od_g",
    "amt_field_moist_water_content_p",
    "amt_bulkdensity_total_gcm3",
    "amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3",
    "amt_orgc_p",
    "amt_tn_p"
  )

  cols_df <- names(df)

  missing_cols <- which(! cols_needed %in% cols_df)

  if (length(missing_cols) > 0) {
    stop(
      "Missing column(s) in the input dataset:\n\n",
      paste0(cols_needed[missing_cols], collapse = ",\n"),
      call. = FALSE
    )
  }

  return(NULL)
}

#' Process the Downloaded Data
#'
#' @param df a data.frame read from the NSDR Viewer SQLite
#' @returns a data.frame with added/processed data
#' @export
process_mfe_data <- function(df) {

  # Check all columns are here
  check_columns(df)

  # Calculate sample thickness
  df <- calculate_thickness(df)

  # Calculate total sample volume
  df <- calculate_volume(df)

  # Calculate bulk density of fine fraction
  df <- calculate_fine_bd(df)

  # Add SOC and TN stocks
  df <- calculate_stocks(df)

  return(df)
}
