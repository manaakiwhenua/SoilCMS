#' @title Check if data model is a monitoring site or not
#' @importFrom RSQLite dbReadTable
#' @importFrom dplyr pull
#' @keywords internal
#' @noRd
#'
.isMonitoring <- function(con) {

  test <- "sa_sitevisit" %in% dbListTables(con)

  # This represents the identifier for a monitored dataset
  # md <- "1106-02-mon-sc-00"
  #
  # # Test used to see whether dataset is monitored
  # test <- pull(
  #   dbReadTable(con, "dm_constraintset"),
  #   "dm_constraintset_id"
  # ) == md

  return(test)
}

#' @title Check all required tables are present in SQLite
#' @importFrom RSQLite dbListTables
#' @keywords internal
#' @noRd
#'
.checkRequiredTables <- function(con) {

  # Required tables
  req_tbls <- c(
    # Views
    "MfE_carbon_sample",

    # Tables
    "sa_sample",
    "ob_observation"
  )

  if (.isMonitoring(con)) req_tbls <- c(req_tbls, "sa_sitevisit")

  tbls <- dbListTables(con)

  for (t in req_tbls) {
    if (! t %in% tbls) {
      stop(
        paste0(
          "The required table '", t, "' isn't available in the SQLite database."
        ),
        call. = FALSE
      )
    }
  }

  return(invisible(NULL))
}

#' @title Get subsite site data from sample table
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select left_join join_by collect
#' @keywords internal
#' @noRd
#'
.getSubsites <- function(con) {

  if (.isMonitoring(con)) {

    # If it's a monitoring dataset, we need to get the site_id
    # from the sa_visit_data table
    res <- tbl(con, "sa_sample") |>
      select(
        sa_sitevisit_id,
        subsite_id = "sc_sub_site_identifier"
      ) |>
      full_join(
        tbl(con, "sa_sitevisit") |>
          select(
            site_id = sa_monitoringsite_id,
            sa_sitevisit_id
          ),
        by = join_by(sa_sitevisit_id)
      ) |>
      # Remove sitevist_id
      select(
        -sa_sitevisit_id
      ) |>
      distinct() |>
      # Run request
      collect()

  } else {
    res <- tbl(con, "sa_sample") |>
      # Subset specific columns
      select(
        site_id = "sa_site_id",
        subsite_id = "sc_sub_site_identifier"
      ) |>
      # Run request
      collect()
  }

  # Remove duplicates and reorder columns
  res <- res |>
    select(site_id, subsite_id) |>
    distinct()

  return(res)
}

#' @keywords internal
#' @noRd
.setCol <- function(df, col) {
  # Check if 'c' exists and transmute if it doesn't
  if (!col %in% names(df)) {
    res <- rep(NA, times = nrow(df))
  } else {
    res <- df[[col]]
  }

  return(res)
}

#' @title Get soil data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select
#' @keywords internal
#' @noRd
.getSoilTbl <- function(con) {

  if (.isMonitoring(con)) {
    res <- tbl(con, "sd_soil_unique") |>
      select(
        site_id = sa_sitevisit_id,
        # sd_soil_id,
        nzsc = classifier_nzsc,
        nzsc_alt = classifier_nzsc_alt
      )
  } else {
    res <- tbl(con, "sd_soil_unique") |>
      select(
        site_id = sa_site_id,
        # sd_soil_id,
        nzsc = classifier_nzsc,
        nzsc_alt = classifier_nzsc_alt
      )
  }

  return(res)
}

#' @title Get dataset metadata
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select
#' @keywords internal
#' @noRd
.getDatasetTbl <- function(con) {

  if (.isMonitoring(con)) {
    res <- tbl(con, "dm_dataset_data") |>
      select(
        dataset = name,
        dataset_id = dm_monitoringdataset_id
      )
  } else {
    res <- tbl(con, "dm_dataset_data") |>
      select(
        dataset = name,
        dataset_id = dm_surveydataset_id
      )
  }

  return(res)
}

#' @title Get site metadata
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select
#' @keywords internal
#' @noRd
.getSiteMetaTbl <- function(con) {

  if (.isMonitoring(con)) {

    # If it's a monitoring dataset, we need to get the site_id
    # from the sa_visit_data table
    res <- tbl(con, "sa_visit_data") |>
      select(
        # Dataset
        dataset_id = dm_monitoringdataset_id,

        # Site identification
        site_id = sa_monitoringsite_id ,
        site_identifier = site_identifier,
        site_identifier_alt = site_identifier_alt,

        # Spatial coordinates
        location_x = location_x,
        location_y = location_y,
        location_srid = location_srid,

        # Altitude
        # altitude_val = location_altitude_val,
        # altitude_uom = location_altitude_uom,

        # Other metadata
        visit_date = visit_date,
        # site_type = type,
        authority = visit_authority#,
        # sampler
      )

  } else {

    res <- tbl(con, "sa_site") |>
      select(
        # Dataset
        dataset_id = dm_surveydataset_id,

        # Site identification
        site_id = sa_site_id,
        site_identifier = identifier,
        site_identifier_alt = identifier_alt,

        # Spatial coordinates
        location_x = location_geometry_x,
        location_y = location_geometry_y,
        location_srid = location_geometry_srid,

        # Altitude
        # altitude_val = location_altitude_val,
        # altitude_uom = location_altitude_uom,

        # Other metadata
        visit_date = date,
        # site_type = type,
        authority = authority#,
        # sampler
      )
  }
  return(res)
}

#' @title Get landscape position data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select
#' @keywords internal
#' @noRd
.getLandscapeTbl <- function(c) {
  res <- tbl(con, "lc_landscape") |>
    select(
      site_id = sa_site_id,
      slopeangle_val,
      slopeaspect_val
    )
  return(res)
}

#' @title Get landuse notes data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select left_join
#' @keywords internal
#' @noRd
.getLanduseNotesTbl <- function(con) {
  res <- tbl(con, "lc_landuse_note") |>
    select(
      lc_landuse_id,
      landuse_notes = note
    )
  return(res)
}

#' @title Get landuse data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select left_join
#' @keywords internal
#' @noRd
.getLanduseTbl <- function(con) {

  tbl_lu <- tbl(con, "lc_landuse")

  if (.isMonitoring(con)) {
    res <- tbl_lu |>
      select(
        site_id = sa_sitevisit_id,
        lc_landuse_id = lc_landuse_id
      )
  } else {
    res <- tbl_lu |>
      select(
        site_id = sa_site_id,
        lc_landuse_id = lc_landuse_id
      )
  }

  # Add option fields
  if ("landuse" %in% names(tbl_lu)) {
    res$landuse <- tbl_lu$landuse
  } else res$landuse <- NA

  if("landuse_previous" %in% names(tbl_lu)) {
    res$landuse_previous <- tbl_lu$landuse_previous
  } else res$landuse_previous <- NA

  if ("landuse_lum" %in% names(tbl_lu)) {
    res$landuse_lum <- tbl_lu$landuse_lum
  } else res$landuse_lum <- NA

  # landuse_duration
  if ("sc_val_landuse_duration" %in% names(tbl_lu)) {
    res$landuse_duration <- tbl_lu$sc_val_landuse_duration
  } else res$landuse_duration <- NA

  # forest_rotation (sc_val_forest_rotation in lc_landuse)
  if ("sc_val_forest_rotation" %in% names(tbl_lu)) {
    res$forest_rotation <- tbl_lu$sc_val_forest_rotation
  } else res$forest_rotation <- NA

  # irrigation (irrigated in lc_landuse)
  if ("irrigated" %in% names(tbl_lu)) {
    res$irrigation <- tbl_lu$irrigated
  } else res$irrigation <- NA

  # irrigation_method (sc_txt_irrigation_method in lc_landuse)
  if ("sc_txt_irrigation_method" %in% names(tbl_lu)) {
    res$irrigation_method <- tbl_lu$sc_txt_irrigation_method
  } else res$irrigation_method <- NA

  # irrigation_duration (sc_txt_irrigation_duration in lc_landuse)
  if ("sc_txt_irrigation_duration" %in% names(tbl_lu)) {
    res$irrigation_duration <- tbl_lu$sc_txt_irrigation_duration
  } else res$irrigation_duration <- NA

  # planting_year (sc_val_planting_year in lc_landuse)
  if ("sc_val_planting_year" %in% names(tbl_lu)) {
    res$planting_year <- tbl_lu$sc_val_planting_year
  } else res$planting_year <- NA

  # tree_age_planting (sc_val_tree_age_at_sampling in lc_landuse)
  if ("sc_val_tree_age_at_sampling" %in% names(tbl_lu)) {
    res$tree_age_planting <- tbl_lu$sc_val_tree_age_at_sampling
  } else res$tree_age_planting <- NA

  # management_description (description in lc_landuse)
  if ("description" %in% names(tbl_lu)) {
    res$management_description <- tbl_lu$description
  } else res$management_description <- NA

  # Add landuse_notes from lc_landuse_note
  res <- res |>
    left_join(
      .getLanduseNotesTbl(con),
      by = join_by(lc_landuse_id)
    ) |>
    select(-lc_landuse_id)

  # Remove and concatenate
  res <- res |>
    collect() |>
    group_by(site_id) |>
    summarise(
      across(
        everything(),
        function(x) {
          idx_valid <- which(x != "" & !is.na(x))
          res <- paste0(x[idx_valid], collapse = ",")
          return(res)
        }
      ),
      .groups = "drop"
    )

  return(res)
}


#' @title Get site data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select starts_with any_of contains distinct collect tbl relocate
#' @keywords internal
#' @noRd
#'
.getSiteTbl <- function(con) {

  # if (.isMonitoring(con)) {
  #
  #   # res <- .getSiteMetaTbl(con) |>
  #
  #
  #
  #   # If it's a monitoring dataset, we need to get the site_id
  #   # from the sa_visit_data table
  #   # res <- tbl(con, "sa_sample") |>
  #   #   select(
  #   #     sa_sitevisit_id,
  #   #     subsite_id = "sc_sub_site_identifier"
  #   #   ) |>
  #   #   left_join(
  #   #     tbl(con, "sa_sitevisit") |>
  #   #       select(
  #   #         site_id = sa_monitoringsite_id,
  #   #         sa_sitevisit_id
  #   #       ),
  #   #     by = join_by(sa_sitevisit_id)
  #   #   ) |>
  #   #   # Remove sitevist_id
  #   #   select(
  #   #     -sa_sitevisit_id
  #   #   ) |>
  #   #   # Run request
  #   #   collect()
  #
  # } else {

    res <- .getSiteMetaTbl(con) |>

      # Add dataset information
      inner_join(
        .getDatasetTbl(con),
        by = join_by(dataset_id)
      ) |>

      # Add pedological information
      left_join(
        .getSoilTbl(con),
        by = join_by(site_id)
      ) |>

      # Run request as we've had to also run the landuse data request
      collect() |>

      # Add landuse and management information
      left_join(
        .getLanduseTbl(con),
        by = join_by(site_id)
      ) |>

      # Add landscape position information
      left_join(
        collect(.getLandscapeTbl(con)),
        by = join_by(site_id)
      ) |>
      # Remove duplicate rows
      distinct()

  # }

  # ADD SUBSITE_ID
  res <- res |>
    left_join(
      .getSubsites(con),
      by = join_by(site_id),
      relationship = "one-to-many"
    ) |>
    relocate(
      subsite_id,
      .after = site_id
    )

  return(res)
}

#' @title Get sample support data (ids, depths)
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select starts_with any_of contains distinct mutate case_when collect tbl
#' @importFrom tidyr drop_na
#' @keywords internal
#' @noRd
#'
.getSampleSupport <- function(con) {

  # Correcting factors for units different than cm
  depth_units_factor <- c("mm" = 0.1, "cm" = 1, "m" = 100)

  # Find out how to get the correct site_id
  is_monitoring <- .isMonitoring(con)

  if (is_monitoring) {

    # If it's a monitoring dataset, we need to get the site_id
    # from the sa_visit_data table
    res <- tbl(con, "sa_sample") |>
      select(
        sa_sitevisit_id,
        subsite_id = "sc_sub_site_identifier",
        sa_sample_id,
        sa_laboratorysample_id,
        sample_identifier = identifier,
        sample_identifier_alt = identifier_alt,
        type,
        any_of(c("type_composite", "type_method", "amt_core_diameter_cm_val", "n_composite", "area_composite_samples_represent")),
        field_samplingdepth_minval,
        field_samplingdepth_maxval,
        field_samplingdepth_uom,
        lab_samplingdepth_minval,
        lab_samplingdepth_maxval,
        lab_samplingdepth_uom
      ) |>
      left_join(
        tbl(con, "sa_sitevisit") |>
          select(
            sa_monitoringsite_id,
            sa_sitevisit_id
          ),
        by = join_by(sa_sitevisit_id)
      ) |>
      # Run request
      collect() |>
      # Harmonise lab and field depths
      mutate(
        # Site_id
        site_id = sa_monitoringsite_id,
        # Min depth
        depth_minval = case_when(
          # If the lab depth is valid
          !is.na(lab_samplingdepth_minval) ~ lab_samplingdepth_minval * depth_units_factor[lab_samplingdepth_uom],
          # If the lab depth is invalid but the field depth is valid
          is.na(lab_samplingdepth_minval) & is.na(field_samplingdepth_minval) ~ field_samplingdepth_minval * depth_units_factor[field_samplingdepth_uom],
          # if there are no valid depths
          TRUE ~ NA
        ),
        # Max depth
        depth_maxval = case_when(
          # If the lab depth is valid
          !is.na(lab_samplingdepth_maxval) ~ lab_samplingdepth_maxval * depth_units_factor[lab_samplingdepth_uom],
          # If the lab depth is invalid but the field depth is valid
          is.na(lab_samplingdepth_maxval) & is.na(field_samplingdepth_maxval) ~ field_samplingdepth_maxval * depth_units_factor[field_samplingdepth_uom],
          # if there are no valid depths
          TRUE ~ NA
        ),
        # Add UOM field (cm)
        depth_uom = case_when(
          !is.na(depth_minval) | !is.na(depth_maxval) ~ "cm",
          TRUE ~ NA
        )
      ) |>
      # Remove field and lab depth fields
      select(
        -starts_with("field_samplingdepth"),
        -starts_with("lab_samplingdepth"),
        -sa_monitoringsite_id
      ) |>
      # Remove records that are all NA
      drop_na(
        starts_with("depth_")
      )

  } else {
    res <- tbl(con, "sa_sample") |>
      # Subset specific columns
      select(
        site_id = "sa_site_id",
        subsite_id = "sc_sub_site_identifier",
        sa_sample_id,
        sa_laboratorysample_id,
        sample_identifier = identifier,
        sample_identifier_alt = identifier_alt,
        type,
        any_of(c("type_composite", "type_method", "amt_core_diameter_cm_val", "n_composite", "area_composite_samples_represent")),
        field_samplingdepth_minval,
        field_samplingdepth_maxval,
        field_samplingdepth_uom,
        lab_samplingdepth_minval,
        lab_samplingdepth_maxval,
        lab_samplingdepth_uom
      ) |>
      # Run request
      collect() |>
      # Harmonise lab and field depths
      mutate(
        # Min depth
        depth_minval = case_when(
          # If the lab depth is valid
          !is.na(lab_samplingdepth_minval) ~ lab_samplingdepth_minval * depth_units_factor[lab_samplingdepth_uom],
          # If the lab depth is invalid but the field depth is valid
          is.na(lab_samplingdepth_minval) & is.na(field_samplingdepth_minval) ~ field_samplingdepth_minval * depth_units_factor[field_samplingdepth_uom],
          # if there are no valid depths
          TRUE ~ NA
        ),
        # Max depth
        depth_maxval = case_when(
          # If the lab depth is valid
          !is.na(lab_samplingdepth_maxval) ~ lab_samplingdepth_maxval * depth_units_factor[lab_samplingdepth_uom],
          # If the lab depth is invalid but the field depth is valid
          is.na(lab_samplingdepth_maxval) & is.na(field_samplingdepth_maxval) ~ field_samplingdepth_maxval * depth_units_factor[field_samplingdepth_uom],
          # if there are no valid depths
          TRUE ~ NA
        ),
        # Add UOM field (cm)
        depth_uom = case_when(
          !is.na(depth_minval) | !is.na(depth_maxval) ~ "cm",
          TRUE ~ NA
        )
      ) |>
      # Remove field and lab depth fields
      select(
        -starts_with("field_samplingdepth"),
        -starts_with("lab_samplingdepth")
      ) |>
      # Remove records that are all NA
      drop_na(
        starts_with("depth_")
      )
  }

  return(res)
}

#' @title Get soil chemistry observations
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select mutate case_when filter collect tbl
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @noRd
#'
.getChemObs <- function(con) {

  # Table containing all observations data
  tbl_ob_obs <- tbl(con, "ob_observation")

  res <- tbl_ob_obs |>
    mutate(
      name = case_when(
        propertytypeid %in% c(2663, 1814) & result_uom == "%" ~ "amt_orgc_p",
        propertytypeid %in% c(2663, 1814) & result_uom == "Mg/ha" ~ "amt_calc_orgc_mgha",
        propertytypeid == 1832 & result_uom == "%" ~ "amt_tn_p",
        TRUE ~ NULL
      )
    ) |>
    filter(
      !is.null(name)
    ) |>
    select(
      sa_laboratorysample_id,
      result_val,
      name
    ) |>
    pivot_wider(
      names_from = name,
      values_from = result_val
    ) |>
    collect()

  return(res)
}

#' @title Get soil physics observations
#' @importFrom dbplyr tbl
#' @importFrom dplyr select mutate case_when filter collect
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @noRd
#'
.getPhysObs <- function(con) {

  # Table containing all observations data
  tbl_ob_obs <- tbl(con, "ob_observation")

  res <- tbl_ob_obs|>
    collect() |>
    mutate(
      name = case_when(
        propertytypeid == 2777 & str_detect(result_uom, fixed("cm")) ~ "amt_sampled_volume_cm3",
        propertytypeid == 2763 & result_uom == "g" ~ "amt_sample_wet_g",
        propertytypeid == 2764 & result_uom == "g" ~ "amt_sample_airdry_g",
        propertytypeid == 2768 & result_uom == "g" ~ "amt_stones_2mm_airdry_g",
        propertytypeid == 2769 & result_uom == "g" ~ "amt_roots_2mm_airdry_g",
        propertytypeid == 2770 & result_uom == "g" ~ "amt_coarse_airdry_g",
        propertytypeid == 2750 & result_uom == "%" ~ "amt_airdry_water_content_p",
        propertytypeid == 2767 & result_uom == "g" ~ "amt_total_oven_dry_sample_g",
        propertytypeid == 2778 & result_uom == "g" ~ "amt_fine_od_g",
        propertytypeid == 1978 & result_uom %in% c('g/mL', 't/m3') ~ "amt_bulkdensity_total_gcm3",
        propertytypeid == 1980 & result_uom %in% c('g/mL', 't/m3') ~ "amt_bulkdensity_of_2mm_per_tot_sample_volume_gcm3",
        propertytypeid == 2779 ~ "amt_calc_coarse_fraction_pp",
        propertytypeid == 1988 & result_uom == "%" ~ "amt_field_moist_water_content_p",
        TRUE ~ NA
      )
    ) |>
    filter(
      !is.na(name)
    ) |>
    select(
      sa_laboratorysample_id,
      result_val,
      name
    ) |>
    distinct() |>
    pivot_wider(
      names_from = name,
      values_from = result_val
    )

  return(res)
}

#' @title Assemble soil chemistry data
#' @importFrom dplyr left_join join_by starts_with contains any_of group_by summarise across select mutate
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @noRd
#'
.getChem <- function(tbl_sa_spl, tbl_ob_obs_chem) {

  res <- tbl_sa_spl |>
    left_join(
      tbl_ob_obs_chem,
      by = join_by(sa_laboratorysample_id == sa_laboratorysample_id)
    ) |>
    # Remove empty records
    drop_na(
      starts_with("amt_")
    ) |>
    # Aggregate at the site/subsite level
    # - remove sample IDs
    select(-contains("sample")) |>
    # - group_by site, subiste, depths
    group_by(
      across(
        any_of(
          c("site_id", "subsite_id", "depth_minval", "depth_maxval")
        )
      )
    ) |>
    summarise(
      # Quantitative variables
      across(
        starts_with("amt_"),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  return(res)
}

#' @title Assemble soil physics data
#' @importFrom dplyr left_join join_by starts_with contains any_of group_by summarise across select mutate
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @noRd
#'
.getPhys  <- function(tbl_sa_spl, tbl_ob_obs_phys) {

  res <- tbl_sa_spl |>
    left_join(
      tbl_ob_obs_phys,
      by = join_by(sa_laboratorysample_id == sa_laboratorysample_id)
    ) |>
    # Remove empty records
    drop_na(
      starts_with("amt_")
    ) |>
    # Aggregate at the site/subsite level
    # - remove sample IDs
    select(-contains("sample")) |>
    # - group_by site, subiste, depths
    group_by(
      across(
        any_of(
          c("site_id", "subsite_id", "depth_minval", "depth_maxval")
        )
      )
    ) |>
    summarise(
      # Quantitative variables
      across(
        starts_with("amt_"),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  return(res)
}

#' @title Aggregate soil physics data on the soil chem depth intervals
#' @importFrom dplyr filter summarise across
#' @importFrom tidyr pivot_wider
#' @importFrom pbapply pblapply
#' @importFrom aqp depths `depths<-` dice
#' @keywords internal
#' @noRd
#'
.aggregateSoilPhys <- function(tbl_site, tbl_chem, tbl_phys) {
  # For each site in the dataset
  #   - we pull the chem depths
  #   - we pull the phys depths
  #   - we match phys depths to chem depths

  # Custom weighted mean function that weights mean based
  # on the cross tabulation of values to average
  .weighted_mean_count <- function(x) {
    # Remove any NAs
    x <- as.numeric(na.omit(x))
    tb <- table(x)
    lv <- as.numeric(names(tb))

    weighted.mean(x = lv, w = tb)
  }

  ####################################
  ####
  #### NEED TO IMPLEMENT SUBSITES!!!
  ####
  ####################################

  # Pull all the unique site IDs of the dataset
  sids_df <- tbl_site %>%
    select(site_id, subsite_id) %>%
    distinct()

  l_res <- pbapply::pblapply(
      1:nrow(sids_df),
      function(i) {

        sid <- sids_df$site_id[i]
        subid <- sids_df$subsite_id[i]

        chem <- tbl_chem |> filter(site_id == sid & subsite_id == subid)
        phys <- tbl_phys |> filter(site_id == sid & subsite_id == subid)

        # That's if there no soil physics data at all
        if (nrow(phys) == 0) {
          return(NULL)
        }

        # Convert soil physics data.frame to a SoilProfileCollection
        phys_sdf <- phys
        depths(phys_sdf) <- site_id ~ depth_minval + depth_maxval

        # Create diced soil profile
        phys_dc <- dice(
          phys_sdf,
          fm = as.formula(paste0(min(chem$depth_minval), ":", max(chem$depth_maxval), " ~ .")),
          SPC = FALSE
        ) |>
          select(names(phys))

        # For each sample in the soil chemistry profile
        res <- plyr::adply(
          chem,
          1,
          function(dc) {
            min_depth <- dc$depth_minval
            max_depth <- dc$depth_maxval

            rres <- phys_dc |>
              filter(depth_minval >= min_depth & depth_maxval <= max_depth) |>
              summarise(
                across(
                  starts_with("amt_"),
                  .weighted_mean_count
                ),
                .groups = "drop"
              )

            return(rres)
          }
        )

        return(res)
      }
    )

  res <- bind_rows(l_res)

  return(res)
}

#' @title Read a SQLite DB into a data.frame
#'
#' @param fn Path to a SQLite file
#' @param view Name of the SQLite View to load. Defaults to "MfE_Carbon_data".
#' @param legacy Boolean, default to TRUE. This is the old system relying purely
#'  on Views. It is kept temporarily for testing purposes.
#'
#' @returns a data.frame
#'
#' @author Pierre Roudier
#'
#' @importFrom RSQLite SQLite dbConnect dbListTables  dbReadTable dbDisconnect
#' @export
read_mfe_sqlite <- function(fn, view = "MfE_Carbon_data", legacy = TRUE) {

  # Check file exists
  if (!file.exists(fn)) {
    stop("The requested file doesn't exists", call. = FALSE)
  }
  if (file.size(fn) == 0) {
    stop("The requested file is empty (size 0 kB)", call. = FALSE)
  }

  # Initiate connection to SQLite
  con <- dbConnect(SQLite(), fn)

  # This is the old system relying purely on Views.
  # It is kept temporarily for testing purposes.
  if (legacy) {
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

  # Check all required tables are present in SQLite
  .checkRequiredTables(con)

  # Get site tbl
  tbl_site <- .getSiteTbl(con)

  # Get sample support tbl
  tbl_sa_spl <- .getSampleSupport(con)

  # Test whether we have all site IDs correctly there for both site and sample support
  test_site_ids <- all(unique(tbl_sa_spl$site_id) %in% tbl_site$site_id)

  if (!test_site_ids) {
    warning(
      paste0(
        "Some unmatched site identifiers (",
        length(which(unique(tbl_sa_spl$site_id) %in% tbl_site$site_id)),
        " out of ",
        length(unique(tbl_sa_spl$site_id)),
        " matched)."
      ),
      call. = FALSE
    )
  }

  # Get soil chemistry observations
  tbl_ob_obs_chem <- .getChemObs(con)

  # Get soil physics observations
  tbl_ob_obs_phys <- .getPhysObs(con)

  # Disconnect from DB
  dbDisconnect(con)

  # Join sample table with soil chemistry table
  tbl_chem <- .getChem(tbl_sa_spl, tbl_ob_obs_chem)

  # Join sample table with soil physics table
  tbl_phys <- .getPhys(tbl_sa_spl, tbl_ob_obs_phys)

  # Run depth support harmonisation routine
  agg_chem_phys <- .aggregateSoilPhys(
    tbl_site = tbl_site,
    tbl_chem = tbl_chem,
    tbl_phys = tbl_phys
  )

  # Final table
  res <- tbl_site |>
    right_join(
      agg_chem_phys,
      by = join_by(site_id, subsite_id)
    )

  return(res)
}

fn1 = "/mnt/c/Users/RoudierP/OneDrive - MWLR/MFE_CARBON/soilcms-data/data/NSDR_Export_nscm_20250820.db"
fn2 = "/mnt/c/Users/RoudierP/OneDrive - MWLR/MFE_CARBON/soilcms-data/data/NSDR_Export_sustain_20250825.db"
con1  = dbConnect(RSQLite::SQLite(), fn1)
con2  = dbConnect(RSQLite::SQLite(), fn2)
