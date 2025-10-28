#' @title Check if data model is a monitoring site or not
#' @importFrom RSQLite dbReadTable
#' @importFrom dplyr pull
#' @keywords internal
#' @noRd
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
#' @importFrom dplyr select left_join join_by collect full_join distinct
#' @keywords internal
#' @noRd
.getSubsites <- function(con) {

  if (.isMonitoring(con)) {

    # If it's a monitoring dataset, we need to get the site_id
    # from the sa_visit_data table
    res <- tbl(con, "sa_sample") |>
      select(
        sa_sitevisit_id,
        subsite_id = any_of("sc_sub_site_identifier")
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
      # select(
      #   -sa_sitevisit_id
      # ) |>
      distinct() |>
      # Run request
      collect()

  } else {
    res <- tbl(con, "sa_sample") |>
      # Subset specific columns
      select(
        site_id = "sa_site_id",
        subsite_id = any_of("sc_sub_site_identifier")
      ) |>
      distinct() |>
      # Run request
      collect()
  }

  # Remove duplicates and reorder columns
  res <- res |>
    # Add a subsite_id column if it was not present in the table
    .add_cols("subsite_id") |>
    # Make sure all empty strings are changed to NA
    mutate(
      subsite_id = case_when(
        subsite_id == "" ~ NA,
        TRUE ~ subsite_id
      )
    ) |>
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
#' @importFrom dplyr select left_join join_by
#' @keywords internal
#' @noRd
.getSoilTbl <- function(con) {

  if (.isMonitoring(con)) {

    tbl_visit <-  tbl(con, "sa_sitevisit")

    tbl_soil <- tbl(con, "sd_soil") |>
      left_join(
        tbl_visit,
        by = join_by(sa_sitevisit_id)
      )

    res <- tbl_soil |>
      select(
        sa_sitevisit_id,
        site_id = sa_monitoringsite_id,
        visit_date = date,
        nzsc = classifier_nzsc,
        nzsc_alt = classifier_nzsc_alt
      )

  } else {

    tbl_soil <- tbl(con, "sd_soil")

    res <- tbl_soil |>
      select(
        site_id = sa_site_id,
        nzsc = classifier_nzsc,
        nzsc_alt = classifier_nzsc_alt
      )
  }

  res <- collect(res)

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

  res <- collect(res)

  return(res)
}

#' @title Get site location
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select left_join full_join join_by any_of colnames case_when
#' @details
#' When no locations are available in "sa_visit_data", we
#' need to pull it from "sa_sample" (good example of that in
#' the "ECAN AP carbon" dataset).
#'
#' @keywords internal
#' @noRd
.getCoords <- function(con) {

  if (.isMonitoring(con)) {

    tbl_sites <- tbl(con, "sa_sample") |>
      select(
        sa_sitevisit_id,
        subsite_id = any_of("sc_sub_site_identifier")
      ) |>
      left_join(
        tbl(con, "sa_sitevisit")|>
          select(
            site_id = sa_monitoringsite_id,
            sa_sitevisit_id
          ),
        by = join_by(sa_sitevisit_id)
      ) |>
      distinct()

    # Pull site visit coords
    df_coords_visit <- tbl(con, "sa_visit_data") |>
      select(
        # Visit identification
        sa_sitevisit_id, visit_date,

        # Site identification
        site_id = sa_monitoringsite_id ,
        site_identifier = site_identifier,
        site_identifier_alt = site_identifier_alt,

        # Spatial coordinates
        location_x_site = location_x,
        location_y_site = location_y,
        location_srid_site = location_srid
      )

    # Pull sample coords
    df_coords_spl <- tbl(con, "sa_sample") |>

      # Selecting relevant columns
      select(
        # Visit identification
        sa_sitevisit_id,
        # Site identification
        subsite_id = any_of("sc_sub_site_identifier"),
        # Sample identification
        sa_sample_id,
        sa_laboratorysample_id,
        sample_identifier = identifier,
        sample_identifier_alt = identifier_alt,

        # Spatial coordinates
        location_x_spl = any_of("location_geometry_x"),
        location_y_spl = any_of("location_geometry_y"),
        location_srid_spl = any_of("location_geometry_srid")
      ) |>
      left_join(
        # Here we join to collate the site_id
        tbl(con, "sa_sitevisit") |>
          select(
            site_id = sa_monitoringsite_id,
            sa_sitevisit_id
          ),
        by = join_by(sa_sitevisit_id)
      )

    # If there are sample level coordinates, we need to be able to pick
    # between visit-level and site -level coords
    if (all(c("location_x_spl", "location_y_spl") %in% colnames(df_coords_spl))) {

      # Assemble vist-level and sample-level coords
      res <- df_coords_visit |>
        full_join(
          df_coords_spl,
          by = join_by(site_id, sa_sitevisit_id)
        ) |>
        # We use sample level coords if they are available, because they usually correspond
        # to specific replicates coordinates
        mutate(
          location_x = case_when(
            !is.na(location_x_spl) ~ location_x_spl,
            TRUE ~ location_x_site
          ),
          location_y = case_when(
            !is.na(location_y_spl) ~ location_y_spl,
            TRUE ~ location_y_site
          ),
          location_srid = case_when(
            !is.na(location_srid_spl) ~ location_srid_spl,
            TRUE ~ location_srid_site
          )
        ) |>
        # Remove unecessary columns
        select(
          -any_of(
            c(
              "location_x_site", "location_y_site", "location_srid_site",
              "sa_sitevisit_id",
              "sa_sample_id", "sa_laboratorysample_id", "sample_identifier", "sample_identifier_alt",
              "location_x_spl", "location_y_spl", "location_srid_spl"
            )
          )
        )

    } else {

      # Else we just stick to visit-level coords
      res <- df_coords_visit

    }

    # Here we join the sites/subsites list with the visit_dates/coords
    res <- tbl_sites |>
      right_join(
        res,
        by = join_by(subsite_id, site_id)
      )

  } else {
    # If this is not a monitored dataset, we don't support sample-level coords

    res <- tbl(con, "sa_site") |>
      select(
        # Site identification
        site_id = sa_site_id,
        site_identifier = identifier,
        site_identifier_alt = identifier_alt,
        subsite_id = any_of("sc_sub_site_identifier"),

        # Spatial coordinates
        location_x = location_geometry_x,
        location_y = location_geometry_y,
        location_srid = location_geometry_srid
      )
  }

  # Remove any potential duplicate rows
  res <- res |>
    distinct() |>
    collect()

  return(res)
}

#' @title Get site metadata
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select
#' @keywords internal
#' @noRd
.getSiteMetaTbl <- function(con) {

  if (.isMonitoring(con)) {

    # This is a table of site/subsite IDS
    tbl_site_ids <- .getSubsites(con)

    # If it's a monitoring dataset, we need to get the site_id
    # from the sa_visit_data table
    res <- tbl(con, "sa_visit_data") |>
      collect() |>
      # Adding subsites
      left_join(
        tbl_site_ids,
        join_by(sa_sitevisit_id)
      ) |>
      select(
        # Dataset
        dataset_id = dm_monitoringdataset_id,

        # Visit identification
        sa_sitevisit_id,

        # Site identification
        site_id = sa_monitoringsite_id ,
        site_identifier = site_identifier,
        site_identifier_alt = site_identifier_alt,
        subsite_id,

        # Spatial coordinates
        # location_x = location_x,
        # location_y = location_y,
        # location_srid = location_srid,

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
        sa_sitevisit_id = sa_site_id,
        site_identifier = identifier,
        site_identifier_alt = identifier_alt,

        # Spatial coordinates
        # location_x = location_geometry_x,
        # location_y = location_geometry_y,
        # location_srid = location_geometry_srid,

        # Altitude
        # altitude_val = location_altitude_val,
        # altitude_uom = location_altitude_uom,

        # Other metadata
        visit_date = date,
        # site_type = type,
        authority = authority#,
        # sampler
      ) |>
      collect()
  }

  # Add spatial coordinates
  tbl_coords <- .getCoords(con)

  res <- res |>
    left_join(
      tbl_coords,
      by = intersect(names(tbl_coords), names(res))
    )

  return(res)
}

#' @title Get landscape position data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select
#' @keywords internal
#' @noRd
.getLandscapeTbl <- function(con) {

  tbl_ls <- tbl(con, "lc_landscape")

  if (.isMonitoring(con)) {
    res <- tbl_ls |>
      select(
        sa_sitevisit_id,
        slopeangle_val,
        slopeaspect_val
      ) |>
      distinct() |>
      group_by(sa_sitevisit_id) |>
      summarise(
        slopeangle_val = mean(slopeangle_val, na.rm = TRUE),
        slopeaspect_val = mean(slopeaspect_val, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    res <- tbl_ls |>
      select(
        site_id = sa_site_id,
        slopeangle_val,
        slopeaspect_val
      ) |>
      distinct() |>
      group_by(site_id) |>
      summarise(
        slopeangle_val = mean(slopeangle_val, na.rm = TRUE),
        slopeaspect_val = mean(slopeaspect_val, na.rm = TRUE),
        .groups = "drop"
      )
  }

  res <- collect(res)

  return(res)
}

#' @title Get sample notes data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select
#' @keywords internal
#' @noRd
.getSplNotesTbl <- function(con) {

  res <- tbl(con, "sa_sample_note")

  if ("sa_laboratorysample_id" %in% colnames(res)) {
    res <- res |>
      select(
        sa_laboratorysample_id,
        sample_note = note
      )
  } else if ("sa_sample_id" %in% colnames(res)) {
    res <- res |>
      select(
        sa_sample_id,
        sample_note = note
      )
  } else {
    stop(
      cat("No sample ID columns found in table `sa_sample_note` for file:\n ", con@dbname),
      call. = FALSE
    )
  }

  return(res)
}

#' @title Get landuse notes data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select
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
#' @importFrom dplyr select left_join everything across
#' @keywords internal
#' @noRd
.getLanduseTbl <- function(con) {

  if (.isMonitoring(con)) {

    tbl_visit <-  tbl(con, "sa_sitevisit")

    tbl_lu <- tbl(con, "lc_landuse") |>
      left_join(
        tbl_visit,
        by = join_by(sa_sitevisit_id)
      ) |>
      collect()

    res <- tbl_lu |>
      select(
        sa_sitevisit_id,
        site_id = sa_monitoringsite_id,
        lc_landuse_id
      )

  } else {

    tbl_lu <- tbl_lu <- tbl(con, "lc_landuse") |>
      collect()

    res <- tbl_lu |>
      select(
        site_id = sa_site_id,
        lc_landuse_id
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
  tbl_lu_notes <- .getLanduseNotesTbl(con) |>
    collect()

  res <- res |>
    left_join(
      tbl_lu_notes,
      by = join_by(lc_landuse_id)
    ) |>
    select(-lc_landuse_id)

  # Remove and concatenate additional observations
  if (.isMonitoring(con)) {
    res <- res |>
      group_by(sa_sitevisit_id, site_id) |>
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
  } else {
    res <- res |>
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
  }



  return(res)
}


#' @title Get site data
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select starts_with any_of contains distinct collect tbl relocate bind_cols bind_rows inner_join left_join
#' @importFrom lubridate ymd
#' @keywords internal
#' @noRd
.getSiteTbl <- function(con) {

  if (.isMonitoring(con)) {

    site_df <- .getSiteMetaTbl(con) |>

      # Add dataset information
      inner_join(
        .getDatasetTbl(con), # not depending on sampling date
        by = join_by(dataset_id)
      ) |>

      # Add pedological information
      left_join(
        .getSoilTbl(con),
        by = join_by(sa_sitevisit_id, site_id, visit_date)
      ) |>

      # Run request as we've had to also run the landuse data request
      collect() |>

      # Add landuse and management information
      left_join(
        .getLanduseTbl(con),
        by = join_by(sa_sitevisit_id, site_id)
      ) |>

      # Add landscape position information
      left_join(
        .getLandscapeTbl(con),
        by = join_by(sa_sitevisit_id)
      ) |>
      # Remove duplicate rows
      distinct()

  } else {

    site_df <- .getSiteMetaTbl(con) |>

      # Add dataset information
      inner_join(
        .getDatasetTbl(con), # not depending on sampling date
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
  }

  # # This averages site locations where more than one location is given
  # # (typically, where 2 pits have been dug)
  # site_df_avg <- lapply(
  #   unique(site_df$site_id),
  #   function(sid) {
  #
  #     # Select current site data
  #     cur_df <- site_df |>
  #       filter(
  #         site_id == sid
  #       )
  #
  #     # If there's more than one row of data, we need to average it before returning the site data
  #     if (nrow(cur_df) > 1) {
  #
  #       nms <- names(cur_df)
  #       l_cols <- lapply(
  #         nms,
  #         function(nm) {
  #
  #             # Average the location
  #           if (nm %in% c("location_x", "location_y")) {
  #             res <- mean(cur_df[[nm]], na.rm = TRUE)
  #           } else if (nm == "visit_date") {
  #             # Take the latest sampling date
  #             dates <- unique(ymd(cur_df$visit_date))
  #
  #             if (length(dates) > 1) {
  #               if (diff(dates) > 30) {
  #                 print(
  #                   paste0(cur_df$site_id, " : ", diff(dates))
  #                 )
  #               }
  #             }
  #
  #             res <- as.character(max(dates))
  #           } else {
  #             # Any other attrbutes just take the unique value or collapse with a comma
  #             res <- .unique(cur_df[[nm]])
  #           }
  #
  #           return(res)
  #         }
  #       )
  #       names(l_cols) <- nms
  #       res <- bind_cols(l_cols)
  #     } else {
  #     # Otherwise we just return the site data as that one row
  #       res <- cur_df
  #     }
  #
  #   return(res)
  #   }
  # ) |>
  #   bind_rows()
  #
  # ADD SUBSITE_ID
  # res <- site_df_avg |>
  #   left_join(
  #     .getSubsites(con),
  #     by = join_by(site_id),
  #     relationship = "one-to-many"
  #   ) |>
  #   relocate(
  #     subsite_id,
  #     .after = site_id
  #   )

  # Catch for cases where columns are not available -- we then suppose the data isn't there
  # and initiate these as NA
  res <- .add_cols(
    site_df,
    "subsite_id"
  )

  return(res)
}

#' @title Get sample support data (ids, depths)
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select starts_with any_of contains distinct mutate case_when collect tbl
#' @importFrom tidyr drop_na
#' @keywords internal
#' @noRd
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
        subsite_id = any_of("sc_sub_site_identifier"),
        sa_sample_id,
        sa_laboratorysample_id,
        sample_identifier = identifier,
        sample_identifier_alt = identifier_alt,
        type, type_composite, type_method,
        amt_core_diameter_cm_val = sc_amt_core_diameter_cm_val,
        n_composite = sc_n_composite,
        area_composite_samples_represent = sc_txt_area_composite_samples_represent,
        field_samplingdepth_minval,
        field_samplingdepth_maxval,
        field_samplingdepth_uom,
        lab_samplingdepth_minval,
        lab_samplingdepth_maxval,
        lab_samplingdepth_uom
      ) |>
      left_join(
        tbl(con, "sa_sitevisit")|>
          select(
            site_id = sa_monitoringsite_id,
            sa_sitevisit_id
          ),
        by = join_by(sa_sitevisit_id)
      ) |>
      # Run request
      collect() |>

      # Add subsite_id if not present
      .add_cols("subsite_id") |>

      # Harmonise lab and field depths
      mutate(

        # Convert empty strings to NA if present
        subsite_id = case_when(
          subsite_id == "" ~ NA,
          TRUE ~ subsite_id
        ),

        # Depth unit correction factor
        unit_factor = case_when(
          !is.na(lab_samplingdepth_minval) & lab_samplingdepth_uom %in% depth_units_factor ~
            depth_units_factor[lab_samplingdepth_uom],
          !is.na(field_samplingdepth_minval) & field_samplingdepth_uom %in% depth_units_factor ~
            depth_units_factor[field_samplingdepth_uom],
          TRUE ~ 1 # Defaulting to centimeters
        ),

        # Min depth
        depth_minval = case_when(
          # If the lab depth is valid
          !is.na(lab_samplingdepth_minval) ~ lab_samplingdepth_minval * unit_factor,
          # If the lab depth is invalid but the field depth is valid
          is.na(lab_samplingdepth_minval) & !is.na(field_samplingdepth_minval) ~ field_samplingdepth_minval * unit_factor,
          # if there are no valid depths
          TRUE ~ NA
        ),
        # Max depth
        depth_maxval = case_when(
          # If the lab depth is valid
          !is.na(lab_samplingdepth_maxval) ~ lab_samplingdepth_maxval * unit_factor,
          # If the lab depth is invalid but the field depth is valid
          is.na(lab_samplingdepth_maxval) & !is.na(field_samplingdepth_maxval) ~ field_samplingdepth_maxval * unit_factor,
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
        depth_minval, depth_maxval
      )

  } else {
    res <- tbl(con, "sa_sample") |>
      # Subset specific columns
      select(
        sa_sitevisit_id = "sa_site_id",
        site_id = "sa_site_id",
        subsite_id = any_of("sc_sub_site_identifier"),
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

      # Add subsite_id if not present
      .add_cols("subsite_id") |>

      # Harmonise lab and field depths
      mutate(

        # Convert empty strings to NA if present
        subsite_id = case_when(
          subsite_id == "" ~ NA,
          TRUE ~ subsite_id
        ),

        # Depth unit correction factor
        unit_factor = case_when(
          !is.na(lab_samplingdepth_minval) & lab_samplingdepth_uom %in% depth_units_factor ~
            depth_units_factor[lab_samplingdepth_uom],
          !is.na(field_samplingdepth_minval) & field_samplingdepth_uom %in% depth_units_factor ~
            depth_units_factor[field_samplingdepth_uom],
          TRUE ~ 1 # Defaulting to centimeters
        ),

        # Min depth
        depth_minval = case_when(
          # If the lab depth is valid
          !is.na(lab_samplingdepth_minval) ~ lab_samplingdepth_minval * unit_factor,
          # If the lab depth is invalid but the field depth is valid
          is.na(lab_samplingdepth_minval) & !is.na(field_samplingdepth_minval) ~ field_samplingdepth_minval * unit_factor,
          # if there are no valid depths
          TRUE ~ NA
        ),

        # Max depth
        depth_maxval = case_when(
          # If the lab depth is valid
          !is.na(lab_samplingdepth_maxval) ~ lab_samplingdepth_maxval * unit_factor,
          # If the lab depth is invalid but the field depth is valid
          is.na(lab_samplingdepth_maxval) & !is.na(field_samplingdepth_maxval) ~ field_samplingdepth_maxval * unit_factor,
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
        depth_minval, depth_maxval
      )
  }

  # Add sample notes, if any
  tbl_spl_notes <- .getSplNotesTbl(con) |>
    collect()

  if (nrow(tbl_spl_notes) > 0) {
    if ("sa_laboratorysample_id" %in% names(tbl_spl_notes)) {
      res <- res |>
        left_join(
          tbl_spl_notes,
          by = join_by(sa_laboratorysample_id)
        )
    } else if ("sa_sample_id" %in% names(tbl_spl_notes)) {
      res <- res |>
        left_join(
          tbl_spl_notes,
          by = join_by(sa_sample_id)
        )
    } else {
      res <- res |>
        left_join(
          tbl_spl_notes
        )
    }
  }

  # Catch for cases where columns are not available -- we then suppose the data isn't there
  # and initiate these as NA
  res <- .add_cols(
    res,
    c(
      "subsite_id",
      "type_composite", "type_method",
      "amt_core_diameter_cm_val", "n_composite", "area_composite_samples_represent",
      "amt_field_moist_water_content_p"
    )
  )

  return(res)
}

#' @title Get soil chemistry observations
#' @importFrom dbplyr tbl_lazy
#' @importFrom dplyr select mutate case_when filter collect tbl
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @noRd
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
#' @importFrom dplyr select mutate case_when filter collect tbl
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_detect fixed
#' @keywords internal
#' @noRd
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
      values_from = result_val,
      values_fn = function(x) {
        warning("Averaging duplicated soil physics value(s) for a specific `sa_laboratorysample_id`.", call. = FALSE)
        res <- mean(x, na.rm = TRUE)
        return(res)
      }
    )

  return(res)
}

#' @title Get soil sample metadata
#' @importFrom dplyr group_by summarise across any_of
#' @keywords internal
#' @noRd
.getSplMetaTbl <- function(tbl_sa_spl) {

  res <- tbl_sa_spl |>

    select(
      sa_sitevisit_id,
      site_id, subsite_id,
      depth_minval, depth_maxval,
      type, type_method, type_composite,
      n_composite, area_composite_samples_represent
    ) |>
    distinct() |>
    mutate(
      type = case_when(
        type == "unknown" ~ NA,
        TRUE ~ type
      )
    )

  return(res)
}

#' @title Assemble soil chemistry data
#' @importFrom dplyr left_join join_by starts_with contains any_of group_by summarise across select mutate where
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @noRd
.getChem <- function(tbl_sa_spl, tbl_ob_obs_chem) {

  res <- tbl_sa_spl |>
    left_join(
      tbl_ob_obs_chem,
      by = join_by(sa_laboratorysample_id == sa_laboratorysample_id)
    ) |>

    # Aggregate at the site/subsite level
    # - remove sample IDs
    select(
      # -contains("sample"),
      # These are now handled in the dedicated .getSplMetaTbl function
      -type, -type_method, -type_composite,
      - n_composite, -area_composite_samples_represent
    ) |>
    # - group_by site, subiste, depths
    group_by(
      across(
        any_of(
          c("sa_sitevisit_id", "site_id", "subsite_id", "depth_minval", "depth_maxval")
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
#' @importFrom dplyr left_join join_by starts_with contains any_of group_by summarise across select mutate where
#' @importFrom tidyr pivot_wider
#' @keywords internal
#' @noRd
.getPhys  <- function(tbl_sa_spl, tbl_ob_obs_phys) {

  res <- tbl_sa_spl |>
    left_join(
      tbl_ob_obs_phys,
      by = join_by(sa_laboratorysample_id == sa_laboratorysample_id)
    ) |>

    # Aggregate at the site/subsite level
    # - remove sample IDs
    select(
      # -contains("sample")
      # -sa_sitevisit_id,
      -sa_sample_id, -sa_laboratorysample_id,
      -sample_identifier, -sample_identifier_alt,
      -unit_factor, -depth_uom,
      # These are now handled in the dedicated .getSplMetaTbl function
      -type, -type_method, -type_composite,
      - n_composite, -area_composite_samples_represent
    ) |>

    # - group_by site, subiste, depths
    group_by(
      across(
        any_of(
          c("sa_sitevisit_id", "site_id", "subsite_id", "depth_minval", "depth_maxval")
        )
      )
    ) |>
    summarise(
      # across(
      #   starts_with("amt_"),
      #   \(x) mean(x, na.rm = TRUE)),

      # Quantitative variables
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      across(where(is.character), ~ .unique(.x)),

      .groups = "drop"
    )

  return(res)
}

#' @title Aggregate soil physics data on the soil chem depth intervals
#' @importFrom dplyr filter summarise across where bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom pbapply pblapply
#' @importFrom stats as.formula na.omit weighted.mean
#' @importFrom aqp depths `depths<-` dice
#' @keywords internal
#' @noRd
.aggregateSoilPhys <- function(tbl_site, tbl_chem, tbl_phys) {
  # For each site in the dataset
  #   - we pull the chem depths
  #   - we pull the phys depths
  #   - we match phys depths to chem depths

  # Pull all the unique site IDs of the dataset
  sids_df <- tbl_site |>
    select(sa_sitevisit_id, site_id, subsite_id) |>
    distinct()

  l_res <- pbapply::pblapply(
      1:nrow(sids_df),
      function(i) {

        visitid <- sids_df$sa_sitevisit_id[i]
        sid <- sids_df$site_id[i]
        subid <- sids_df$subsite_id[i]

        if (is.na(subid)) {
          chem <- tbl_chem |> filter(sa_sitevisit_id == visitid & site_id == sid & is.na(subsite_id))
          phys <- tbl_phys |> filter(sa_sitevisit_id == visitid & site_id == sid & is.na(subsite_id))
        } else {
          chem <- tbl_chem |> filter(sa_sitevisit_id == visitid & site_id == sid & subsite_id == subid)
          phys <- tbl_phys |> filter(sa_sitevisit_id == visitid & site_id == sid & subsite_id == subid)
        }

        # Removing zero-thicness horizons
        phys <- phys |>
          mutate(
            thickness = depth_maxval - depth_minval
          ) |>
            filter(
              thickness > 0
            ) |>
            select(
              -thickness
            )
        chem <- chem |>
          mutate(
            thickness = depth_maxval - depth_minval
          ) |>
          filter(
            thickness > 0
          ) |>
          select(
            -thickness
          )

        # That's if there no soil physics OR no soil chem data at all
        if (nrow(phys) == 0 | nrow(chem) == 0) {
          return(NULL)
        }

        # Convert soil physics data.frame to a SoilProfileCollection
        phys_sdf <- phys
        depths(phys_sdf) <- site_id ~ depth_minval + depth_maxval

        # Create diced soil profile
        #

        # A requirement of aqp::dice is that depths must be >= 0
        min_depth <- max(0, min(chem$depth_minval))
        max_depth <- max(0, max(chem$depth_maxval))

        phys_dc <- dice(
          phys_sdf,
          fm = as.formula(paste0(min_depth, ":", max_depth, " ~ .")),
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
              filter(
                depth_minval >= min_depth & depth_maxval <= max_depth
              ) |>
              select(
                -site_id, -subsite_id,
                -depth_minval, -depth_maxval
              ) |>
              summarise(
                # across(
                #   starts_with("amt_"),
                #     .weighted_mean_count
                # ),

                # Quantitative variables
                across(where(is.numeric), ~ .weighted_mean_count(.x)),
                # Character variables
                across(where(is.character), ~ .mode(.x)),

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
#' @param view Name of the SQLite View to load. Defaults to "MfE_Carbon_data". Part of the legacy system mentioned below.
#' @param legacy Boolean, default to FALSE This is the old system relying purely
#'  on Views. It is kept temporarily for testing purposes.
#'
#' @returns a data.frame
#'
#' @author Pierre Roudier
#'
#' @importFrom RSQLite SQLite dbConnect dbListTables dbReadTable dbDisconnect
#' @importFrom dplyr right_join join_by
#' @export
cms_read <- function(fn, view = "MfE_Carbon_data", legacy = FALSE) {

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

  # Get sample support tbl
  tbl_sa_spl <- .getSampleSupport(con)

  # Get site tbl
  tbl_site <- .getSiteTbl(con)

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

  # Get the sample metadata (type of composite etc)
  tbl_md <- .getSplMetaTbl(tbl_sa_spl)

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
      # by = join_by(site_id, subsite_id)
      by = join_by(sa_sitevisit_id, site_id, subsite_id)
    ) |>
    left_join(
      tbl_md,
      # by = join_by(site_id, subsite_id, depth_minval, depth_maxval)
      by = join_by(sa_sitevisit_id, site_id, subsite_id, depth_minval, depth_maxval)
    )

  return(res)
}

# fn1 = "/mnt/c/Users/RoudierP/OneDrive - MWLR/MFE_CARBON/soilcms-data/data/NSDR_Export_nscm_20250820.db"
# fn2 = "/mnt/c/Users/RoudierP/OneDrive - MWLR/MFE_CARBON/soilcms-data/data/NSDR_Export_sustain_20250825.db"

# fn1 = "/Users/pierreroudier/OneDrive - MWLR/MFE_CARBON/soilcms-data/data/NSDR_Export_nscm_20250820.db"
# fn2 = "/Users/pierreroudier//OneDrive - MWLR/MFE_CARBON/soilcms-data/data/NSDR_Export_sustain_20250825.db"
# fn3 = "/Users/pierreroudier//OneDrive - MWLR/MFE_CARBON/soilcms-data/data/NSDR_Export_MfE Soil CMS_20250814.db"
fn4 = "/Users/pierreroudier//OneDrive - MWLR/MFE_CARBON/soilcms-data/data/NSDR_Export_nsd_20240523.db"

# con1  = dbConnect(RSQLite::SQLite(), fn1)
# con2  = dbConnect(RSQLite::SQLite(), fn2)
# con3 = dbConnect(RSQLite::SQLite(), fn3)
