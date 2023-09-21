config_sampling_site <- openxlsx::read.xlsx("data-raw/Soil carbon template. MfE CMS project.xlsx", sheet = "Sampling site details")
config_sample <- openxlsx::read.xlsx("data-raw/Soil carbon template. MfE CMS project.xlsx", sheet = "Sample data")
config_landuse <- openxlsx::read.xlsx("data-raw/Soil carbon template. MfE CMS project.xlsx", sheet = "Landuse details")

df_config_sampling_site <- data.frame(
  short_heading = names(config_sampling_site),
  long_heading = as.character(config_sampling_site[1,])
)
df_config_sample <- data.frame(
  short_heading = names(config_sample),
  long_heading = as.character(config_sample[1,])
)
df_config_landuse <- data.frame(
  short_heading = names(config_landuse),
  long_heading = as.character(config_landuse[1,])
)
# Custom go here
df_fixed <- data.frame(
  short_heading = c(
    "dataset",
    "dm_monitoringdataset_id",
    "sa_site_id",
    "site_identifier",
    "site_identifier_alt",
    "sa_laboratorysample_id",
    "sa_sample_id",
    'sample_identifier',
    'sample_identifier_alt',
    'laboratoryidentifier',
    'type',
    'type_composite',
    'type_method',
    "classifier_nzsc",
    "slopeangle_val",
    "slopeaspect_val",
    "depth_uom",
    "depth_minval",
    "depth_maxval",
    "depth_from",
    "laboratorydataset",
    "visit_authority",
    "location_x",
    "location_y",
    "type_method",
    "thickness",
    "fine_bulk_density",
    "carbon_stocks",
    "nitrogen stocks"
  ),
  long_heading = c(
    "Dataset",
    "Dataset UID",
    "Site UID",
    "Site ID",
    "Site ID (Alt)",
    "Lab sample UID",
    "Sample UID",
    'Sample ID',
    'Sample ID (Alt)',
    'Lab ID',
    '',
    'Composite type',
    'Sampling method',
    "NZSC Order",
    "Slope (°)",
    "Aspect (°)",
    "Depth unit of measure",
    "Sample upper depth (cm)",
    "Sample lower depth (cm)",
    "Source of depth information",
    "Laboratory dataset",
    "Sampling organisation",
    "Site X Coord (NZTM, m)",
    "Site Y Coord (NZTM, m)",
    "Sampling method",
    "Sample thickness (cm)",
    "Bulk density of <2mm per total sample volume (g/cm3)",
    "Organic carbon (Mg/ha)",
    "Total nitrogen (Mg/ha)"
  )
)

names_config <- rbind(
  df_config_sampling_site,
  df_config_sample,
  df_config_landuse,
  df_fixed
)

# df_config <- openxlsx::read.xlsx("data-raw/Soil_carbon_template.config.xlsx", sheet = "Columns")
# names_config <- data.frame(
#   short_heading = df_config[,1],
#   long_heading = df_config[,2]
# )

names_config$short_heading <- stringr::str_replace(names_config$short_heading, ">", "_")
names_config$short_heading <- stringr::str_replace(names_config$short_heading, "<", "_")

usethis::use_data(names_config, internal = TRUE, overwrite = TRUE)
