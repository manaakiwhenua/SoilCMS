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
