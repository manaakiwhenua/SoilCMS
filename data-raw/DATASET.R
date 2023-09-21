config_sampling_site <- openxlsx::read.xlsx("data-raw/Soil carbon template. MfE CMS project.xlsx", sheet = "Sampling site details")
config_sample <- openxlsx::read.xlsx("data-raw/Soil carbon template. MfE CMS project.xlsx", sheet = "Sample data")
config_landuse <- openxlsx::read.xlsx("data-raw/Soil carbon template. MfE CMS project.xlsx", sheet = "Landuse details")

df_config <- data.frame(
  short_heading = c(
    names(config_sampling_site),
    names(config_sample),
    names(config_landuse)
  ),
  long_heading = c(
    as.character(config_sampling_site[1,]),
    as.character(config_sample[1,]),
    as.character(config_landuse[1,])
  )
)

# df_config <- openxlsx::read.xlsx("data-raw/Soil_carbon_template.config.xlsx", sheet = "Columns")
# names_config <- data.frame(
#   short_heading = df_config[,1],
#   long_heading = df_config[,2]
# )
names_config$short_heading <- stringr::str_replace(names_config$short_heading, ">", "")
names_config$short_heading <- stringr::str_replace(names_config$short_heading, "<", "")

usethis::use_data(names_config, internal = TRUE, overwrite = TRUE)
