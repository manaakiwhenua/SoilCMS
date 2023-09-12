df_config <- openxlsx::read.xlsx("data-raw/Soil_carbon_template.config.xlsx", sheet = "Columns")
names_config <- data.frame(
  short_heading = df_config[,1],
  long_heading = df_config[,2]
)
names_config$short_heading <- stringr::str_replace(names_config$short_heading, ">", "_")
names_config$short_heading <- stringr::str_replace(names_config$short_heading, "<", "_")

usethis::use_data(names_config, internal = TRUE)
