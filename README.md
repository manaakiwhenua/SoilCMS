
# SoilCMS

## Installation

You can install the development version of SoilCMS from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("manaakiwhenua/SoilCMS")
```

## Usage

```r
# In this example, the SQLite data downloaded from the NSDR-Viewer
# website was saved as "nsdr-data.db"
fn <- "./nsdr_data.db"

library(SoilCMS)

# Get data into a data.frame
df <- fn |> read_mfe_sqlite() |> process_mfe_data()
head(df)

# Export data to Excel
fn |> 
  read_mfe_sqlite() |> 
  process_mfe_data() |>
  write_mfe_xlsx(file = "./output.xlsx")
  
# This can be done directly as:
export_mfe(sqlite_fn = fn, xlsx_fn = "./output.xlsx", process_data = TRUE)
```
