# set-up -----------------------------------------------------------------
library(readr)
library(tidyr)

# extract ----------------------------------------------------------------
# access date: 11/5/2026
wdi_raw <- read_csv(
  "https://data360files.worldbank.org/data360-data/data/WB_WDI/WB_WDI_SI_DST_FRST_20_WIDEF.csv"
)

# transform --------------------------------------------------------------
wdi_first_20 <- wdi_raw |>
  select(
    country_code = REF_AREA,
    `1963`:`2025`
  ) |>
  pivot_longer(
    cols = -country_code,
    names_to = "year",
    values_to = "share_income_lowest_20"
  )

# load -------------------------------------------------------------------
wdi_first_20 |>
  write_csv(
    "data/output/wdi_first_20.csv"
  )