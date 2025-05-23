# 0) Packages
library(tidyverse)    # data wrangling
library(tsibble)      # time‐series data
library(fable)        # modelling
library(fabletools)   # reconciliation

# 1) Prepare your tsibble (you already did this)
df_monthly <- read_csv("./data/df_monthly.csv") %>%
  select(month, Location = `Location ID`, SKU, quantity) %>%
  group_by(month, Location, SKU) %>%
  summarise(quantity = sum(quantity), .groups = "drop") %>%
  mutate(month = yearmonth(month)) %>%
  as_tsibble(key = c(Location, SKU), index = month) %>%
  fill_gaps(quantity = 0L)

# 1) Define the hierarchy: Location → SKU
hts <- df_monthly %>%
  aggregate_key(
    Location / SKU,
    quantity = sum(quantity)
  ) %>%
  fill_gaps(quantity = 0L)

# 2) Fit ARIMA to every leaf series
models <- hts %>%
  model(
    arima = ARIMA(quantity)
  )

# 3) Forecast 6 months ahead
raw_fc <- models %>%
  forecast(h = "6 months")

# 4) Now you can reconcile
fc <- raw_fc %>%
  reconcile(
    bu = bottom_up()
  )

# 5) Quick check
fc %>%
  filter(Location == "0001", SKU == "Alpha") %>%
  autoplot(hts)