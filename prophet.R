library(tidyverse)       # data wrangling
library(tsibble)         # time-series tibbles
library(lubridate)       # date handling
library(timetk)          # ts train/test split
library(modeltime)       # time-series modeling
library(prophet)         # Prophet engine
library(tidymodels)      # recipes, parsnip, workflows

# 2. Read & prepare the monthly panel
df_raw <- read_csv("data/df_monthly.csv")

df_ts <- df_raw |> 
  rename(Location = `Location ID`) |>      #
  mutate(month = yearmonth(month)) |> 
  select(month, Location, SKU, quantity) |> 
  group_by(month, Location, SKU) |> 
  mutate(
    quantity = sum(quantity)
  ) |> 
  ungroup() |> 
  unique() |> 
  as_tsibble(key = c(Location, SKU), index = month) %>%
  fill_gaps(quantity = 0L) %>%              
  mutate(
    ds = as.Date(month),                    
    y  = quantity                           
  ) %>%
  select(Location, SKU, ds, y)              

# 3. Create nested train/test splits (hold out last 12 months)
nested_tbl <- df_ts %>%
  group_by(Location, SKU) %>%
  nest()
nested_tbl = nested_tbl[-29,]

nested_splits <- nested_tbl %>%
  mutate(
    split = map(
      data,
      ~ .x %>% time_series_split(assess = 12, cumulative = TRUE)
    ),
    train = map(split, training),
    test  = map(split, testing)
  )


# 4. Define Prophet spec & fit per series
prophet_spec <- prophet_reg() %>%
  set_engine("prophet") %>%
  set_mode("regression")

nested_models <- nested_splits %>%
  mutate(
    model = map(train, ~ fit(prophet_spec, y ~ ds, data = .x))
  )


# 5. Build a Modeltime table, calibrate & compute accuracy
results_tbl <- nested_models %>%
  mutate(
    model_table = map(model, ~ modeltime_table(.x)),
    calib       = map2(model_table, test, ~ calibrate(.x, .y)),
    accuracy    = map2(calib, test, ~ accuracy(.x, .y))
  ) %>%
  select(Location, SKU, accuracy) %>%
  unnest(accuracy)

# View top‐lines of accuracy
results_tbl %>%
  arrange(Location, SKU, .metric) %>%
  print(n = 10)


