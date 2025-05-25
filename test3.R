# 0. Packages
library(tidyverse)
library(lubridate)
library(modeltime)   # forecasting bridges tidymodels + prophet + ARIMA + ETS
library(timetk)      # time‐series toolkit
library(rsample)     # for resampling
library(parsnip)     # model specification
library(recipes)     # feature engineering
library(workflows)   # bundling recipe + model
library(yardstick)   # metrics
library(tsibble) 

# 1) Read & prep
df_monthly <- read_csv("./data/df_monthly.csv") |>
  select(month, Location = `Location ID`, SKU, quantity) |>
  group_by(month, Location, SKU) |>
  summarise(quantity = sum(quantity), .groups = "drop") |>
  mutate(month = yearmonth(month)) |>
  as_tsibble(key = c(Location, SKU), index = month) |>
  fill_gaps(quantity = 0L)

df <- df_monthly |>
  select(month, Location, SKU, quantity) |>
  mutate(
    ds = yearmonth(month) %>% as.Date(),  # convert to Date at month‐start
    y  = quantity
  ) |>
  select(ds, y, Location, SKU)

# 2) Nest by series
nested_tbl <- df %>%
  group_by(Location, SKU) %>%
  nest() %>%
  ungroup()

# 3) Define a function to build a modeltime table per series
make_models <- function(data_tbl) {

  # a. Split train / test (last 6 months hold-out)
  splits <- initial_time_split(data_tbl, prop = (nrow(data_tbl) - 6) / nrow(data_tbl))

  # b. Recipe (no extra features right now)
  rec <- recipe(y ~ ds, data = training(splits))
  
  # c. Model specs
  arima_spec <- arima_reg()      %>% set_engine("auto_arima")
  ets_spec   <- exp_smoothing()  %>% set_engine("ets")
  prop_spec  <- prophet_reg()    %>% set_engine("prophet")

  # d. Workflows
  wf_arima <- workflow() %>% add_model(arima_spec) %>% add_recipe(rec)
  wf_ets   <- workflow() %>% add_model(ets_spec)   %>% add_recipe(rec)
  wf_prop  <- workflow() %>% add_model(prop_spec)  %>% add_recipe(rec)

  # e. Modeltime table
  modeltime_table(
    arima = fit(wf_arima,  data = training(splits)),
    ets   = fit(wf_ets,    data = training(splits)),
    prop  = fit(wf_prop,   data = training(splits))
  ) %>%
    # f. Calibrate (generate forecasts on the test set)
    modeltime_calibrate(new_data = testing(splits)) %>%
    # g. Compute accuracy metrics
    modeltime_accuracy() %>%
    # h. Also embed the calibration object for later forecasting
    left_join(
      modeltime_table(
        arima = fit(wf_arima,  data = training(splits)),
        ets   = fit(wf_ets,    data = training(splits)),
        prop  = fit(wf_prop,   data = training(splits))
      ) %>% modeltime_calibrate(new_data = testing(splits)) %>% mutate(.model_desc = names(.model_id)),
      by = ".model_id"
    )
}

# 4) Apply to each series
results_tbl <- nested_tbl %>%
  mutate(
    metrics = map(data, make_models)
  ) %>%
  select(Location, SKU, metrics) %>%
  unnest(metrics)

# 5) Inspect accuracy
results_tbl %>%
  select(Location, SKU, .model_desc, rmse, mae, mape) %>%
  arrange(Location, SKU, rmse) %>%
  print(n = Inf)

# 6) Re-nest for final forecasting
forecast_tbl <- nested_tbl %>%
  left_join(
    # pick the best model per series by lowest RMSE
    results_tbl %>%
      group_by(Location, SKU) %>%
      slice_min(rmse, n = 1) %>%
      select(Location, SKU, .model_id),
    by = c("Location","SKU")
  ) %>%
  mutate(
    # build a single modeltime_table with the chosen engine
    model = map2(
      data, .model_id,
      ~ modeltime_table(
          arima = fit(arima_reg()   %>% set_engine("auto_arima") %>% workflow() %>% add_recipe(recipe(y~ds,data = .x)), data = .x),
          ets   = fit(exp_smoothing()%>% set_engine("ets")       %>% workflow() %>% add_recipe(recipe(y~ds,data = .x)), data = .x),
          prop  = fit(prophet_reg() %>% set_engine("prophet")   %>% workflow() %>% add_recipe(recipe(y~ds,data = .x)), data = .x)
        ) %>%
        pluck(".model_tbl", which(names(.) == .y))
    ),
    calib = map2(model, data, ~ modeltime_calibrate(.x, new_data = .y)),
    forecast = map(
      calib,
      ~ modeltime_forecast(.x,
         new_data    = future_frame(.x, .length = 6, .freq = "month"),
         actual_data = .x$data
      )
    )
  )