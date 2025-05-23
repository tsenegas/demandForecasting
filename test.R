# 0) Packages
library(tidyverse)   # dplyr, tidyr, purrr, readr…
library(tsibble)     # as_tsibble(), fill_gaps()
library(lubridate)   # yearmonth(), %m-% 
library(prophet)     # prophet(), make_future_dataframe(), predict()

# 1) Load & re-create your tsibble
df_monthly <- read_csv("./data/df_monthly.csv") %>%
  select(month, Location = `Location ID`, SKU, quantity) %>%
  group_by(month, Location, SKU) %>%
  summarise(quantity = sum(quantity), .groups = "drop") %>%
  mutate(month = yearmonth(month)) %>%
  as_tsibble(key = c(Location, SKU), index = month) %>%
  fill_gaps(quantity = 0L)

# 2) Ungroup & turn into prophet‐friendly df
df_prop <- df_monthly %>%
  as_tibble() %>% 
  mutate(
    ds = as.Date(month),   # Prophet wants a Date column called ds
    y  = quantity
  ) %>%
  select(ds, y, Location, SKU)

# 3) Define your hold‐out: last 6 months as test
H <- 6
max_ds   <- max(df_prop$ds)
cutoff   <- max_ds %m-% months(H)

train_df <- df_prop %>% filter(ds <= cutoff)
test_df  <- df_prop %>% filter(ds  > cutoff)

# 4) Nest training data by series
nested <- train_df %>%
  group_by(Location, SKU) %>%
  nest()

# 5) Fit & forecast function
fit_and_fc <- function(df, periods = H) {
  m <- prophet(df, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)
  future <- make_future_dataframe(m, periods = periods, freq = "month")
  pred   <- predict(m, future)
  pred %>% select(ds, yhat) %>% tail(periods)
}

# 6) Apply to every series
library(purrr)
results <- nested %>%
  mutate(
    fc = map(data,  ~ fit_and_fc(.x, periods = H))
  ) %>%
  select(-data) %>%
  unnest(fc)

# 7) Join with actuals & compute metrics
library(rlang)
evals <- results %>%
  left_join(test_df, by = c("Location","SKU","ds")) %>%
  group_by(Location, SKU) %>%
  summarise(
    RMSE = sqrt(mean((y - yhat)^2, na.rm=TRUE)),
    MAE  = mean(abs(y - yhat),       na.rm=TRUE),
    MAPE = mean(abs((y - yhat)/y)*100, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(RMSE))

# 8) Inspect
print(evals)
# Optionally: view the worst & best series
evals %>% slice_min(RMSE, n=5)
evals %>% slice_max(RMSE, n=5)
