# 0 . Load libraries -----

library(tidyverse)
library(lubridate)
library(skimr)
library(janitor)
library(ggplot2)

# 1. Load Data ---------------------------------------------------------------

products <- read_csv("./data/products.csv") |> 
  clean_names()
locations <- read_csv("./data/locations.csv") |> 
  clean_names()
transactions <- read_csv("./data/transactions.csv") |> 
  clean_names()

locations %>%
  count(region_type ) %>%
  arrange(desc(n))

ggplot(locations, aes(x = region_type)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Transactions per Region", x = "Region", y = "Count")

transactions %>%
  summarise(
    total_tx    = n(),
    first_date  = min(date),
    last_date   = max(date),
    unique_prods= n_distinct(sku),
    unique_locs = n_distinct(location_id)
  )

 ggplot(transactions, aes(x = ymd(date))) +
    geom_histogram(bins = 50) +
    labs(title = "Transactions Over Time", x = "Date", y = "Number of Transactions")

  tx_full <- transactions %>%
    left_join(products,  by = "sku") %>%
    left_join(locations, by = "location_id")

  top_prods <- tx_full %>%
    group_by(sku) %>%
    summarise(sales_qty = sum(quantity, na.rm = TRUE)) %>%
    arrange(desc(sales_qty)) %>%
    slice_head(n = 10)

  ggplot(top_prods, aes(x = reorder(sku, sales_qty), y = sales_qty)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top 10 Products by Quantity Sold", x = "", y = "Total Quantity")
