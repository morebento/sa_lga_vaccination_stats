#' predict_sa_rates_prophet.R
#'
#' uses the prophet library to predict the SA vaccination rates for 16+ population
#'
#' code here https://github.com/morebento/sa_lga_vaccination_stats
#'
#' ben moretti
#' november 2021
#'

# libraries -----------------------------------------------

library(tidyverse)
library(janitor)
library(ggthemes)
library(ggrepel)
library(prophet)

# gather data  -----------------------------------------------

# read data from the site 
data_tbl <- read_csv("https://vaccinedata.covid19nearme.com.au/data/air.csv") %>% clean_names()


# conditioning -----------------------------------------------

# get the latest date
max_date <- data_tbl %>%
    slice_max(date_as_at) %>%
    summarise(
        max_date_as_at = max(date_as_at)
    ) %>%
    pull(max_date_as_at)


# get all sa data
sa_data_tbl <- data_tbl %>%
    select(
        date_as_at,
        starts_with("air_sa_"),
        validated, url
    ) 

sa_data_tbl %>% glimpse()

# get the data into the format prophet uses
data_prophet_tbl <- sa_data_tbl %>%
    select(date_as_at,  air_sa_16_plus_second_dose_pct) %>%
    rename(
        ds = date_as_at,
        y = air_sa_16_plus_second_dose_pct
    ) %>%
    drop_na(y)

# modelling --------------------------------------------------------------

# train the model
prophet_model <- prophet(data_prophet_tbl)

# generate future dates
future_tbl <- make_future_dataframe(prophet_model, periods = 90)

# forecast to these dates
forecast_tbl <- predict(prophet_model, future_tbl)

# join back to the actual data
combined_tbl <- forecast_tbl %>%
    left_join(data_prophet_tbl) %>% 
    as_tibble()

# visualisation --------------------------------------------------

# get the date at 80%
date_at_80 <- combined_tbl %>%
    filter(
        between(yhat, 80, 81)
    ) %>%
    slice_min(yhat) %>%
    pull(ds) 

# get the date at 90%
date_at_90 <- combined_tbl %>%
    filter(
        between(yhat, 90, 91)
    ) %>%
    slice_min(yhat) %>%
    pull(ds) 

# build a data table of values for labelling
labels_tbl <- tribble(
    ~ds, ~y,
    date_at_80, 80,
    date_at_90, 90
)

labels_tbl <- labels_tbl %>%
    mutate(
        ds_label = as.character(format(ds, "%d %b"))
    )

# create a standard caption footer
plot_footer <- stringr::str_glue("Latest Data: {max_date} Author: @morebento. Code: https://github.com/morebento/sa_lga_vaccination_stats")

# create the actual and forecast plot
actual_forecast_plot <- combined_tbl %>%
    filter(yhat <= 100) %>%
    ggplot(aes(x=ds)) +
    geom_line(aes(y=yhat, colour="Predicted")) +
    geom_point(aes(y=y, colour="Actual"), size=0.9) +
    geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill="Confidence"), alpha=0.5) +
    geom_hline(yintercept=80, linetype="dashed") +
    geom_hline(yintercept = 90, linetype = "dashed") +
    geom_label_repel(data = labels_tbl,
                     aes(x=ds, y=y, label=ds_label)) +
    theme_clean() + 
    scale_colour_tableau() + # from ggthemes
    scale_fill_tableau() +
    labs(
        title = "Predicted dates for SA 16+ second dose vaccination at 80% and 90%",
        caption = plot_footer,
        colour = "Dose",
        fill = "Statistics",
        x = "",
        y = "%"
    )

# save to disc
ggsave(
    plot=actual_forecast_plot, 
    filename = "plots/forecast_sa_vax_dates_prophet.png", 
    width=297, 
    height=210, 
    units = "mm"
)


# save to disc
ggsave(
    plot=actual_forecast_plot, 
    filename = "plots/forecast_sa_vax_dates_prophet.pdf", 
    width=297, 
    height=210, 
    units = "mm"
)
