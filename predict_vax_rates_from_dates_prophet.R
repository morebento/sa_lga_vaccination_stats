#'
#' predict_vax_rates_from_dates_prophet.R
#'
#' uses prophet library to predict vax rates for a range of dates 
#'
#'

# first some libraries
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggrepel)
library(broom)
library(scales)
library(lubridate)
library(readxl)
library(prophet)

# read the data from csv and clean the names
air_tbl <- read_csv("https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv") %>% 
    clean_names()


air_tidy_tbl <- air_tbl %>%
    filter(state == "SA") %>%
    select(date_as_at, state, abs_name, air_first_dose_pct:abs_erp_2019_population) %>%
    drop_na(air_second_dose_pct)


# get the latest date
max_date <- air_tidy_tbl %>%
    slice_max(date_as_at) %>%
    summarise(
        max_date_as_at = max(date_as_at)
    ) %>%
    pull(max_date_as_at)


# create a standard caption footer
plot_footer <- stringr::str_glue("Latest Data: {max_date} Author: @morebento. Code: https://github.com/morebento/sa_lga_vaccination_stats")


# make sure in format required by prophet (ds, y) and nest
# returns a nested data frame with column data as a list of tibbles
air_nested_prophet_tbl <- air_tidy_tbl %>%
    select(abs_name, date_as_at, air_second_dose_pct) %>%
    rename(ds = date_as_at, y = air_second_dose_pct) %>% 
    group_by(abs_name) %>%
    nest()


# function to train the model on the input set, build a set of future date and forecast those dates
# before returning the combined actual and forecasts
predict_with_prophet <- function(data_tbl) {
    
    # train the model
    prophet_model <- prophet(data_tbl)
    
    # generate future dates for 90 days
    future_tbl <- make_future_dataframe(prophet_model, periods = 90)
    
    # forecast to these dates
    forecast_tbl <- predict(prophet_model, future_tbl)
    
    # join together
    combined_tbl <- forecast_tbl %>%
        left_join(data_tbl) %>% 
        as_tibble()
    
    return(combined_tbl)
}


# do the prediction
air_nested_predictions_tbl <- air_nested_prophet_tbl %>%
    mutate(
        predictions = map(data, predict_with_prophet)
    ) 

# unnest
air_predictions_tbl <- air_nested_predictions_tbl %>%
    select(abs_name, predictions) %>%
    unnest() %>%
    ungroup()


# get the value of opening day
opening_day <- as_date(as.POSIXct("2021-11-23"))


# get population data
lga_pop_tbl <- read_csv("https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv") %>% 
    clean_names() %>%
    filter(state == "SA") %>%
    slice_max(date_as_at) %>%
    select(abs_name, abs_erp_2019_population) 

# visualisation ---------------------------


air_predictions_tbl %>%
    inner_join(lga_pop_tbl) %>%
    filter(
        abs_name != "Grant (DC)"
    ) %>%
    filter(abs_name %in% c("Playford (C)", "Unley (C)", "Burnside (C)", "Adelaide (C)")) %>%
    filter(yhat <= 100) %>%
    ggplot(aes(x=ds)) +
    geom_line(aes(y=yhat, colour="Predicted")) +
    geom_point(aes(y=y, colour="Actual"), size=0.9) +
    geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill="Confidence"), alpha=0.5) +
    geom_hline(yintercept=80, linetype="dashed") +
    geom_hline(yintercept = 90, linetype = "dashed") +
    geom_vline(xintercept = opening_day, linetype = "dotted") +
    theme_clean() + 
    scale_colour_tableau() + 
    scale_fill_tableau() +
    facet_wrap(vars(abs_name)) +
    labs(
        title = "SA 16+ vaccination % vs Time",
        colour = "Dose",
        x = "",
        y = "%"
    )

