#'
#'
#'
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


# define a data table of vaccination rates from 50% to 90%
p <- tribble(
    ~air_second_dose_pct,
    30,
    40,
    50,
    60,
    70,
    80,
    90
)

# nest
air_nested_tbl <- air_tidy_tbl %>%
    mutate(
        date_as_at_epoch = as.integer(as.POSIXct(date_as_at))
    ) %>%
    select(abs_name, date_as_at_epoch, air_second_dose_pct) %>%
    group_by(abs_name) %>%
    nest()

air_nested_model_tbl <- air_nested_tbl %>%
    mutate(
        model = map(data, function(df) lm(date_as_at_epoch ~ air_second_dose_pct, data = df))
    ) 

predicted_dates_tbl <- air_nested_model_tbl %>%
    mutate(
        prediction = map(.x = model, ~ predict(., p)), 
        predictors = map(abs_name,  ~ bind_cols(p)),
        regression_metrics = map(.x = model, ~glance(.))
    ) 

predicted_dates_tidy_tbl <- predicted_dates_tbl %>%
    select(abs_name, prediction, predictors) %>%
    unnest() %>%
    mutate(
        date_as_at = round_date(as_datetime(prediction), "day")
    ) %>%
    select(-prediction) %>%
    mutate(
        type = "Predicted"
    )


combined_tbl <- air_tidy_tbl %>%
    select(date_as_at, abs_name, air_second_dose_pct) %>%
    mutate(
        type = "Actual"
    ) %>%
    bind_rows(
        predicted_dates_tidy_tbl
    ) %>%
    arrange(
        abs_name, date_as_at
    )

# visualise ----------------------

forecast_lga_vax_rates_by_date_plot <- combined_tbl %>%
    filter(abs_name != "Grant (DC)") %>%
    ggplot(aes(date_as_at, air_second_dose_pct)) +
    geom_line(aes(colour=type)) +
    geom_point(aes(colour=type)) +
    geom_hline(yintercept = 50, alpha=0.8, linetype="dashed") +
    geom_hline(yintercept = 70,alpha=0.8, linetype="dashed") +
    geom_hline(yintercept = 80, alpha=0.8, linetype="dashed") +
    geom_vline(xintercept=as.numeric(as.Date("2021-11-23"))) +
    facet_wrap(vars(abs_name)) +
    theme_clean() + 
    scale_colour_tableau() + # from ggthemes
    labs(
        title = "Forecast and Actual growth rate of second Covid vaccination % by SA LGA",
        caption =  plot_footer,
        y = "Second Dose %",
        x = "Date",
        colour = "Data Type"
    )

# save plot to plots directory
ggsave(forecast_lga_vax_rates_by_date_plot, filename = "plots/forecast_lga_vax_rates_by_date.png", width=297 * 2, height=210 * 2, units = "mm")

# forecast lga vax rates by date line chart plot
forecast_lga_vax_rates_by_date_linechart_plot <- combined_tbl %>%
    filter(abs_name != "Grant (DC)") %>%
    ggplot(aes(abs_name, date_as_at)) +
    geom_line() +
    geom_label(aes(label=air_second_dose_pct, fill = type), size=3) +
    theme_clean() + 
    scale_fill_tableau() + # from ggthemes
    geom_hline(yintercept=as.numeric(as.Date("2021-11-23")), linetype="dashed") +
    coord_flip() +
    labs(
        title = "Forecast and Actual expected rates for second Covid vaccination % by SA LGA",
        caption =  plot_footer,
        y = "Date",
        x = "",
        fill = "Type"
    )

# save to disc
ggsave(
    forecast_lga_vax_rates_by_date_linechart_plot, 
    filename = "plots/forecast_lga_vax_rates_by_date_linechart_plot.png", 
    width=210, 
    height=297, 
    units = "mm"
)
