

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


# nest
air_nested_tbl <- air_tidy_tbl %>%
    mutate(
        date_as_at_epoch = as.integer(as.POSIXct(date_as_at))
    ) %>%
    select(abs_name, date_as_at_epoch, air_second_dose_pct) %>%
    group_by(abs_name) %>%
    nest()

air_nested_reversed_model_tbl <- air_nested_tbl %>%
    mutate(
        model = map(data, function(df) lm(air_second_dose_pct ~ date_as_at_epoch, data = df))
    ) 

opening_day_tbl <- as.integer(as.POSIXct("2021-11-23")) %>%
    as_tibble() %>%
    rename(date_as_at_epoch = value)

predicted_vax_rates_tbl <- air_nested_reversed_model_tbl %>%
    mutate(
        prediction = map(.x = model, ~ predict(., opening_day_tbl)), 
        predictors = map(abs_name,  ~ bind_cols(opening_day_tbl)),
        regression_metrics = map(.x = model, ~glance(.))
    ) 

predicted_vax_rates_tidy_tbl <- predicted_vax_rates_tbl %>%
    select(abs_name, prediction, predictors) %>%
    unnest() %>%
    mutate(
        date_as_at = round_date(as_datetime(date_as_at_epoch), "day")
    ) %>%
    select(-date_as_at_epoch) %>%
    mutate(
        type = "Predicted"
    ) %>%
    rename(
        air_second_dose_pct = prediction
    ) %>%
    ungroup()

# visualisation ---------------------------


# get population data
lga_pop_tbl <- read_csv("https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv") %>% 
    clean_names() %>%
    filter(state == "SA") %>%
    slice_max(date_as_at) %>%
    select(abs_name, abs_erp_2019_population) 



# plot the chart
forecast_vax_rates_opening_day_plot <- predicted_vax_rates_tidy_tbl %>%
    ungroup() %>%
    inner_join(lga_pop_tbl) %>%
    filter(
        abs_name != "Grant (DC)"
    ) %>%
    mutate(
        above_80 = if_else(air_second_dose_pct >= 80, "Yes", "No")
    ) %>%
    mutate(
        abs_name = fct_reorder(abs_name, air_second_dose_pct)
    ) %>%
    ggplot(aes(abs_name, air_second_dose_pct)) +
    geom_point(aes(colour=above_80, size=abs_erp_2019_population)) +
    scale_size_continuous(labels=comma) +
    theme_clean() + 
    scale_colour_tableau() + # from ggthemes
    coord_flip() +
    labs(
        title = "Forecast COVID Vaccination Second Dose % by LGA for 23 November 2021",
        caption =  plot_footer,
        y = "Second Dose %",
        x = "",
        colour = "Above 80% Second Dose",
        size = "ABS 2019 Population"
    )


# save to disc
ggsave(
    forecast_vax_rates_opening_day_plot, 
    filename = "plots/forecast_vax_rates_opening_day_plot.png", 
    width=297, 
    height=210, 
    units = "mm"
)

# high risk plo

# get high risk lgas, defined as those with vaccination < 80% and a population > 50k
high_risk_lgas <- predicted_vax_rates_tidy_tbl %>%
    inner_join(lga_pop_tbl) %>%
    filter(
        abs_erp_2019_population > 50000,
        air_second_dose_pct < 80
    ) %>%
    pull(abs_name)

# plot the chart - excluding the Grant District Council
forecast_high_risk_plot <- predicted_vax_rates_tidy_tbl %>%
    filter(
        abs_name != "Grant (DC)"
    ) %>%
    inner_join(lga_pop_tbl) %>%
    mutate(
        above_80 = if_else(air_second_dose_pct >= 80, "Yes", "No")
    ) %>%
    select(abs_name, air_second_dose_pct, abs_erp_2019_population, above_80) %>%
    ggplot(aes(air_second_dose_pct, abs_erp_2019_population, label=abs_name)) +
    geom_point(aes(colour=above_80)) +
    scale_y_continuous(labels=comma_format()) +
    geom_vline(xintercept = 80, linetype="dashed") +
    geom_label_repel(data = subset(predicted_vax_rates_tidy_tbl %>% inner_join(lga_pop_tbl), abs_name %in% high_risk_lgas), size=3) +
    scale_colour_tableau() +
    theme_clean() +
    labs(
        title = "Scatter plot of Forecast COVID Vaccination Second Dose % for SA LGAs at 23 November 2021",
        caption =  plot_footer,
        y = "ABS 2019 Population",
        x = "Predicted % Second Dose",
        colour = "Above 80% Double Vax Rate"
    )

# save to disc
ggsave(
    forecast_high_risk_plot, 
    filename = "plots/forecast_high_risk_plot.png", 
    width=297, 
    height=210, 
    units = "mm"
)
