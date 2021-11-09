library(tidyverse)
library(janitor)

data_tbl <- read_csv("https://vaccinedata.covid19nearme.com.au/data/air.csv") %>% clean_names()

data_tbl %>%
    glimpse()

# get all sa datga
sa_data_tbl <- data_tbl %>%
    select(
        date_as_at,
        starts_with("air_sa_"),
        validated, url
    ) 

# exploratory plot
sa_data_tbl %>%
    select(date_as_at, air_sa_16_plus_first_dose_pct, air_sa_16_plus_second_dose_pct) %>%
    pivot_longer(!date_as_at) %>%
    mutate(
        name = case_when(
            name == "air_sa_16_plus_first_dose_pct" ~ "First",
            name == "air_sa_16_plus_second_dose_pct" ~ "Second"
        )
    ) %>%
    drop_na(value) %>%
    ggplot(aes(date_as_at, value)) +
    geom_line(aes(colour=name)) +
    theme_clean() + 
    scale_colour_tableau() + # from ggthemes
    labs(
        title = "SA 16+ vaccination % vs Time",
        colour = "Dose",
        x = "",
        y = "%"
    )

library(prophet)

# get the data into the format prophet uses
data_prophet_tbl <- sa_data_tbl %>%
    select(date_as_at,  air_sa_16_plus_second_dose_pct) %>%
    rename(
        ds = date_as_at,
        y = air_sa_16_plus_second_dose_pct
    ) %>%
    drop_na(y)

# train the model
prophet_model <- prophet(data_prophet_tbl)

# generate future dates
future_tbl <- make_future_dataframe(prophet_model, periods = 90)

# forecast to these dates
forecast_tbl <- predict(prophet_model, future_tbl)

# plot
plot(prophet_model, forecast_tbl)

# plot components
prophet_plot_components(prophet_model, forecast_tbl)

dyplot.prophet(prophet_model, forecast_tbl)

forecast_tbl %>% glimpse()

forecast_tbl %>%
    select(ds, yhat) %>%
    filter(
        between(yhat, 80, 81) | between(yhat, 90, 91)
    )
     