library(tidyverse)
library(janitor)
library(ggthemes)
library(prophet)



# define source url
source_url <- "https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv"

# get the data into the format prophet uses
data_prophet_tbl <- read_csv(source_url) %>% 
    clean_names() %>%
    filter(abs_name == "Playford (C)") %>%
    select(date_as_at,  air_second_dose_pct) %>%
    rename(
        ds = date_as_at,
        y = air_second_dose_pct
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

data_prophet_tbl %>% glimpse()

combined_tbl <- forecast_tbl %>%
    left_join(data_prophet_tbl) %>% 
    as_tibble()

date_at_80 <- combined_tbl %>%
    filter(
        between(yhat, 80, 81)
    ) %>%
    select(ds, yhat) %>%
    slice_min(yhat) %>%
    pull(ds)

date_at_90 <- combined_tbl %>%
    filter(
        between(yhat, 90, 91)
    ) %>%
    select(ds, yhat) %>%
    slice_min(yhat) %>%
    pull(ds)


combined_tbl %>%
    filter(yhat <= 100) %>%
    ggplot(aes(x=ds)) +
    geom_line(aes(y=yhat, colour="Predicted")) +
    geom_point(aes(y=y, colour="Actual"), size=0.9) +
    geom_ribbon(aes(ymax = yhat_upper, ymin = yhat_lower, fill="Confidence"), alpha=0.5) +
    geom_hline(yintercept=80, linetype="dashed") +
    geom_hline(yintercept = 90, linetype = "dashed") +
    geom_vline(xintercept = date_at_80, linetype="dotted") +
    geom_vline(xintercept = date_at_90, linetype = "dotted") +
    theme_clean() + 
    scale_colour_tableau() + # from ggthemes
    labs(
        title = "SA 16+ vaccination % vs Time",
        colour = "Dose",
        x = "",
        y = "%"
    )
