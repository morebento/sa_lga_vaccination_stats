#' charts_exploration.R
#'
#' 7 november 2021
#' 
#' ben moretti 
#' 
#' gathers data from covid19nearme.com.au and plots 
#'



# setup ------------------------------------

# libraries
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggrepel)
library(broom)
library(scales)
library(lubridate)


# gather ------------------------------------

# define source url
source_url <- "https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv"

# read the data from csv and clean the names
air_tbl <- read_csv(source_url) %>% 
    clean_names()

# tidy ------------------------------------


# tidy the data
air_tidy_tbl <- air_tbl %>%
    filter(state == "SA") %>%
    select(date_as_at, state, abs_name, air_first_dose_pct:abs_erp_2019_population) %>%
    drop_na(air_second_dose_pct)


# explore ------------------------------------

# get the latest date
max_date <- air_tidy_tbl %>%
    slice_max(date_as_at) %>%
    summarise(
        max_date_as_at = max(date_as_at)
    ) %>%
    pull(max_date_as_at)


# create a standard caption footer
plot_footer <- stringr::str_glue("Latest Data: {max_date} Author: @morebento. Code: https://github.com/morebento/sa_lga_vaccination_stats")



# plot ------------------------------------

# histogram plot
histogram_plot <- air_tidy_tbl %>%
    slice_max(date_as_at) %>%  
    select(abs_name, air_second_dose_pct, air_first_dose_pct) %>% 
    pivot_longer(!abs_name, names_to = "metric", values_to = "value") %>%
    mutate(
        metric = case_when(
            metric == "air_first_dose_pct" ~ "First",
            metric == "air_second_dose_pct" ~ "Second"
        )
    ) %>%
    ggplot(aes(value)) +
    geom_histogram(aes(fill=metric), alpha=0.8, position="identity", bins = 10) +
    geom_vline(xintercept=50, linetype="dashed", aes(colour="lightgray")) +
    geom_vline(xintercept=70, linetype="dashed", aes(colour="lightgray")) +
    geom_vline(xintercept=80, linetype="dashed", aes(colour="lightgray")) +
    theme_clean() + 
    scale_fill_tableau() + # from ggthemes
    labs(
        title = "Histogram of SA LGAs by percentage of population vaccinated by dose count",
        subtitle = "Data binned into deciles",
        caption = plot_footer,
        fill = "Dose",
        x = "%",
        y = "Count of LGAs"
    )

# save plot to plots directory
ggsave(histogram_plot, filename = "plots/current_vax_rate_histogram.png", width=297, height=210, units = "mm")


# get population data
lga_pop_tbl <- read_csv("https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv") %>% 
    clean_names() %>%
    filter(state == "SA") %>%
    slice_max(date_as_at) %>%
    select(abs_name, abs_erp_2019_population) 

# plot the chart
sorted_scatter_plot <- air_tidy_tbl %>%
    slice_max(date_as_at) %>%  
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
        title = "COVID Vaccination Second Dose % by LGA",
        subtitle = "Point size ABS 2019 Population",
        caption =  plot_footer,
        y = "Second Dose %",
        x = "",
        colour = "Above 80% Second Dose",
        size = "ABS 2019 Population"
    )

# save plot to plots directory
ggsave(sorted_scatter_plot, filename = "plots/current_sorted_scatter_plot.png", width=297, height=210, units = "mm")
