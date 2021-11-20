#' current_vax_rates_vs_seifa.R
#'
#' inspired by this post below
#' https://www.reddit.com/r/Adelaide/comments/qk8p2w/comment/hiv2z3h/?utm_source=share&utm_medium=web2x&context=3
#'
#' SEIFA data https://www.abs.gov.au/websitedbs/censushome.nsf/home/seifa
#'
#' downloadable here 
#' https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2033.0.55.0012016?OpenDocument 
#'
#' ben moretti
#' 8 november 2021


# setup -------------------------------

library(tidyverse)
library(janitor)
library(ggthemes)
library(ggrepel)
library(broom)
library(scales)
library(lubridate)
library(readxl)
library(httr)


# gather data  -------------------------------

# read the data from csv and clean the names
air_tbl <- read_csv("https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv") %>% 
    clean_names()

# get the latest date
max_date <- air_tbl %>%
    slice_max(date_as_at) %>%
    summarise(
        max_date_as_at = max(date_as_at)
    ) %>%
    pull(max_date_as_at)


# seifa data is availabel from here 
# https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2033.0.55.0012016?OpenDocument

# this is the url for the excel spreadsheet
seifa_url <- "https://www.abs.gov.au/ausstats/subscriber.nsf/log?openagent&2033055001%20-%20lga%20indexes.xls&2033.0.55.001&Data%20Cubes&5604C75C214CD3D0CA25825D000F91AE&0&2016&27.03.2018&Latest"

# download file
GET(seifa_url, write_disk(tf <- tempfile(fileext = ".xls")))

# import Table 2 State Suburb (SSC) Index of Relative Socio-economic Disadvantage, 2016
seifa_excel <- read_excel(tf, sheet = 3, range = "B6:D13697", )


# condition --------------------------

# condition and rename
seifa_excel_tbl <- seifa_excel %>%
    rename(
        abs_name = `...1`,
        population = `...2`,
        seifa_score = Score
    )

# create a standard caption footer
plot_footer <- stringr::str_glue("Latest Data: {max_date} Author: @morebento. Code: https://github.com/morebento/sa_lga_vaccination_stats")

# merge covid vax data with seifa data
combined_tbl <- inner_join(air_tbl, seifa_excel_tbl)

# select SA data and most recent datetime
selected_sa_tbl <- combined_tbl %>%
    slice_max(date_as_at) %>%
    filter(state == "SA") %>%
    filter(str_detect(abs_name, "\\(C\\)")) %>%
    filter(abs_name != "Grant (DC)") %>%
    select(date_as_at, abs_name, air_second_dose_pct, seifa_score, population) %>%
    drop_na(air_second_dose_pct, seifa_score) 

# get a list of LGAs with high or low seifa scores for labelling
labels <- selected_sa_tbl %>%
    filter(
        seifa_score > 1050 | seifa_score < 900
    ) %>%
    pull(abs_name)

    
# model --------------------------------

# regress
lm_fit <- lm(air_second_dose_pct ~ seifa_score, data = selected_sa_tbl)

# examine the regression statistics - adjusted r squared is only 0.507
glance(lm_fit)

# look at predicted vs actual plus residuals
augment(lm_fit)

# examine the lm object
tidy(lm_fit)

# fit the values to the model to get a line for regression  and bind back onto the original data
predicted_tbl <- predict(lm_fit, newdata = selected_sa_tbl) %>%
    as_tibble() %>%
    rename(
        predicted_air_second_dose_pct = value
    ) %>%
    bind_cols(selected_sa_tbl) %>%
    select(date_as_at:population, predicted_air_second_dose_pct)

# visualise ------------------------------------------------------

# plot
current_seifa_vs_vax_rate_plot <- predicted_tbl %>%

    ggplot(aes(x=seifa_score, y=air_second_dose_pct, label=abs_name)) +
    geom_point(aes(size=population)) +
    scale_size_continuous(labels=comma) +
    geom_line(aes(x=seifa_score, y=predicted_air_second_dose_pct), alpha=0.8, linetype="dashed") +
    geom_label_repel(size=3) +
    ylim(0,100) +
    theme_clean() + 
    scale_colour_tableau() + # from ggthemes
    labs(
        title = "SEIFA Score vs Actual second Covid vaccination % by SA LGA (Metro Councils)",
        subtitle = "SEIFA Data: ABS State Suburb (SSC) Index of Relative Socio-economic Disadvantage, 2016",
        caption =  plot_footer,
        y = "Second Dose %",
        x = "SEIFA Score" 
    )

# save to disc
# save to disc
ggsave(
    current_seifa_vs_vax_rate_plot, 
    filename = "plots/current_seifa_vs_vax_rate_plot.png", 
    width=297, 
    height=210, 
    units = "mm"
)
