
library(absmaps)
library(tidyverse)
library(sf)
library(ggthemes)
library(janitor)

# define source url
source_url <- "https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv"

# read the data from csv and clean the names
air_tbl <- read_csv(source_url) %>% 
    clean_names()

# get the map data
map_lga_tbl <- load_absmaps(area = "lga", year = 2018, saveDirectory = "data/")


# tidy ------------------------------------


# tidy the data
air_tidy_tbl <- air_tbl %>%
    filter(state == "SA") %>%
    select(date_as_at, state, abs_name, air_first_dose_pct:abs_erp_2019_population) %>%
    drop_na(air_second_dose_pct)


map_lga_sa_tbl <- map_lga_tbl %>%
    filter(
        state_name_2016 == "South Australia"
    )

map_lga_sa_tbl %>% glimpse()

merged_tbl <- air_tidy_tbl %>%
    slice_max(date_as_at) %>%  
    select(abs_name, air_second_dose_pct, air_first_dose_pct) %>%
    inner_join(map_lga_sa_tbl, by = c("abs_name" = "lga_name_2018"))

glimpse(merged_tbl)

merged_tbl %>%
    #filter(
    #    abs_name %in% c("Burnside (C)", "Unley (C)", "Adelaide (C)", "Mitcham (C)")
    #) %>%
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = air_second_dose_pct)) +
    theme_clean() +
    scale_fill_binned()
