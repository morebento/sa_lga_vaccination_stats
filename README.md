
# Vaccination stats and predictions for South Australian Local Government Areas

## Introduction 

The South Australian Government has announced lifting of various COVID restrictions when the State reaches a vaccination rate of 80% overall, and then further restriction lifting at 90%. These figures made me wonder how much variation there is between the whole of state vaccination rate and those for Local Government Areas (Councils), and what sort of variation there is between LGAs. So the purpose of this is to look at some exploratory charts and statistics. 

## Attribution

Source: WA Health (second dose by state data prior to 1st July 2021) and Department of Health (all other data); Data extracted by Ken Tsang

## Data

For this exercise the data from the <https://covid19nearme.com.au/vaccination-stats> will be used. These excellent people have written scraper, parser and data visualisation code for all of the Australian governments' COVID data. All of their code is published here <https://github.com/jxeeno/aust-govt-covid19-vaccine-pdf> and the collated vaccination data is published here <https://vaccinedata.covid19nearme.com.au/data/geo/air_lga.csv> in CSV format, and this is the source for this exercise. 

Note that for the LGAs the data is for age 15+ 

## Published 

The rendered version of the R Markdown file is published here <https://rpubs.com/morebento/SA_LGA_vax_stats_exploration>

Plots are found in the `plots` directory


## To Do

1. ~~R Markdown notebook describing approach and statistics~~ - Done
2. ~~Date vs Vax rate modelling & visualisation~~ - Done
3. ~~Vax rate vs Date modelling & visualisation~~ - Done
4. Shiny dashboard
5. ~~Vax rate vs ABS inequality & visualisation~~ - Done



## Author

Ben Moretti
7 November 2021