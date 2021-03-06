
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Long Term Care Facility COVID Dashboard

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/377954746.svg)](https://zenodo.org/badge/latestdoi/377954746)
<!-- badges: end -->

The goal of LTCF Covid Dashboard is to provide a tool for Long Term Care
Facility staff and stakeholders to explore the spread of COVID-19 with
in long term care facilities and the effects of variables like
vaccination rates, community prevalence, and testing cadence.

# File structure

    ltcf_covid_dashboard
    ├── LICENSE
    ├── R
    │   ├── 00_project_init.R
    │   ├── 01_abm_helpers.R
    │   ├── 02_abm_fun.R
    │   ├── 03_plot_funs.R
    │   ├── 04_summaries.R
    │   └── 05_splash_page.R
    ├── README.md
    ├── README.Rmd
    ├── server.R
    ├── ui.R
    └── www
        └── styles.css

# Running the app locally

**Required Packages** :

shiny, shinyDashboard, tidyr, dplyr, purrr, ggplot2, conflicted, plotly

## Using `runGitHub` from the shiny package

This requires the [Shiny](https://www.github.com/rstudio/shiny) package.

``` r
if (!require("shiny", character.only = TRUE)) {
  install.packages("shiny", dependencies = TRUE)
}
library(shiny)
runGitHub("jakedilliott/ltcf_covid_dashboard", ref = "main")
```

## Running the files on your local machine

1.  clone or download this repository from GitHub
2.  open the folder in RStudio and run the app by opening the **ui.R**
    file and pressing the **runApp** button or typing `shiny::runApp()`
    into the console. For the second option make sure the working
    directory is set to ltcf_covid_dashboard.

# Running your own simulations

1.  Source files in the **R/** directory, which holds all of the
    agent-based model simulation functions
2.  Initialize agents that will be used in the simulation with the
    `make_agents()` function (found in R/01_abm_helpers.R)
3.  Use the `snf_abm()` function to run simulations. Read R/02_abm_fun.R
    to see the inputs for the `snf_abm()` function. For running multiple
    simulations there is also the `multi_run()` function which takes the
    number of simulations you want to run and all of the same inpunts as
    `snf_abm()`

## Minimal Example

``` r
# Source files in R/ directory
files <- list.files("R/", full.names = TRUE)
lapply(files, source, echo = FALSE)

# Example simulation with 125 Day Staff, 45 Night staff, and 90 Residents
# 38% of Staff are vaccinated, 78% of Residents are vaccinated
# start the simulation with 2% of staff infected
# The rest of the inputs are left to their defaults, 
# which can be found in R/02_abm_fun.R
agents <- make_agents(day = 125, night = 45, res = 90)
sim_results <- snf_abm(inputs = agents, R_0_staff = 0.38, R_0_res = 0.78,
                       init_inf_staff = 0.02)
```
