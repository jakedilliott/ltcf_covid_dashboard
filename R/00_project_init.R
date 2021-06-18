## Necessary Libraries
library(shiny)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(conflicted)
library(plotly)

## Resolve package conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("box", "shinydashboard")
conflict_prefer("layout", "plotly")
