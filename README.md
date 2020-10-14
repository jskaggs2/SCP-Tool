# SCP-Tool

last updated: 2020-10-14

### Spatial conservation prioritization tool

Goal: Rank spatial zones on a landscape by their conservation value. Conservation value is defined by user inputs. Implemented in R and Rshiny.

### Contents

**data** contains example input datasets

**scripts** contains `app.R`, which defines the shiny user interface and server, and `helpers.R`, which defines functions applied in `app.R`

### Run from the R console

1. Install prerequisities

`install.packages(c("shiny", "DT", "sf", "leaflet"))`

2. Run application

`shiny::runApp("C:/scp/scripts")` # path to folder containing app.R
