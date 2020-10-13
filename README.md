# SCP-Tool

last updated: 2020-10-13

### Spatial conservation prioritization tool

Goal: Rank spatial zones on a landscape by their conservation value. Conservation value is defined by user inputs. Implemented in R and Rshiny.

### Contents

`scripts` contains `app.R`, which defines the shiny user interface and server, and `helpers.R`, which defines functions applied in `app.R`

`data` contains example input datasets

### Run from the R console

Install prerequisities
`install.packages(c(shiny, DT, sf))`

Run application
`shiny::runApp("C:/scp/scripts")`
