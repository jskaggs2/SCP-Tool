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

3. Upload data

Load "../data/dat.csv" into `input data` on the left panel. 

4. Adjust other settings

Note that not all settings have been tested.

5. Run

Click `Run` at the top of the sidebar. Note progress bar on bottom right (190 zones takes approximately 10 minutes / solution). Once this progress bar disappears, click the View Solution tab. A solution may take ~10 seconds to render.
