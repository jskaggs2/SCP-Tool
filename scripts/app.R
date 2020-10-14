## Create a spatial conservation planning application
## Jon Skaggs
## Initiated 2020-10-11


library(shiny)
library(DT)
library(sf)
library(leaflet)


## To Do
# in view_solution, define range of options dynamically
# in evaluate_priortization(), insert check to only count rows which have a rank; throw out NA and 0?
# allow for file upload to control inputs
# tidy plot_eval() family
# in results[[eval]], add a column to mimic results[[solutions]]?
# in annealing, is cooling rate doing what I think it is?
# add hierarchical progress bar to ui
# add protected area slider functionality
# add annealing controls as subgroup within sidepanel
# split solutions across multiple cores
# use tryCatch() to report progress outside of prioritization(); eliminate need for prioritization_shiny()
# make withProgress() dynamic in prioritization_shiny()
# add error handling to user file upload
# add huc10 prioritization capability
# features, protected areas, and zones defined by single file upload?
# add feature to allow remove zone from analysis


# User interface side -----------------------------------------------------


ui <- fluidPage(
  
  # Title
  titlePanel(
    #h4("Prioritization Tool v1")
    "Spatial Conservation Prioritization Tool (v1)"
  ),
  
  # Sidebar panel for inputs
  sidebarLayout(
    
    sidebarPanel(
    
      ## Run
      actionButton(inputId = "run", label = "Run"),
      
      tags$hr(),
      
      ## User defined settings
      
      h4(HTML("<I>Required</I>")),
      
      # input dataset
      fileInput("dataFile", "Input data", accept = ".csv"),
    
      # solutions
      numericInput(inputId = "solutions", label = "Number of solutions", value = 1, min = 1, max = 100, step = 1),
      
      # algorithm
      selectInput(inputId = "algorithm", label = "Algorithm", choices = c("Core Area Zonation" = "CAZ", "Additive Benefit Function" = "ABF")),
      
      # field_value
      selectInput(inputId = "field_value", label = "Feature representation", choices = c("Presence-Absence" = "rep001", "Modelled Habitat Suitability" = "rep003")),
      
      # field_zone
      #textInput(inputId = "field_zone", label = "Zones", value = "HUC12"),
      #selectInput(inputId = "field_zone", label = "Zones", choices = c("Upper Coosa" = "HUC12", "Conasauga" = "03150301", "Coosawattee" = "03150302", "Oostanauala" = "03150304", "Etowah" = "03150304")),
      
      # AOI
      # selectInput(inputId = "aoi", label = "Area of interest", choices = c("Upper Coosa" = "031503", "Conasauga" = "03150301", "Coosawattee" = "03150302", "Oostanauala" = "03150304", "Etowah" = "03150304")),
      textInput(inputId = "aoi", label = "Area of interest", value = "0315"),
      
      # evalThreshold
      sliderInput(inputId = "evalThreshold", label = "Representation threshold for evaluation", min = 1, max = 10, value = 1, step = 1),
      
      tags$hr(),
      
      h4(HTML("<I>Optional</I>")),
      
      # subset features
      checkboxInput(inputId = "subsetFeatures", label = "Subset features", value = FALSE),
      conditionalPanel(
        condition = "input.subsetFeatures==1", #javascript
        fileInput("featuresFile", "Features", accept = ".csv")),

      # field_cost
      checkboxInput(inputId = "useCost", label = "Use zone cost", value = FALSE),
      conditionalPanel(
        condition = "input.useCost==1", #javascript
        selectInput(inputId = "field_cost", label = "Cost", choices = c("Local drainage area" = "DA_km2", "Total drainage area" = "TotDA_km2"))),
      
      # weight
      checkboxInput(inputId = "useWeight", label = "Use feature weight", value = FALSE),
      conditionalPanel(
        condition = "input.useWeight==1", #javascript
        textInput(inputId = "weight", label = "Feature weight")),
      
      # protectedAreas using file
      checkboxInput(inputId = "useProtectedAreas_file", label = "Use protected areas - file", value = FALSE),
      conditionalPanel(
        condition = "input.useProtectedAreas_file==1", #javascript
        fileInput("protectedAreasFile", "Based on name", accept = ".csv")),
      
      # protectedAreas using percentage
      checkboxInput(inputId = "useProtectedAreas_p", label = "Use protected areas - percent", value = FALSE),
      conditionalPanel(
        condition = "input.useProtectedAreas_p==1", #javascript
        textInput(inputId = "field_pa", label = "Protected areas field name", value = "Percent_PA"),
        sliderInput(inputId = "pa_p", label = "Based on % protection", min = 0, max = 1, value = 0.6, step = 0.1)),
      
      # useAnnealing
      checkboxInput(inputId = "useAnnealing", label = "Use simulated annealing", value = FALSE),
      conditionalPanel(
        condition = "input.useAnnealing==1", #javascript
        sliderInput(inputId = "temperature_start", label = "Starting temperature", min = 0, max = 1, value = 0.5, step = 0.1),
        sliderInput(inputId = "cooling_rate", label = "Cooling rate", min = 0, max = 1, value = 0.1, step = 0.1))),
  
  mainPanel(
    
    # Present results as tabs
    tabsetPanel(type = "tabs",
                # Tab 1
                tabPanel("Overview",
                         h2("Overview"),
                         h3("Description"),
                         HTML(""),
                         br(),
                         br(),
                         h3("Quick start"),
                         HTML(""),
                         br(),
                         br(),
                         h3("More information"),
                         HTML("")),
    
                # Tab 2
                tabPanel("View solution",
                         h2("View solution"),
                         numericInput(inputId = "view_solution", label = "View solution:", value = 1, min = 1, max = 100, step = 1),
                         #plotOutput("map", click = "plot_click"),
                         #verbatimTextOutput("mapinfo"),
                         leafletOutput("map"),
                         plotOutput("eval"),
                         DT::dataTableOutput("table_pa")),
                # Tab 3
                tabPanel("View all solutions",
                         h2("View all solutions"),
                         plotOutput("eval_all")),
                # Tab 4
                tabPanel("Help",
                         h2("Help"),
                         h3("Detailed variable descriptions"),
                         HTML("<strong>Input data</strong> (required). Species representation values (columns) across zones (rows). Only CSV allowed."),
                         br(),
                         br(),
                         HTML("<strong>Number of solutions</strong> (required). Define the number of prioritization solutions for given settings i.e. bootstrapping variable."),
                         br(),
                         br(),
                         HTML("<strong>Algorithm</strong> (required)."),
                         br(),
                         br(),
                         HTML("<strong>Feature representation</strong> (required)."),
                         br(),
                         br(),
                         HTML("<strong>Area of interest</strong> (optional). Define the spatial extent of the analysis using USGS NHD HUC codes. Defaults to all zones in 'Input Data'. E.g. 0315 (Upper Coosa) or 03150104 (Etowah)."),
                         br(),
                         br(),
                         HTML(""))),
    
    )
  )
)


# Server side -------------------------------------------------------------


# Cheating input data
load("../data/dat.Rdata")
path_sf_zones <- "../data/huc12_uc.shp"
source("helpers.R")

# Notes
# https://shiny.rstudio.com/articles/plot-interaction.html add plot interaction

server <- function(input, output){
  
  # If user clicks action button, run prioritization and visualize results
  observeEvent(input$run, {
    
    ## Required
    dat <- read.csv(input$dataFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    features_values <- names(dat[grep(pattern = field_value, x = names(dat))]) #temporary
    features <- unique(gsub(pattern = "*.rep00.*", replacement = "", x = features)) #temporary
    field_zone <- "HUC12" #temporary
    dat[[field_zone]] <- paste0(0, dat[[field_zone]]) #temporary
    
    ## Optional
    if(input$subsetFeatures == TRUE){
      features <- read.csv(input$featuresFile$datapath, header = FALSE, stringsAsFactors = FALSE)[,1]
    }
    if(input$useProtectedAreas_file == TRUE){
      protectedAreas <- read.csv(input$protectedAreasFile$datapath, header = false, stringsAsFactors = FALSE)[,1]
    }else if(input$useProtectedAreas_p == TRUE){
      protectedAreas <- dat[dat[input$field_pa] > input$pa_p, field_zone]
    }else{
      protectedAreas <- c()
    }
    # Subset input dataset by AOI
    dat_aoi <- dat[grep(pattern = input$aoi, dat[[field_zone]]),]
  
    result <- prioritization_shiny(
      solutions = input$solutions,
      df = dat_aoi,
      features = features,
      #zones = input$zones,
      field_value = input$field_value,
      field_zone = field_zone, #temporary
      useCost = input$useCost,
      field_cost = input$field_cost,
      useFeatureWeight = input$useWeight,
      #weight = input$weight, #temporary
      protectedAreas = protectedAreas,
      algorithm = input$algorithm,
      useAnnealing = input$useAnnealing,
      temperature_start = input$temperature_start,
      cooling_rate = input$cooling_rate,
      evalThreshold = input$evalThreshold)
    
    # Individual spatial plot
    output$map <- renderLeaflet({
      map_results_leaflet(result, path_sf_zones, input$view_solution)
    })
    
    #output$map <- renderPlot({
    #  map_results(result = result, path_sf_zones = path_sf_zones, s = input$view_solution + 1)
    #})
    
    # Individual spatial plot click
    #output$mapinfo <- renderText({
    #  paste0("Rank: ", input$plot_click$Rank,
    #         "\n", "Zone:", input$plot_click$HUC12)
    #})
    
    # Individual evaluation plot
    output$eval <- renderPlot({
      plot_eval(result = result, s = input$view_solution)
    })
    
    # Protected areas applied
    if(input$useProtectedAreas_p == TRUE){
      output$table_pa <- DT::renderDataTable({
        dat[,c(field_zone, input$field_pa)]
      })
    }
    
    # All tabular results
    output$table <- DT::renderDataTable({
      result[["summary"]]
    })
    
    # All evaluation plots
    output$eval_all <- renderPlot({
      plot_eval_all(result = result)
    })
  
    showNotification("Complete", type = "message")
    
  }) #observeEvent
} #server

shinyApp(ui, server)