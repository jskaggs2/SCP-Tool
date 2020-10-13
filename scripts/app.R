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
    
      # solutions
      numericInput(inputId = "solutions", label = "Number of solutions", value = 1, min = 1, max = 100, step = 1),
      
      # algorithm
      selectInput(inputId = "algorithm", label = "Algorithm", choices = c("Core Area Zonation" = "CAZ", "Additive Benefit Function" = "ABF")),
      
      # features
      fileInput("featuresFile", "Features", accept = ".csv"),
      
      # field_value
      selectInput(inputId = "field_value", label = "Feature representation", choices = c("Presence-Absence" = "rep001", "Modelled Habitat Suitability" = "rep003")),
      
      # field_zone
      #textInput(inputId = "field_zone", label = "Zones", value = "HUC12"),
      selectInput(inputId = "field_zone", label = "Zones", choices = c("Upper Coosa" = "HUC12", "Conasauga" = "03150301", "Coosawattee" = "03150302", "Oostanauala" = "03150304", "Etowah" = "03150304")),
      
      # evalThreshold
      sliderInput(inputId = "evalThreshold", label = "Representation threshold for evaluation", min = 1, max = 10, value = 1, step = 1),
      
      tags$hr(),
      
      h4(HTML("<I>Optional</I>")),
      
      # field_cost
      checkboxInput(inputId = "useCost", label = "Use zone cost"),
      conditionalPanel(
        condition = "input.useCost==1", #javascript
        selectInput(inputId = "field_cost", label = "Cost", choices = c("None" = "None", "Local drainage area" = "DA_km2", "Total drainage area" = "TotDA_km2"))),
      
      # weight
      checkboxInput(inputId = "useWeight", label = "Use feature weight", value = FALSE),
      conditionalPanel(
        condition = "input.useWeight==1", #javascript
        textInput(inputId = "weight", label = "Feature weight")),
      
      # protectedAreas
      checkboxInput(inputId = "useProtectedAreas", label = "Use existing protected areas", value = FALSE),
      conditionalPanel(
        condition = "input.useProtectedAreas==1", #javascript
        fileInput("protectedAreasFile", "Existing protected areas", accept = ".csv"),
        sliderInput(inputId = "protectedAreas", label = "Define % protection", min = 0, max = 1, value = 0.6, step = 0.1)),
      
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
                         numericInput(inputId = "view_solution", label = "View solution:", value = 1, min = 1, max = 100, step = 1), #define range dynamically?
                         #plotOutput("map", click = "plot_click"),
                         #verbatimTextOutput("mapinfo"),
                         leafletOutput("map"),
                         plotOutput("eval")),
                # Tab 3
                tabPanel("View all solutions",
                         h2("View all solutions"),
                         plotOutput("eval_all"),
                         DT::dataTableOutput("table")),
                # Tab 4
                tabPanel("Help",
                         h2("Help"),
                         h3("Detailed variable descriptions"),
                         HTML(""),
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
    
    ## Load user files
    # Required
    features <- read.csv(input$featuresFile$datapath, header = FALSE, stringsAsFactors = FALSE)[,1]
    # Optional
    if(input$useProtectedAreas == TRUE){
      protectedAreas <- read.csv(input$protectedAreasFile$datapath, header = false, stringsAsFactors = FALSE)[,1]
    }else{
      protectedAreas <- c()
    }
  
    result <- prioritization_shiny(
      solutions = input$solutions,
      df = dat, #temporary input$df
      features = features,
      field_value = input$field_value,
      field_zone = input$field_zone,
      useCost = input$useCost,
      field_cost = input$field_cost,
      useFeatureWeight = input$useWeight,
      #weight = input$weight, #temporary
      useProtectedAreas = input$useProtectedAreas,
      protectedAreas = input$protectedAreas,
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