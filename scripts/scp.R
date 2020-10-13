## Create a spatial conservation planning application
## Jon Skaggs
## Initiated 2020-10-11


library(shiny)
library(DT)
library(sf)


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
# display optional inputs dynamically


# User interface side -----------------------------------------------------


ui <- fluidPage(
  
  # Title
  titlePanel(
    h4("Prioritization Tool v1")),
  
  # Sidebar panel for inputs
  sidebarLayout(
    
    sidebarPanel(
    
      ## Run
      actionButton(inputId = "run", label = "Run"),
      
      tags$hr(),
      
      ## User defined settings
      
      h4("Required"),
    
      # solutions
      numericInput(inputId = "solutions", label = "Number of solutions", value = 1, min = 1, max = 100, step = 1),
      
      # algorithm
      selectInput(inputId = "algorithm", label = "Algorithm", choices = c("Core Area Zonation" = "CAZ", "Additive Benefit Function" = "ABF")),
      
      # features
      fileInput("featuresFile", "Features", accept = ".csv"),
      
      # field_value
      selectInput(inputId = "field_value", label = "Feature representation", choices = c("Presence-Absence" = "rep001", "Modelled Habitat Suitability" = "rep003")),
      
      # field_zone
      textInput(inputId = "field_zone", label = "Zones", value = "HUC12"),
      
      tags$hr(),
      
      h4("Optional"),
      
      # field_cost
      checkboxInput(inputId = "useCost", label = "Use cost", value = FALSE),
      selectInput(inputId = "field_cost", label = "Cost", choices = c("None" = "None", "Zone drainage area" = "DA_km2", "Total upstream drainage area" = "TotDA_km2")),
      
      # weight
      checkboxInput(inputId = "useWeight", label = "Use feature weight", value = FALSE),
      textInput(inputId = "weight", label = "Feature weight"),
      
      # protectedAreas
      checkboxInput(inputId = "useProtectedAreas", label = "Existing protected areas", value = FALSE),
      fileInput("protectedAreasFile", "Define existing protected areas", accept = ".csv"),
      #sliderInput(inputId = "protectedAreas", label = "Define protected zones", min = 0, max = 1, value = 0.6, step = 0.1),
      
      # useAnnealing
      checkboxInput(inputId = "useAnnealing", label = "Use simulated annealing", value = FALSE),
      
      # temperature_start
      sliderInput(inputId = "temperature_start", label = "Starting temperature", min = 0, max = 1, value = 0.5, step = 0.1),
      
      # cooling_rate
      sliderInput(inputId = "cooling_rate", label = "Cooling rate", min = 0, max = 1, value = 0.1, step = 0.1),
      
      tags$hr(),
      
      # evalThreshold
      sliderInput(inputId = "evalThreshold", label = "Representation threshold for evaluation", min = 1, max = 10, value = 1, step = 1)),
  
  mainPanel(
    
    # Present results as tabs
    tabsetPanel(type = "tabs",
                # Tab 1
                tabPanel("Overview",
                         h2("Overview"),
                         h3("Description"),
                         HTML("Prioritization Tool v1 is a conservation planning tool designed to explore..."),
                         br(),
                         br(),
                         h3("Quick start"),
                         HTML("Select..."),
                         br(),
                         br(),
                         h3("More information"),
                         HTML("See detailed variable descriptions in the Help tab.")),
    
                # Tab 2
                tabPanel("View solution",
                         numericInput(inputId = "view_solution", label = "View solution:", value = 1, min = 1, max = 100, step = 1), #define range dynamically?
                         plotOutput("map"),
                         plotOutput("eval")),
                # Tab 3
                tabPanel("View all solutions",
                         plotOutput("eval_all"),
                         DT::dataTableOutput("table")),
                # Tab 4
                tabPanel("Help",
                         h2("Help"),
                         h3("Detailed variable descriptions"),
                         HTML("<strong>solutions</strong> describes the number of solutions to calculate based on input values"),
                         br(),
                         HTML("<strong>algorithm</strong> describes the zone valuation formula"))),
    
    )
  )
)


# Server side -------------------------------------------------------------


# Cheating input data
load("../data/dat.Rdata")
#load("ap/spp.Rdata")
#load("./pa.Rdata")
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
    
    ## VIEW ONE SOLUTION
    
    # Individual spatial plot
    output$map <- renderPlot({
      map_results(result = result, path_sf_zones = path_sf_zones, s = input$view_solution + 1)
    })
    
    # Individual evaluation plot
    output$eval <- renderPlot({
      plot_eval(result = result, s = input$view_solution)
    })
    
    ## VIEW ALL SOLUTIONS
    
    # All tabular results
    output$table <- DT::renderDataTable({
      result[["summary"]]
    })
    
    # All evaluation plots
    output$eval_all <- renderPlot({
      plot_eval_all(result = result)
    })
  }) #observeEvent
} #server

shinyApp(ui, server)