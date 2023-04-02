
 #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Libraries
library(shiny)
library(bslib)
library(maptools)
library(sf)
library(raster)
library(spatstat)
library(tmap)
library(dplyr)
library(tidyverse)

# Geospatial Data Import and Wrangling

carpark_ppp <- read_rds("data/carpark_ppp.rds")
carpark_ppp_km <- rescale(carpark_ppp, 1000, "km")

# Aspatial Data Import and Wrangling


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  navbarPage("CarSpot Application",
             tabPanel("Homepage",
                      sidebarLayout(
                        sidebarPanel(
                          "A group project by: Celine, Gail, Kathy for IS415 Geospatial Analytics and Applications"
                        ),
                        mainPanel(
                          "About our project..."
                        )
                      )),
             
             tabPanel("Spatial Point Analysis",
                        tabsetPanel(
                          tabPanel("Choropleth Mapping",
                                   sidebarLayout(
                                     sidebarPanel("various selections"),
                                     mainPanel("map")
                                   )),
                          tabPanel("KDE",
                                   sidebarLayout(
                                     sidebarPanel("sliders and dropdown lists go here"),
                                     mainPanel("Map and interpretations go here")
                                   )),
                          tabPanel("G Function Analysis",
                                   sidebarLayout(
                                     sidebarPanel(
                                       sliderInput(inputId = "g_funct_sim",
                                                   label = "Number of Simulations",
                                                   min = 0,
                                                   max = 1000,
                                                   value= 99),
                                       helpText("99 simulations are being run by default"),
                                       actionButton(inputId = "run_g_function",
                                                    label = "Run Analysis")
                                     ),
                                     mainPanel(
                                       plotOutput("g_function_plot")
                                      )
                                   )),
                          tabPanel("L Function Analysis",
                                   sidebarLayout(
                                     sidebarPanel(
                                       sliderInput(inputId = "l_funct_sim",
                                                   label = "Number of Simulations",
                                                   min = 0,
                                                   max = 1000,
                                                   value= 99),
                                       actionButton(inputId = "run_l_function",
                                                    label = "Run Analysis")
                                     ),
                                     mainPanel(
                                       plotOutput("l_function_plot")
                                     )
                                   )),
                          tabPanel("Local Co-location Quotient Analysis",
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = "dataset",
                                                   label = "Data Source",
                                                   choices = c("HDB Flat Locations" = "HDB FLAT LOCATIONS",
                                                               "Shopping Malls" = "SHOPPING MALLS",
                                                               "Hawker Centers" = "HAWKER CENTERS"),
                                                   selected = "HDB FLAT LOCATIONS"),
                                     ),
                                     mainPanel("map goes here")
                                   ))
                        )
                      ),
             
             tabPanel("Import Data",
                      fileInput("file1", "Choose CSV File",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")),)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # G Function Plot
  output$g_function_plot <- renderPlot({
    input$run_g_function
    g_func.csr <- isolate(envelope(carpark_ppp_km, Gest, nsim = input$g_funct_sim))
    plot(g_func.csr)
  })
  
  # L Function Plot
  output$l_function_plot <- renderPlot({
    input$run_l_function
    l_func.csr <- isolate(envelope(carpark_ppp_km, Lest, nsim = input$l_funct_sim))
    plot(l_func.csr, . - r ~ r, xlab="d", ylab="L(d)-r")
  })
  

    
}

# Run the application 
shinyApp(ui = ui, server = server)
