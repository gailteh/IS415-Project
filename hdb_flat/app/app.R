
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
library(shinycssloaders)

library(leaflet)
library(tidyverse)
library(tools)
library(shinythemes)
library(shinyjs)
library(tmap)
library(readr)
library(sp)
library(sf)
library(rgdal)
library(spNetwork)
library(spatstat)
library(raster)
library(maptools)
library(dplyr)
library(stringr)
library(sfdep)

# Geospatial Data Import and Wrangling

# sg (Coastal outline)
sg_owin <- read_rds("rds/sg_owin.rds")
sg <- read_rds("rds/sg.rds")
sg_sf <- read_rds("rds/sg_sf.rds")
sg_sp <- read_rds("rds/sg_sp.rds")

# mpsz
mpsz <- read_rds("rds/mpsz.rds")
mpsz_sf <- read_rds("rds/mpsz_sf.rds")


# Aspatial Data Import and Wrangling

# HDB flats
carpark_sf <- read_rds("rds/carpark_sf.rds")
carpark_ppp <- read_rds("rds/carpark_ppp.rds")
carpark_ppp_km <- rescale(carpark_ppp, 1000, "km")

# hawker
hawker_sf <- read_rds("rds/hawker_sf.rds")
# hawker_sp <- write_rds(hawker_sp, "data/rds/hawker_sp.rds")
# hawker_ppp <- read_rds("data/rds/hawker_ppp.rds")
# hawker <- read_rds("data/rds/hawker.rds")

# HDB flats
hdb_sf <- read_rds("rds/hdb_sf.rds")

# Shopping mall
mall_sf <- read_rds("rds/mall_sf.rds")


#### LCLQ preparation & wrangling ####
hawker_lclq <- hawker_sf |>
  mutate(Name = "Hawker")

carpark_lclq <- carpark_sf |>
  dplyr::select(address, geometry) |>
  mutate(address = "Carpark") |>
  rename("Name" = "address")

hdb_lclq <- hdb_sf |>
  dplyr::select(address, geometry) |>
  mutate(address = "HDB") |>
  rename("Name" = "address")

mall_lclq <- mall_sf |>
  rename("Name" = "Mall Name")

## Combine LCLQ together
hk_cp_lclq <- rbind(hawker_lclq, carpark_lclq)
hdb_cp_lclq <- rbind(hdb_lclq, carpark_lclq)
mall_cp_lclq <- rbind(mall_lclq, carpark_lclq)


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
                                     sidebarPanel(
                                       # select kernel method
                                       selectInput(inputId = "kernel_var",
                                                   label = "Select a Kernel Method",
                                                   choices = list("Gaussian" = "gaussian",
                                                                  "Epanechnikov" = "epanechnikov",
                                                                  "Quartic" = "quartic",
                                                                  "Disc" = "disc"),
                                                   selected = "gaussian"),
                                       
                                       # select bandwidth method
                                       radioButtons(inputId = "bandwidth_method",
                                                    label = "Select the bandwidth method to be used:",
                                                    choices = c("Auto" = "auto",
                                                                "Fixed" = "fixed", 
                                                                "Adaptive" = "adaptive"),
                                                    selected = "auto"),
                                       
                                       # if automatic 
                                       conditionalPanel(
                                         condition = "input.bandwidth_method == 'auto'",
                                         selectInput(inputId = "bw_auto",
                                                     label = "Select a Bandwidth Method",
                                                     choices = list("bw.CvL" = "bw.CvL",
                                                                    "bw.scott" = "bw.scott",
                                                                    "bw.diggle" = "bw.diggle",
                                                                    "bw.ppl" = "bw.ppl"),
                                                     selected = "bw.diggle")
                                       ),
                                       
                                       # if fixed
                                       conditionalPanel(
                                         condition = "input.bandwidth_method == 'fixed'",
                                         sliderInput(inputId = "bw_fix_var",
                                                     label = "Select the fixed bandwidth to be used: (in km)",
                                                     min = 0,
                                                     max = 5,
                                                     step = 0.1,
                                                     value = 0.6)
                                       ),
                                       
                                       actionButton(inputId = "kde_run",
                                                    label = "Run Kernel Density Estimation")
                                     ),
                                     
                                     mainPanel(
                                       tmapOutput("kde_map")
                                     )
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
  
  # KDE Plot
  
  output$kde_map <- renderTmap({
    
    input$kde_run
    
    kde <- reactive({
      if (input$bandwidth_method == 'auto'){
        if (input$bw_auto == 'bw.diggle'){
          the_bw <- bw.diggle(carpark_ppp_km)
        }
        else if (input$bw_auto == 'bw.CvL'){
          the_bw <- bw.CvL(carpark_ppp_km)
        }
        else if (input$bw_auto == 'bw.scott'){
          the_bw <- bw.scott(carpark_ppp_km)
        }
        else if (input$bw_auto == 'bw.ppl'){
          the_bw <- bw.ppl(carpark_ppp_km)
        }
        kde <- isolate(density(carpark_ppp_km,
                               sigma=as.numeric(the_bw),
                               edge=TRUE,
                               kernel=input$kernel_var))
        
      }
      else if (input$bandwidth_method == 'fixed'){
        kde <- isolate(density(carpark_ppp_km,
                               sigma=input$bw_fix_var,
                               edge=TRUE,
                               kernel=input$kernel_var))
      }
      else if (input$bandwidth_method == 'adaptive'){
        kde <- isolate(adaptive.density(carpark_ppp_km, 
                                        method="kernel"))
      }
      return (kde)
    })
    
    gridded_kde <- isolate(as.SpatialGridDataFrame.im(kde()))
    kde_raster <- isolate(raster(gridded_kde))
    projection(kde_raster) <- CRS("+init=EPSG:3414 +datum=WGS84 +units=km")
    
    tmap_mode("view")
    
    SPPA_KDE <- isolate(tm_shape(sg_sf) +
                          tm_borders(col = 'black',
                                     lwd = 1,
                                     alpha = 0.5) +
                          tm_shape(kde_raster) + 
                          tm_raster("v", alpha = 0.7) +
                          tm_layout(legend.outside = TRUE, frame = FALSE, title = "KDE") +
                          tmap_options(basemaps = c("Esri.WorldGrayCanvas","OpenStreetMap", "Stamen.TonerLite"),
                                       basemaps.alpha = c(0.8, 0.8, 0.8)) +
                          tm_view(set.zoom.limits = c(11,13))) 
  })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
