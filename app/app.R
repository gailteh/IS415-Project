
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

# Geospatial Data Import and Wrangling

# sg (coastal outline)
sg_owin <- read_rds("data/rds/sg_owin.rds")
sg <- read_rds("data/rds/sg.rds")
sg_sf <- read_rds("data/rds/sg_sf.rds")
sg_sp <- read_rds("data/rds/sg_sp.rds")

# mpsz
mpsz <- read_rds("data/rds/mpsz.rds")
mpsz_sf <- read_rds("data/rds/mpsz_sf.rds")




# Aspatial Data Import and Wrangling

# hawker
hawker_sf <- read_rds("data/rds/hawker_sf.rds")
# hawker_sp <- write_rds(hawker_sp, "data/rds/hawker_sp.rds")
# hawker_ppp <- read_rds("data/rds/hawker_ppp.rds")
# hawker <- read_rds("data/rds/hawker.rds")

# HDB flats
hdb_sf <- read_rds("data/rds/hdb_sf.rds")

# Shopping mall

# LCLQ preparation & wrangling
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

## Combine LCLQ together
interest_lclq <- rbind(hawker_lclq, carpark_lclq)
interest_lclq <- rbind(interest_lclq, hdb_lclq)
#interest_lclq <- rbind(interest_lclq, shop_lclq)


## Prepare Vector list for LCLQ
Carpark <- interest_lclq %>%
  filter(Name == "Carpark")
A <- interest_lclq$Name

Hawker <- interest_lclq %>%
  filter(Name == "Hawker")
B <- interest_lclq$Name

HDB <- interest_lclq %>%
  filter(Name == "HDB")
C <- interest_lclq$Name

# Shopping_mall <- interest_lclq %>%
#   filter(Name == "Shopping mall")
# D <- interest_lclq$Name




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
                                       actionButton(inputId = "run_g_function",
                                                    label = "Run Analysis")
                                     ),
                                     mainPanel("graphs go here")
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
                                     mainPanel("graphs go here")
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

    
}

# Run the application 
shinyApp(ui = ui, server = server)
