
# Libraries
library(shiny)
library(bslib)

# Geospatial Data Import and Wrangling

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
             
             tabPanel("Import Data")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
