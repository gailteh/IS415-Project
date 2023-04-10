
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
carpark_ppp_km <- read_rds("rds/carparkSG_ppp.km.rds")

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
  rename("Name" = "Mall Name") |>
  mutate( Name = "Mall")

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
                          h2("Project Motivation"),
                          ("Singapore is a densely populated city-state facing the challenge of providing adequate and efficient car park facilities to its residents and visitors. The spatial distribution of car parks across the city is uneven, with some areas experiencing high demand while others have surplus capacity."),
                          ("Many people do not have enough knowledge on the underlying reasons for the spatial distribution of carpark facilities as they are not aware of Point Pattern Analysis techniques."),
                          h2("Project Objectives"),
                          ("We aim to create an application that enables users to:"),
                          tags$ul(
                            tags$li("Explore the capabilities and use cases of Point Pattern Analysis in the case of Car parks in Singapore"),
                            tags$li("Conduct Spatial Point Pattern Analysis to derive insights on the distributions of carparks.")
                          )
                        )
                      )),
             
             tabPanel("Mapping & Colocation",
                      tabsetPanel(
                        tabPanel("Interactive Map",
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     # side panel for the Mapping variables
                                     tags$strong("Spatial Distribution of variables"),
                                     
                                     hr(),
                                     
                                     # select mapping variable
                                     selectInput(inputId = "variable",
                                                 label = "Select a Mapping variable:",
                                                 choices = list("Carpark" = "Carpark",
                                                                "Hawker" = "Hawker",
                                                                "Mall" = "Mall",
                                                                "HDB" = "HDB"),
                                                 selected = "Carpark"),
                                     
                                     # Size of the Points
                                     sliderInput(inputId = "dot_size",
                                                 label = "Select the size of the points:",
                                                 min = 0.01,
                                                 max = 0.1,
                                                 value = c(0.01)),
                                     
                                     # Width of the border line
                                     sliderInput(inputId = "line_width",
                                                 label = "Select the width of the border line:",
                                                 min = 0.1,
                                                 max = 0.5,
                                                 value = c(0.5)),
                                     
                                     # select the point colour
                                     selectInput(inputId = "colour",
                                                 label = "Select the Point Colour:",
                                                 choices = list("Black" = "black",
                                                                "Blue" = "blue", 
                                                                "Cyan" = "cyan",
                                                                "Red" = "red", 
                                                                "Green" = "lightgreen"),
                                                 selected = "Black")
                                   ),
                                   
                                   mainPanel(
                                     withSpinner(tmapOutput("mapPlot",
                                                            width = "100%", 
                                                            height = 550)
                                     ),
                                     hr(),
                                     h3("What does the Interactive Map shows?"),
                                     h6("The interactive map shows the spatial distribution of the selected variable which allows users to easily identify clusters of variables in certain areas."),
                                     h4("How does this Interactive Map work?"),
                                     h6("The interactive tmap offers a user-friendly interface that enables users to easily explore and analyze geospatial data. Users have the flexibility to customize the map by selecting from various available layers and basemaps. Additionally, users can zoom in to focus on specific data points and view detailed information about them.")
                                     
                                   )
                                 )),
                        
                        ### Local Colocation Quotient ###
                        tabPanel("Local Co-Location Quotient (LCLQ)",
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     # side panel for the Mapping variables
                                     tags$strong("Local Co-Location Quotient between Carparks and selected variable"),
                                     
                                     hr(),
                                     
                                     # select mapping variable
                                     selectInput(inputId = "lclq_var",
                                                 label = "Select a Mapping variable:",
                                                 choices = list("Hawker" = "Hawker",
                                                                "Mall" = "Mall",
                                                                "HDB" = "HDB"),
                                                 selected = "Hawker"),
                                     
                                     
                                     # select significance level
                                     
                                     helpText("For a 95% Confidence Interval, Select 0.05 Significance"),
                                     
                                     radioButtons(inputId = "sig", 
                                                  label = "Select level of Significance:",
                                                  c("0.05" = "0.05",
                                                    "0.01" = "0.01"),
                                                  selected = "0.05"),
                                     
                                     # select the number of Neighbours
                                     sliderInput(inputId = "nb_lclq",
                                                 label = "Select the Number of Neighbours:",
                                                 min = 1,
                                                 max = 20,
                                                 value = c(6)),
                                     actionButton("LCLQ_run", "Run Analysis"),
                                     
                                     hr(),
                                     
                                     helpText("Please note that this analysis would take quite some time to display the output, we seek your kind understanding and patience.")
                                     
                                   ),
                                   mainPanel(
                                     withSpinner(tmapOutput("lclq_plot",
                                                            width = "100%", 
                                                            height = 550)
                                     ),
                                     
                                     hr(),
                                     h3("What is LCLQ?"),
                                     h6("LCLQ stands for Local Colocation Quotient. It is a statistical analysis tool that helps to identify and quantify the extent to which certain features or events of interest are collocated in space. In other words, it helps to identify whether certain events or features are more likely to occur together in some locations than in others."),
                                     h4("How to interpret the map?"),
                                     h6("This map shows the Local Colocation Quotient (LCLQ) for the selected variable. LCLQ measures the extent to which features tend to be located near each other in space. A high LCLQ indicates a significant clustering of the features in the area, while a low LCLQ suggests a more random distribution.")
                                   )
                                 )),
                        
                      )),
             
             tabPanel("Spatial Point Analysis",
                        tabsetPanel(
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
                                       em("Please wait a short while for the default map to load."),
                                       tmapOutput("kde_map"),
                                       hr(),
                                       h3("What is Kernel Density Estimation?"),
                                       h6("Kernel Density Estimation (KDE) is a statistical method used to estimate and visualise the intensity of points in a selected geographic area."),
                                       h6("The KDE technique involves placing a kernel, which is a small smooth function, at each observation point in the spatial domain. The kernel function then spreads out from the observation point, and the total contribution of all the kernels gives an estimate of the underlying probability density at any point in the continuous space."),
                                       h3("How to interpret the map?"),
                                       h6("The letter v in the legend represents the quantity of objects located in the kernel window positioned at the center of each grid. In essence, if the color of the area is darker (the larger the value of v), it indicates a greater concentration of points in that particular region and vice versa.")
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
                                       hr(),
                                       actionButton(inputId = "run_g_function",
                                                    label = "Run Analysis")
                                     ),
                                     mainPanel(
                                       em("Please wait a short while for the default graph to load."),
                                       plotOutput("g_function_plot"),
                                       hr(),
                                       h3("What is G-Function?"),
                                       h6("The G-function measures the degree of clustering or dispersion of points relative to a random distribution, by calculating the expected number of points within a given distance of any point in the dataset. The G-function is a cumulative function and is calculated by adding up the number of points at increasing distances from each individual point."),
                                       h3("How to analyse this output?"),
                                       h6(strong("If the G-function lies above the envelope,"), "the null hypothesis is rejected, we can conclude that the value is statistically significant and that the points have a clustered distribution."),
                                       h6(strong("If the G-function lies below the envelope,"), "the null hypothesis is rejected, we can conclude that the value is statistically significant and that the points have a dispersed distribution."),
                                       h6(strong("If the G-function lies within the envelope,"), "the null hypothesis cannot be rejected, we can conclude that the value is not statistically significant and that the points have a random distribution.")
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
                                       helpText("99 simulations are being run by default"),
                                       hr(),
                                       actionButton(inputId = "run_l_function",
                                                    label = "Run Analysis")
                                     ),
                                     mainPanel(
                                       em("Please wait a short while for the default graph to load."),
                                       plotOutput("l_function_plot"),
                                       hr(),
                                       h3("What is L-Function?"),
                                       h6("L-function measures the degree of clustering or dispersion of points by comparing the observed density of points to the expected density under complete spatial randomness."),
                                       h3("How to analyse this output?"),
                                       h6(strong("If the L-function lies above the envelope,"), "the null hypothesis is rejected, we can conclude that the value is statistically significant and that the points have a clustered distribution."),
                                       h6(strong("If the L-function lies below the envelope,"), "the null hypothesis is rejected, we can conclude that the value is statistically significant and that the points have a dispersed distribution."),
                                       h6(strong("If the L-function lies within the envelope,"), "the null hypothesis cannot be rejected, we can conclude that the value is not statistically significant and that the points have a random distribution.")
                                       
                                     )
                                   ))
                        )
                      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  map_var <- reactive({
    if (input$variable == "Hawker"){
      hawker_sf
    } else if (input$variable == "HDB") {
      hdb_sf
    } else if (input$variable == "Carpark") {
      carpark_sf
    } 
    else if (input$variable == "Mall") {
      mall_sf
    }
  })
  
  ## Mapping ##
  output$mapPlot <- renderTmap({
    
    tmap_mode("view") 
      tm_basemap("OpenStreetMap")+
      tm_shape(sg) +
      tm_polygons() +
      tm_shape(map_var()) +
      tm_dots(size = input$dot_size,
              col = input$colour,
              border.col = "black",
              border.lwd = input$line_width) +
      tm_view(set.zoom.limits = c(11, 16))
  })
  
  
  #### LCLQ #### 
  
  output$lclq_plot <- renderTmap({
    
    input$LCLQ_run
    
    ## prepare vector list for LCLQ (main comparison variable)
    A <- reactive({
      if (input$lclq_var == "Hawker") {
        
        Carpark <- hk_cp_lclq %>%
          filter(Name == "Carpark")
        A <- hk_cp_lclq$Name
        
      } else if (input$lclq_var == "HDB") {
        
        Carpark <- hdb_cp_lclq %>%
          filter(Name == "Carpark")
        A <- hdb_cp_lclq$Name
        
      } else if (input$lclq_var == "Mall") {
        
        Carpark <- mall_cp_lclq %>%
          filter(Name == "Carpark")
        A <- mall_cp_lclq$Name
      }
    })
    
    ## prepare vector list for LCLQ (selected variable)
    B <- reactive({
      if (input$lclq_var == "Hawker") {
        
        Hawker <- hk_cp_lclq %>%
          filter(Name == "Hawker")
        B <- hk_cp_lclq$Name
        
      } else if (input$lclq_var == "HDB") {
        
        HDB <- hdb_cp_lclq %>%
          filter(Name == "HDB")
        B <- hdb_cp_lclq$Name
        
      } else if (input$lclq_var == "Mall") {
        
        Mall <- mall_cp_lclq %>%
          filter(Name == "Mall")
        B <- mall_cp_lclq$Name
      }
    })
    
    # Compute the nearest neighbours per selected
    nb <- reactive({
      
      if (input$lclq_var == "Hawker") {
        
        include_self(
          st_knn(st_geometry(hk_cp_lclq), input$nb_lclq))
        
      } else if (input$lclq_var == "HDB") {
        
        include_self(
          st_knn(st_geometry(hdb_cp_lclq), input$nb_lclq))
        
      } else if (input$lclq_var ==  "Mall") {
        
        include_self(
          st_knn(st_geometry(mall_cp_lclq), input$nb_lclq))
        
      }
      
    })
    
    # Compute the Kernel weights
    wt <- reactive({
      
      if (input$lclq_var == "Hawker") {
        
        st_kernel_weights(nb(), hk_cp_lclq, "gaussian", adaptive = TRUE)
        
      } else if (input$lclq_var == "HDB") {
        
        st_kernel_weights(nb(), hdb_cp_lclq, "gaussian", adaptive = TRUE)
        
      } else if (input$lclq_var == "Mall") {
        
        st_kernel_weights(nb(), mall_cp_lclq, "gaussian", adaptive = TRUE)
        
      }
      
    })
    
    # Determine the no. of simulations according to the selected significance level
    conf_int <- reactive({
      if (input$sig == 0.01){
        conf_int <- 49
      }
      else if (input$sig == 0.05){
        conf_int <- 39
      }
    })
    
    #Compute the LCLQ output
    LCLQ_output <- eventReactive(input$LCLQ_run, {
      isolate(
        if (input$lclq_var == "Hawker") {
          
          LCLQ <- local_colocation(A(), B(), nb(), wt(), conf_int())
          cbind(hk_cp_lclq, LCLQ)
          
        } else if (input$lclq_var == "HDB") {
          
          LCLQ <- local_colocation(A(), B(), nb(), wt(), conf_int())
          cbind(hdb_cp_lclq, LCLQ)
          
        } else if (input$lclq_var == "Mall") {
          
          LCLQ <- local_colocation(A(), B(), nb(), wt(), conf_int())
          cbind(mall_cp_lclq, LCLQ)
          
        }
      )
    })
    
    # Create the tmap plot
    tmap_mode("plot") +
      tm_shape(sg) +
      tm_polygons() +
      tm_shape(LCLQ_output()) +
      tm_dots(col = input$lclq_var,
              size = 0.08,
              border.col = "black",
              border.lwd = 0.5) +
      tm_view(set.zoom.limits = c(11, 16))
    
    
  })
  
  ### KDE Plot ###
  
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
                          tm_view(set.zoom.limits = c(11,14))) 
  })
  
  ### G Function Plot ###
  output$g_function_plot <- renderPlot({
    input$run_g_function
    g_func.csr <- isolate(envelope(carpark_ppp_km, Gest, nsim = input$g_funct_sim))
    plot(g_func.csr)
  })
  
  ### L Function Plot ###
  output$l_function_plot <- renderPlot({
    input$run_l_function
    l_func.csr <- isolate(envelope(carpark_ppp_km, Lest, nsim = input$l_funct_sim))
    plot(l_func.csr, . - r ~ r, xlab="d", ylab="L(d)-r")
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
