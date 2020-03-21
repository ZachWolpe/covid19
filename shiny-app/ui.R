
# Dependencies
# library(mapview)
library(shiny)
library(shiny)
library(tidyverse)
library(coronavirus)
library(sf)
library(raster)
library(htmltab)
library(tmap)    
library(leaflet) 
library(ggplot2) 
library(maptools)
library(rvest)
library(RColorBrewer)
library(htmlwidgets)
library(ggrepel)
library(nCov2019)
library(chinamap)
library(plotly)
library(tidyr)
setwd("~/Desktop/covid19")


countries <- read_csv('data/country_names.csv')$x


shinyUI(fluidPage(
    titlePanel("Let's Save the World, Together"),
    
    
    sidebarLayout(
        sidebarPanel(
            selectizeInput('select', 'Countries', countries, multiple=T)
        ),
        
        
        mainPanel(
            leafletOutput('south_africa'),
            br(),
            br(),
            leafletOutput('globe')
        )
    )
))














