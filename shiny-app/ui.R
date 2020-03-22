
# Dependencies

# remotes::install_github("GuangchuangYu/nCov2019")
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
library(ggrepel)
setwd("~/Desktop/covid19")


countries <- read_csv('data/country_names.csv')$x


shinyUI(fluidPage(
    titlePanel("Let's Save the World, Together"),
    
    
    sidebarLayout(
        sidebarPanel(
            selectInput('con_a', 'Country 1', selected='South Africa', countries),
            selectInput('con_b', 'Country 3', selected='China', countries),
            selectInput('con_c', 'Country 4', selected='Italy', countries),
            selectInput('con_d', 'Country 2', selected='United States', countries)
        ),
        
        
        mainPanel(
            leafletOutput('south_africa'),
            br(),
            br(),
            leafletOutput('globe'),
            br(),
            br(),
            column(12, 
                   column(6,plotOutput('con_a')),
                   column(6,plotOutput('con_b')),
                   column(6,plotOutput('con_c')),
                   column(6,plotOutput('con_d')))
    
        )
    )
))














