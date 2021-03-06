




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
library(rgdal)
setwd("~/Desktop/covid19")


countries <- read_csv('data/country_names.csv')$x


shinyUI(fluidPage(
    titlePanel("Understand the Situation"),
    
    
    sidebarLayout(
        
        sidebarPanel(
            width=3,
            selectInput('con_a', 'Country 1', selected='South Africa', countries),
            selectInput('con_b', 'Country 2', selected='China', countries),
            selectInput('con_c', 'Country 3', selected='Italy', countries),
            selectInput('con_d', 'Country 4', selected='United States', countries)
        ),
        
        
        mainPanel(
            
            tabsetPanel(type='tabs',
                        tabPanel('South Africa',
                                 leafletOutput('south_africa'),
                                 br(),
                                 br(),
                                 leafletOutput('globe')),
                        tabPanel('Country Comparison',
                                 column(12, 
                                        column(6,plotOutput('con_a')),
                                        column(6,plotOutput('con_b')),
                                        column(6,plotOutput('con_c')),
                                        column(6,plotOutput('con_d'))),
                                 plotOutput('eff_a'),
                                 plotOutput('eff_b'),
                                 plotOutput('eff_c'),
                                 plotOutput('eff_d'),
                                 br(),
                                 br(),
                                 plotOutput('det_a'),
                                 plotOutput('det_b'),
                                 plotOutput('det_c'),
                                 plotOutput('det_d')),
                        tabPanel('Globe')),
     
            
        )
    )
))
















