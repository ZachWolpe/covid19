



library(shiny)

shinyServer(function(input, output) {
    
    
    # _______________________________________________________________________________________________________________________________________
    #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Fetch Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # _______________________________________________________________________________________________________________________________________
    
    
    # ______________________________ South African Data ______________________________
    provincial_data <- reactive({
        # provincial data
        province <- c('Western Cape','Eastern Cape','Northern Cape', 'Free State', 'KwaZulu-Natal', 'North West', 
                      'Gauteng', 'Mpumalanga', 'Limpopo')
        population <- c(6621103, 6522734, 1225555, 2954348, 11384722, 3978955, 14717040, 4523874, 5797275)
        population_2 <- paste(round(population/1000000, 2), 'm', sep='')
        density <- c(51.1, 38.6, 3.3, 22.8, 120.7, 37.9, 809.6, 59.1, 46.1)
        provincial.data <- data.frame(province, population, population_2, density, stringsAsFactors=FALSE)
        provincial.data
    })
    
    
    # --- Add later ---               # Gender & Age demographics
  #  Age <- c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')
  #  Male <- c('5 292 766','4 842 847','5 237 328','3 754 553','2 598 068','1 823 299','1 013 912','458 781','176 237')
  #  Female <- c('5 212 437', '4 755 516', '5 196 531', '2 751 224', '2 970 834', '2 192 398', '1 329 660', '770 816', '402 352')
  #  total <- c('10 505 203','9 598 363','10 433 859','6 505 777','5 568 902','4 015 697','2 343 572','1 229 597','419 989' )
  #  age.data <- data.frame(Age, Male, Female,total, stringsAsFactors=FALSE)
    # --- Add later ---
    
    
    
    covid_sa <- reactive({
        # SA covid data
        covid_sa_url <- 'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_South_Africa'
        covid_sa <- read_html(covid_sa_url) %>% html_node(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_table(fill=T)
        covid_sa})

    sa_map <- reactive({
        # SA map data
        raw_map <- readShapeSpatial('data/gadm36_ZAF_shp/gadm36_ZAF_1.shp')
        
        # Extract number of cases
        data <- t(covid_sa()[covid_sa()$Date=='Cases',1:10])
        data <- data.frame(data[2:nrow(data),])
        colnames(data) <- 'Cases'
        data$Cases <- as.numeric(as.character(data$Cases))
        
        # add to dataframe
        raw_map@data <- cbind(data, raw_map@data)
        
        # province data
        raw_map@data <- merge(raw_map@data, provincial_data(), by.x='NAME_1', by.y='province')
        
        raw_map})
    # ______________________________ South African Data ______________________________
    
    
    
    
    # _________________________________ Global Data _________________________________
    # global case data
    n_cases <- reactive(load_nCov2019(lang='en')) 
    
    
    med_age <- reactive({
        # median population data
        med_age <- read.csv('data/median-age.csv')
        med_age <- group_by(med_age, Entity) %>% filter(Year==2020) %>%
            transmute(name=factor(Entity),
                      code=Code,
                      country=Entity,
                      median_age=UN.Population.Division..Median.Age...2017...years.)})
    
    world_data <- reactive({
        # produce world dataset
        data("World")
        
        # clean case data
        case_data <- n_cases()$global %>% group_by(country) %>% 
            summarise(cases=max(cum_confirm),
                      cum_heal=max(cum_heal),
                      cum_dead=max(cum_dead)) %>%
            mutate(name=factor(country))
    
        world <- merge(World, case_data, by='name')           # merge case data
        world <- merge(world, med_age(), by='name')           # merge median age data
        world
    })
    # _________________________________ Global Data _________________________________
    
    
    # _______________________________________________________________________________________________________________________________________
    #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Fetch Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # _______________________________________________________________________________________________________________________________________
    
    
    
    
    
    # _______________________________________________________________________________________________________________________________________
    #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Generate Graphs - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # _______________________________________________________________________________________________________________________________________
    
    output$south_africa <- renderLeaflet({
        # SA map
        map <- tm_shape(sa_map()) + 
            tm_layout(title='Confirmed Cases in SA') + 
            tm_borders(alpha=0.3) +
            tm_fill('Cases', title='Confirmed Cases', palette=c((RColorBrewer::brewer.pal(10, 'OrRd')[2:7])), 
                    popup.vars=c('cases'="Cases", "population", 'pop density'="density"))
        tmap_leaflet(map)
    })
    
    
    
    output$globe <- renderLeaflet({
        # global map
        tmap <- tm_shape(world_data()) +
            tm_layout(title='Understand the World') + 
            tm_borders(alpha=0.3) +
            tm_fill('cases', title='Cases', palette=c(RColorBrewer::brewer.pal(10,'Blues')[5:9]),
                    breaks=c(0,50,100,200,500,1000,5000,10000,100000),
                    popup.vars=c('cases', 'cured'='cum_heal', 'fatalities'='cum_dead', 'median age'='median_age', 'population'='pop_est'))
        tmap_leaflet(tmap)
    })
        
        
    # _______________________________________________________________________________________________________________________________________
    #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Generate Graphs - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # _______________________________________________________________________________________________________________________________________
    
    

})





























