





# _______________________________________________________________________________________________________________________________________
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - External Functions - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# _______________________________________________________________________________________________________________________________________


active_ratio_donut_graph <- function(world, country_) {
  # Circular Graphic: Case Distribution
  
  x <- filter(world, name==country_)                                         # filter data  
  x$active <- (x$cases - x$cum_heal - x$cum_dead)                           # compute active 
  data <- data.frame(country=country_,                                       # Create data
                     category=c("Active", "Deaths", "Recovered"), 
                     count=c(x$active, x$cum_dead, x$cum_heal))
  
  data$fraction <- data$count / sum(data$count)                             # Compute percentages
  data$ymax <- cumsum(data$fraction)                                        # Compute the cumulative percentages (top of each rectangle)
  data$ymin <- c(0, head(data$ymax, n=-1))                                  # Compute the bottom of each rectangle
  data$labelPosition <- (data$ymax + data$ymin) / 2                         # Compute label position
  data$label <- paste0(data$category, "\n", data$count)                     # Compute a good label
  
  # Make the plot
  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
    geom_rect() +
    geom_text_repel(x=1, aes(y=labelPosition, label=label, color=category), size=6) +
    # geom_text() + # x here controls label position (inner / outer)
    scale_fill_brewer(palette=3) +
    scale_color_brewer(palette=3) +
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = paste("Covid-19", data$country, "Summary")) +
    theme(plot.title = element_text(hjust = 0.5, size = 18, colour = 'darkgrey'))
}


effectiveness_of_response_graph <- function(n_cases, country_) {
  # Growth in Case Diagnostics
  
  x <- filter(n_cases$global, country==country_) 
  x$cum_open <- x$cum_confirm - x$cum_heal - x$cum_dead
  
  data <- data.frame()
  
  for (i in 1:nrow(x)) {
    # for each time step (row)
    d <- rbind(
      data.frame(time=x$time[i], country=x$country[i], group='cum_heal', count=x$cum_heal[i]),
      data.frame(time=x$time[i], country=x$country[i], group='cum_dead', count=x$cum_dead[i]),
      data.frame(time=x$time[i], country=x$country[i], group='cum_open', count=x$cum_open[i]))
    data <- rbind(data,d)
  }
  
  data %>%
    ggplot(aes(x=time, y=count, fill=group)) + 
    geom_area(colour="grey", size=.2, alpha=.9) +
    scale_fill_brewer(palette="Blues",
                      name="Cases",
                      breaks=c("cum_heal", "cum_dead", "cum_open"),
                      labels=c("Recovered", "Deaths", "Active")) +
    theme_minimal() +
    ggtitle(paste('Effectiveness of Response:', country_)) + 
    xlab('Date') +
    ylab('Cases') 
}


deaths_per_day_graph <- function(n_cases, country_) {
  # Deaths Per Day
  
  x <- n_cases$global[n_cases$global$country==country_,]
  x <- x[!is.na(x$country),]
  
  d <- c(0)
  for (i in 2:length(x$cum_dead)) d <- c(d, x$cum_dead[i]-x$cum_dead[i-1])
  
  x$deaths_per_day <- d
  
  ggplot(x, aes(x=time, y=deaths_per_day)) + 
    geom_point(size=1, col='darkred') +
    geom_segment(aes(x=time, xend=time, y=0, yend=deaths_per_day),  col='darkred') +
    ggtitle(paste('Deaths per day in', country_)) +
    ylab('Deaths') + xlab('Date') +
    theme_minimal()
}


# _______________________________________________________________________________________________________________________________________
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - External Functions - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# _______________________________________________________________________________________________________________________________________










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
        # raw_map <- readShapeSpatial('data/gadm36_ZAF_shp/gadm36_ZAF_1.shp')
        # raw_map <- sf::st_read('data/gadm36_ZAF_shp/gadm36_ZAF_1.shp')
        raw_map <- rgdal::readOGR('data/gadm36_ZAF_shp/gadm36_ZAF_1.shp')
        
        # Extract number of cases
        data <- t(covid_sa()[covid_sa()$Day=='Cases',3:11])
        data <- data.frame(data[1:nrow(data),])
        colnames(data) <- 'Cases'
        data$Cases <- as.numeric(as.character(data$Cases))
        
        raw_map@data <- cbind(data, raw_map@data)                                                    # add to dataframe
        raw_map@data <- merge(raw_map@data, provincial_data(), by.x='NAME_1', by.y='province')       # province data
        raw_map})
    # ______________________________ South African Data ______________________________
    
    
    
    
    # _________________________________ Global Data _________________________________
    # global case data
    n_cases <- reactive(load_nCov2019(lang='en')) 
    
    # global population data
    global_population <- reactive(read.csv('./data/global_population.csv'))

    
    
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
    
        world <- merge(World, case_data, by='name')                           # merge case data
        world <- merge(world, med_age(), by='name')                           # merge median age data
        world <- merge(world, global_population(), by='name', all.x=TRUE)     # add global population data 
        world
    })
    # _________________________________ Global Data _________________________________
    
    
    # _______________________________________________________________________________________________________________________________________
    #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Fetch Data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # _______________________________________________________________________________________________________________________________________

    
    
    
    # _______________________________________________________________________________________________________________________________________
    #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Generate Graphs - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    # _______________________________________________________________________________________________________________________________________
    
    # SA map
    output$south_africa <- renderLeaflet({
        map <- tm_shape(sa_map()) + 
            tm_layout(title='Confirmed Cases in SA') + 
            tm_borders(alpha=0.3) +
            tm_fill('Cases', title='Confirmed Cases', palette=c(RColorBrewer::brewer.pal(10, 'OrRd')[3:7]),
                    breaks=c(0,10,20,50,100,200),
                    popup.vars=c('cases'="Cases", "population_2", 'pop density'="density"))
        tmap_leaflet(map)
    })
    
    
    # circular country graphs
    output$con_a <- renderPlot(active_ratio_donut_graph(world_data(), input$con_a))
    output$con_b <- renderPlot(active_ratio_donut_graph(world_data(), input$con_b))
    output$con_c <- renderPlot(active_ratio_donut_graph(world_data(), input$con_c))
    output$con_d <- renderPlot(active_ratio_donut_graph(world_data(), input$con_d))
    
    
    # global map
    output$globe <- renderLeaflet({
        tmap <- tm_shape(world_data()) +
            tm_layout(title='Understand the World') + 
            tm_borders(alpha=0.3) +
            tm_fill('cases', title='Cases', palette=c(RColorBrewer::brewer.pal(10,'Blues')[5:9]),
                    breaks=c(0,50,100,200,500,1000,5000,10000,100000),
                    popup.vars=c('cases', 'cured'='cum_heal', 'fatalities'='cum_dead', 'median age'='median_age', 'population'='pop_char'))
        tmap_leaflet(tmap)
    })
    
    
    # Growth in Case Diagnostics
    output$eff_a <- renderPlot(effectiveness_of_response_graph(n_cases(), input$con_a))
    output$eff_b <- renderPlot(effectiveness_of_response_graph(n_cases(), input$con_b))
    output$eff_c <- renderPlot(effectiveness_of_response_graph(n_cases(), input$con_c))
    output$eff_d <- renderPlot(effectiveness_of_response_graph(n_cases(), input$con_d))
    
    
    # Deaths per Day
    output$det_a <- renderPlot(deaths_per_day_graph(n_cases(), input$con_a))
    output$det_b <- renderPlot(deaths_per_day_graph(n_cases(), input$con_b))
    output$det_c <- renderPlot(deaths_per_day_graph(n_cases(), input$con_c))
    output$det_d <- renderPlot(deaths_per_day_graph(n_cases(), input$con_d))
        
        
    # _______________________________________________________________________________________________________________________________________
    #  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - Generate Graphs - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    # _______________________________________________________________________________________________________________________________________

})







































