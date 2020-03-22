


# ________________________________________________________ Dependencies ________________________________________________________

# remotes::install_github("GuangchuangYu/nCov2019")
# remotes::install_github("GuangchuangYu/chinamap")

configure_workspace <- function() {
  
  # dependencies
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
  
  # directory
  setwd("~/Desktop/covid19")
}







# __________________________________________________________ Raw Data __________________________________________________________

get_data <- function() {
  # run to fetch & clean all the data
  
  # ______________________________ South African Data ______________________________
  # provincial data
  province <- c('Western Cape','Eastern Cape','Northern Cape', 'Free State', 'KwaZulu-Natal', 'North West', 
                'Gauteng', 'Mpumalanga', 'Limpopo')
  population <- c(6621103, 6522734, 1225555, 2954348, 11384722, 3978955, 14717040, 4523874, 5797275)
  population_2 <- paste(round(population/1000000, 2), 'm', sep='')
  density <- c(51.1, 38.6, 3.3, 22.8, 120.7, 37.9, 809.6, 59.1, 46.1)
  provincial.data <- data.frame(province, population, population_2, density, stringsAsFactors=FALSE)
  
  # --- Add later ---               # Gender & Age demographics
  Age <- c('0-9', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')
  Male <- c('5 292 766','4 842 847','5 237 328','3 754 553','2 598 068','1 823 299','1 013 912','458 781','176 237')
  Female <- c('5 212 437', '4 755 516', '5 196 531', '2 751 224', '2 970 834', '2 192 398', '1 329 660', '770 816', '402 352')
  total <- c('10 505 203','9 598 363','10 433 859','6 505 777','5 568 902','4 015 697','2 343 572','1 229 597','419 989' )
  age.data <- data.frame(Age, Male, Female,total, stringsAsFactors=FALSE)
  # --- Add later ---
  
  # Get South African Covid Data
  covid_sa_url <- 'https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_South_Africa'
  covid_sa <<- read_html(covid_sa_url) %>% html_node(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_table(fill=T)
  
  # geospatial data
  south_africa <<- readShapeSpatial('data/gadm36_ZAF_shp/gadm36_ZAF_1.shp')
  # ______________________________ South African Data ______________________________
  
  
  
  # _________________________________ Global Data _________________________________
  # global case data
  n_cases <<- load_nCov2019(lang='en')
  
  # median population data
  med_age <<- read.csv('data/median-age.csv')
  
  # clean
  med_age <<- group_by(med_age, Entity) %>% filter(Year==2020) %>%
    transmute(name=factor(Entity),
              code=Code,
              country=Entity,
              median_age=UN.Population.Division..Median.Age...2017...years.)
  
  
  # Global Population Data
  global_population_url <- 'https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population'
  global_population <<- read_html(global_population_url) %>% html_node(xpath='//*[@id="mw-content-text"]/div/table') %>% html_table(fill=T)
  
  global_population <<- transmute(global_population, 
                                 'rank'=Rank, 
                                 'country'=`Country (or dependent territory)`,
                                 'population'=Population,
                                 'percent_of_population'=`% of worldpopulation`)
  
  
  global_population$population <<- as.numeric(gsub(',', '', global_population$population))
  global_population$pop_char <<- paste(round(global_population$population/1000000, 2), 'm', sep='')
  # _________________________________ Global Data _________________________________
  
  

  # _________________________________ Concatenate Data _________________________________
  # Extract number of cases
  test <- t(covid_sa[covid_sa$Date=='Cases',1:10])
  test <- data.frame(test[2:nrow(test),])
  colnames(test) <- 'Cases'
  # test$Cases <- as.numeric(as.character(test$Cases))
  
  # add to dataframe
  south_africa@data <<- cbind(test, south_africa@data)
  
  # province data
  south_africa@data <<- merge(south_africa@data, provincial.data, by.x='NAME_1', by.y='province')
  # _________________________________ Concatenate Data _________________________________
  
  
  # _________________________________ World Map Data _________________________________
  data("World")
  
  # clean case data
  case_data <- n_cases$global %>% group_by(country) %>% 
    summarise(cases=max(cum_confirm),
              cum_heal=max(cum_heal),
              cum_dead=max(cum_dead)) %>%
    mutate(name=factor(country))
  
  # merge case data
  world <- merge(World, case_data, by='name')
  
  # merge median age data 
  world <<- merge(world, med_age, by='name')
}



















# ______________________________________________________ Visualization ______________________________________________________


# _____ _____ _____ South Africa _____ _____ _____
tmap_mode('view')
tmap_mode('plot')

map <- tm_shape(south_africa) + 
  tm_layout(title='Confirmed Cases in SA') + 
  tm_borders(alpha=0.3) +
  tm_fill('Cases', id='NAME_1', title='Confirmed Cases', palette=c(RColorBrewer::brewer.pal(9, 'OrRd')), 
          popup.vars=c('cases'="Cases", "population", 'pop density'="density"))
map <- tmap_leaflet(map)
saveWidget(widget=map, 'south_africa_map.html')
# _____ _____ _____ South Africa _____ _____ _____



# _____ _____ _____ Growth by Country _____ _____ _____
confirmed_cases_growth <- function(y, countries) {
  ""
  d <- subset(y['global'], country == countries)
  
  ggplot(d, aes(time, as.numeric(cum_confirm), group = country, color = country)) + 
    geom_point() + geom_line() +
    geom_label_repel(aes(label = country), 
                     data = d[d$time == time(y), ], hjust = 1) +
    theme_bw() + theme(legend.position = 'none') +
    xlab(NULL) + ylab(NULL) + 
    scale_x_date(date_labels = "%Y-%m-%d",
                 limits = c(as.Date("2020-03-12"), as.Date("2020-03-19"))) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
    labs(title = "Outbreak Trend Curves of Countries Around the World")
}

cases <- confirmed_cases_growth(n_cases, c("South Africa"))
ggsave('cases.png')  

saveWidget(ggplotly(cases), 'n_cases.html')                    # not working
save_html(file=cases, html='n_cases.html')                     # not working
# _____ _____ _____ Growth by Country _____ _____ _____                                                   



# _____ _____ _____ Flatten the Curve _____ _____ _____
flatten_the_curve <- function(n_cases, countries) {
  growth_rate <- n_cases$global %>% group_by(country) %>% 
    mutate(normalized_cum = (cum_confirm-min(cum_confirm))/max(cum_confirm))
  
  # select countries of interest
  growth_rate <- growth_rate[growth_rate$country==countries,]
  
  # remove country = NA rows
  growth_rate <- growth_rate[!is.na(growth_rate$country),]
  
  ggplot(growth_rate, aes(time, as.numeric(normalized_cum), group=country, color=country)) + 
    geom_line() + ylab('Growth in Cases') +  xlab('Date') + ggtitle('Who can Flatten the Curve?') +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank())
  
}
flatten_the_curve(n_cases, c('China', 'Italy', 'Iran', 'United States', 'South Africa'))
# _____ _____ _____ Flatten the Curve _____ _____ _____  

# _____ _____ _____ Growth on Log Scale _____ _____ _____
filter(n_cases$global, country==c('China', 'South Africa', 'United States', 'Italy')) %>%
  ggplot(aes(time, cum_confirm, group=country, color=country)) + 
  geom_line() + ylab('Growth in Cases') +  xlab('Date') + ggtitle('Who can Flatten the Curve?') +
  scale_y_log10() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank())
# _____ _____ _____ Growth Curve per Nation _____ _____ _____










# ___________________________________________________ tmap: world diagnostics ___________________________________________________
tmap_mode('view')

tm_shape(world) +
  tm_layout(title='Understand the World') + 
  tm_borders(alpha=0.3) +
  tm_fill('cases', title='Cases', palette=c(RColorBrewer::brewer.pal(10,'Blues')[5:9]),
          popup.vars=c('cases', 'cured'='cum_heal', 'fatalities'='cum_dead', 'median age'='median_age', 'population'='pop_est'))

tm_shape(world) +
  tm_layout(title='Understand the World') + 
  tm_borders(alpha=0.3) +
  tm_fill('cases', title='Cases',breaks=c(0,50,100,200,500,1000,5000,10000,100000),
          palette=c(RColorBrewer::brewer.pal(10,'Blues')[5:9]),
          popup.vars=c('cases', 'cured'='cum_heal', 'fatalities'='cum_dead', 'median age'='median_age', 'population'='pop_est'))

# ___________________________________________________ tmap: world diagnostics ___________________________________________________






# ___________________________________________________ Circular Graphic: Case Distribution ___________________________________________________
active_ratio_donut_graph <- function(world, country) {
 # Return an active/death/recovered donut graph a given country
  
  x <- filter(world, name==country)                                         # filter data  
  x$active <- (x$cases - x$cum_heal - x$cum_dead)                           # compute active 
  data <- data.frame(country=country,                                       # Create data
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

active_ratio_donut_graph(world, "Italy")
# ___________________________________________________ Circular Graphic: Case Distribution ___________________________________________________






# ___________________________________________________ Growth in Case Diagnostics ___________________________________________________

effectiveness_of_response_graph <- function(n_cases, country_) {
  
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

effectiveness_of_response_graph(n_cases, "United States")
# ___________________________________________________ Growth in Case Diagnostics ___________________________________________________



# ___________________________________________________ Deaths Per Day ___________________________________________________













 

 



#________________________RSA MAP (no provincial data)_________________________________

#____________________________Tracking over time _________________________________


# example 
# https://stackoverflow.com/questions/33041266/r-rworldmap-map-issue-and-leaflet-application




# Notes for Keagan

Another option is to use these Graphs to Generate Web Pages (via NETIFY) ---> HTML & THEREAFTER EMBED the Webpages  in the WIX Site !!!!

# Covid Outreach Script 

The Country is Suffering - We want to make a Contribution. 

Covid is fundamentally an asyncronous information problem:
  The better we are able to understand the situation, the better we are able to:
    - Address the Problems we face: distribute limited resources, medical & otherwise
    - Prevent Catastrophy: better information provides insight, allowing us to plan as a society. 

We`ve started by summarising Covid - in a South African context.

www.Smarter_Than_Covid.org

or download the app at:

  Smarter_Than_Covid


We need YOUR help! There`s a fortune of work todo. DM us if you want to contribute. 
& NO, you don`t need to be an engineer/programmer. 



### Wave 2 --> solicit information





















