


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
  library(mapview)
  library(htmlwidgets)
  library(ggrepel)
  library(nCov2019)
  library(chinamap)
  library(plotly)
  library(tidyr)
  
  # directory
  setwd("~/Desktop/covid19")
}







# __________________________________________________________ Raw Data __________________________________________________________


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
covid_sa <- read_html(covid_sa_url) %>% html_node(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_table(fill=T)


# geospatial data
south_africa <- readShapeSpatial('data/gadm36_ZAF_shp/gadm36_ZAF_1.shp')
# ______________________________ South African Data ______________________________



# _________________________________ Global Data _________________________________
# global case data
n_cases <- load_nCov2019(lang='en')

# median population data
med_age <- read.csv('data/median-age.csv')

# clean
med_age <- group_by(med_age, Entity) %>% filter(Year==2020) %>%
  transmute(name=factor(Entity),
            code=Code,
            country=Entity,
            median_age=UN.Population.Division..Median.Age...2017...years.)
# _________________________________ Global Data _________________________________




# ______________________________________________________ Concatenate Data ______________________________________________________

# Extract number of cases
test <- t(covid_sa[covid_sa$Date=='Cases',1:10])
test <- data.frame(test[2:nrow(test),])
colnames(test) <- 'Cases'
test$Cases <- as.numeric(as.character(test$Cases))

# add to dataframe
south_africa@data <- cbind(test, south_africa@data)

# province data
south_africa@data <- merge(south_africa@data, provincial.data, by.x='NAME_1', by.y='province')






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







library('wppExplorer')



# ___________________________________________________ tmap: world diagnostics ___________________________________________________
library(tmap)
data("World")



# ______ ______ Curate Data ______ ______ 


# clean case data
case_data <- n_cases$global %>% group_by(country) %>% 
  summarise(cases=max(cum_confirm),
            cum_heal=max(cum_heal),
            cum_dead=max(cum_dead)) %>%
  mutate(name=factor(country))


  
# merge case data
world <- merge(World, case_data, by='name')

# merge median age data 
world <- merge(world, med_age, by='name')

  
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

# 'Italy', 'South Africa'

library(wpp2019)
data('pop')
population <- select(pop, c('name', '2015', '2020'))
head(population)
population[population$name=='South Africa',]




# ___________________________________________________ Circular Graphic: Case Distribution ___________________________________________________
world %>% filter(country=c('China')) %>%
  ggplot(aes(x='', y=))


filter(world, name==c('China')) %>%
  ggplot(aes(x='', y=c(cases, cum_heal, cum_dead))) +
  geom_bar(stat='identity', width=1) +
  coord_polar()

library(ggplot2)

data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Basic piechart
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)



# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to

# ___________________________________________________ Circular Graphic: Case Distribution ___________________________________________________




# _____ _____ _____ region _____ _____ _____
cn = get_map_china()
cn$province <- trans_province(cn$province)
plot(n_cases, region = 'South Africa')



# _____ _____ _____ cummulative cases _____ _____ _____
d <- subset(n_cases['global'], country == 'South Africa')
d <- gather(d, curve, count, -time, -country)
ggplot(d, aes(time, count, color = curve)) +
  geom_point() + geom_line() + xlab(NULL) + ylab(NULL) +
  theme_bw() + theme(legend.position = "none") +
  geom_text_repel(aes(label = curve), 
                  data = d[d$time == time(y), ], hjust = 1) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  scale_x_date(date_labels = "%Y-%m-%d", 
               limits = c(as.Date("2020-03-05"), as.Date("2020-03-20"))) +
  labs(title="South Africa: Total Confirmed, Recovered & Deaths") 


# _____ _____ _____ entire nation _____ _____ _____
plot(n_cases)











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





















