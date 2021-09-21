
#Storing things

#x = 2
x <- 3

#functions

#c() binds things together in a vector

y <- c(1,3,2,2)

mean(y)

sort(y, 
     TRUE)

#help
?sort

#install package
install.packages("tidyverse")
install.packages("quantreg")
install.packages("ballr")
#load the library

#fatty libraries
library(tidyverse)
library(nycflights13)
library(lubridate)
library(gridExtra)
library(rvest)
library(tidytext)
library(data.table)
library(stringr)
library(leaflet)
library(geojsonio)
library(textdata)
library(shiny)
library(readxl)
library(ggimage)
library(rtweet)
library(spotifyr)
library(readxl)
library(quantreg)
library(jobbR)
library(ggbeeswarm)
library(kableExtra)
library(nbastatR)
library(tigris)
library(tidycensus)

bref
#access the diamonds data set

View(diamonds)

#filter

nice <- filter(.data = diamonds,
               price > 15000,
               cut == "Premium")

#pipe operator %>%

y %>% mean()

#arrange function

nice2 <- diamonds %>%
  filter(price > 15000) %>%
  filter(cut == "Premium") %>%
  filter(carat > 2) %>% 
  filter(color == "G")
  arrange(-carat)
  
nice2


#mutate
#adds new column to data set

diamonds %>%
  mutate(volume = x*y*z) %>%
  arrange(-volume)

#summarize
#summarizes data into single value

diamonds %>%
  summarize(mean(price))

#group_by()
#groups data via a categorical variable

diamonds %>% 
  group_by(color, cut) %>%
  summarise(meanPrice = mean(price))

#neato filter tricks

diamonds %>%
  filter(color == "D" | color == "E" | color == "F")

diamonds %>%
  filter(color %in% c("D","E","F"))

#practice question

#loading data from nycflights13

install.packages('nycflights13')

flights %>%
  ggplot(aes(x = dep_time,
             y = arr_time)) +
  geom_point()

View(flights)
flights %>% 
  filter(dest %in% c("BTV", "BOS"))
          
flights %>% 
  group_by(dest)  %>%
  filter(dest == "BOS" | dest == "BTV") %>%
  filter(dep_delay > 0) %>%
  summarise(mean(dep_delay, na.rm = TRUE))

flights %>%
  group_by(dest)  %>%
  filter(dest == "BOS" | dest == "BTV")
  #mutate(ifelse(dep_delay, TRUE, FALSE))
  
  
  

flights %>%
  group_by(dest)  %>%
  filter(dest == "BOS" | dest == "BTV") %>%
  mutate(is_delayed = ifelse(dep_delay > 0, 1, 0)) %>%
  #select(dep_delay, is_delayed)
  summarize(mean(is_delayed, na.rm = TRUE))
  
# data visualization
#library(ggplot2)


diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point()

install.packages("plotly")

#color

diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(size = .4, color = "thistle")

#changing color in a fun way

diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point(size = .4, aes(color = clarity))

#add themes

diamonds %>%
  ggplot(mapping = aes(x = carat,
                       y = price)) +
  geom_point( aes(color = clarity,
                  shape = cut)) +
  theme_bw()

#histogram

diamonds %>%
  ggplot(mapping = aes(x = price)) +
  geom_histogram(color = "black",
                 fill = "blue",
                 bins = 100) +
  theme_bw()

#zoom in on data
diamonds %>%
  ggplot(mapping = aes(x = price)) +
  geom_histogram(color = "black",
                 fill = "blue",
                 bins = 30) +
  theme_bw() +
  xlim(1300,1700)


#2 questions to ask before making graphs

# how many variables am I trying to show?

#what types of variables are they?

diamonds %>%
  ggplot(mapping = aes(x = color, 
                       y = price)) +
  geom_boxplot()

flights

flights %>%
  ggplot(mapping = aes(x = origin,
                       y = sched_arr_time)) +
  geom_violin(aes(fill = origin)) +
  scale_fill_manual(values = c("red",
                               "white",
                               "blue"))

flights %>%
  ggplot(mapping = aes(x = origin,
                       y = sched_arr_time)) +
  geom_violin(aes(fill = origin)) +
  scale_fill_brewer(palette = "Spectral")



flights %>%
  mutate(is_delayed = ifelse(dep_delay > 0, "delayed", "not delayed")) %>%
  ggplot(mapping = aes(x = carrier))+
  geom_bar(aes(fill(is_delayed)))

# flights %>%
#   filter(!is.na(dep_delay)) %>%
#   
#     

flights %>%
  group_by(origin) %>%
  summarise(avg.air.time = mean(air_time,
                                na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = reorder(origin,
                                   - avg.air.time),
                       y = avg.air.time)) +
  geom_bar(aes(fill = origin),
               stat = "identity") +
  scale_fill_brewer(palette = 15)

climate.data <- read_csv("GlobalLandTemperaturesByCountry.csv")

climate.data


#lets show the relationship of average temperatures over time

#x = date
#y = avg temp
#lines for each country

climate.data %>%
  ggplot(aes(x = dt,
          y = AverageTemperature)) +
  geom_line(aes(color = Country)) +
  theme(legend.position = "none")



climate.data %>%
  mutate(year = year(dt)) %>%
  filter(year > 1900) %>%
  group_by(year) %>%
  summarise(avg.yearly.temp = mean(AverageTemperature, na.rm = TRUE)) %>%
    ggplot(aes(x = year,
               y = avg.yearly.temp)) +
    geom_line() 


climate.data %>%
  mutate(year = year(dt)) %>%
  filter(year > 1900) %>%
  group_by(year) %>%
  summarise(avg.yearly.temp = mean(AverageTemperature, na.rm = TRUE)) %>%
  ggplot(aes(x = year,
             y = avg.yearly.temp)) +
  geom_line(linetype = 5) +
  geom_point(aes(color = avg.yearly.temp)) +
  scale_color_gradient(low = "blue", high = "red")



#reddit usernames
install.packages('data.table')


reddit.data <- fread("RedditUsersFull(4).csv")




#finding usernames with an 'a'

str_detect(str_to_lower(reddit.subset$author), 'a')

str_detect(reddit.subset$author, 'a|A')

str_detect(reddit.subset$author,
           regex('a',
                 ignore_case = TRUE))

#lets identify usernames with ANY digit

str_detect(reddit.subset$author,
           "[:digit:]")


reddit.subset <- reddit.data %>%
  head(250000)

reddit.subset %>%
  mutate(has.digit = ifelse(str_detect(reddit.subset$author, "[:digit:]"), 
                            1,
                            0)) %>%
  summarise(mean(has.digit))

#count the number of digits in each username (on avg)

str_count(reddit.subset$author,
             '[:digit:]')

reddit.subset %>%
  mutate(num.digits = str_count(reddit.subset$author,
                                '[:digit:]')) %>%
  summarise(mean(num.digits))

reddit.with.digits <- reddit.data %>%
  filter(str_detect(reddit.data$author, '[:digit:]'))

reddit.with.digits %>%
  mutate(num.digits = str_count(reddit.with.digits$author,
                                '[:digit:]')) %>%
  summarise(mean(num.digits))

#Calculate distribution of digits in reddit usernames

reddit.with.digits %>%
  mutate(zero.count = str_count(author, )) %>%
  summarise(total.zero.count = sum(zero.count))

#for loops

for(i in 0:9) {
  rand.value <- i
}

#indexing a vector and data.frame

toy.vector <- c(1, 3, 5, 11, 13)

toy.vector[2]
toy.vector[2:4]
toy.vector[2]

#data set

diamonds[1:4,5:9]

diamonds[1,1]
diamonds[[1,1]]

#vector of the numbers 0-9
ten.loop <- NULL


for(i in 0:9){
  ten.loop[i] <- i
}

digit.vector = NULL

for(i in 0:9){
  digit.vector[i+1] <- reddit.with.digits %>%
    mutate(digit.count = str_count(author, toString(i))) %>%
    summarise(digit.count = sum(digit.count)) %>%
    .[[1]]
  print(i)
}

digit.vector

#create data set for graphing

digit.data <- data.frame(count = digit.vector, digit = 0:9)

digit.data %>%
  ggplot(aes(x = factor(digit), y = count)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  ylab("Count (in millions)") +
  scale_y_continuous(labels = c(0,1,2,3),
                     breaks = c(0,1000000,2000000,3000000)) +
  xlab("Digit")

reddit.with.digits %>%
  summarise(sum(str_count(reddit.with.digits$author,
                           '69')))

reddit100 <- reddit.data %>%
  head(100)

str_detect(reddit100$author,
           "[:digit:][:digit:]$")  

reddit100 %>%
  filter(str_detect(reddit100$author,
                    "([:alpha:]|[:punct:])[:digit:]{2}$")  )

reddit.with.digits %>%
  mutate(length = str_length(author)) %>%
  filter(length <= 2)

two.digit.data <- reddit.with.digits %>%
  filter(str_detect(reddit.with.digits$author,
                    "([:alpha:]|[:punct:])[:digit:]{2}$")  )


str_sub("alex99",
        start = -2,
        end = -1)

numbers <- str_sub(two.digit.data$author,
        start = -2,
        end = -1)

numbers.data.frame <- data.frame(digits = as.numeric(numbers))

numbers.data.frame %>%
  ggplot(aes(x = digits)) +
  geom_bar() +
  scale_x_continuous(limits = c(60,99),
                     breaks = 60:99)

#profiles

profiles <- fread("Profiles(4).csv")

profiles %>%
  ggplot(aes(x = height)) +
  geom_histogram() +
  scale_x_continuous(limits = c(48,84))

summary(profiles$height)

profiles %>%
  ggplot(aes(x = drugs)) +
  geom_bar() 

profiles %>%
  mutate(num.languages = str_count(profiles$speaks, ",") + 1) %>%
  group_by(num.languages) %>%
  ggplot(aes(x = age, y = mean(num.languages))) +
  geom_point()


#joining data in R

toy.data1 <- data.frame(name = c('alex',
                                    'bob',
                                    'bill'),
                           color = c('red',
                                     'blue',
                                     'green'))

toy.data2 <- data.frame(Names = c('alex',
                                    'frank',
                                    'bill'),
                           salary = c(5,
                                     3,
                                     2))

left_join(toy.data1, toy.data2)
                           
inner_join(toy.data1, toy.data2)

full_join(toy.data1, toy.data2,
          by = c('name' = 'Names'))

#lets look at faceting

profiles %>%
  ggplot(aes(x = height)) +
  geom_histogram() +
  scale_x_continuous(limits = c(48,84)) + 
  facet_wrap(~sex)

profiles %>%
  ggplot(aes(x = height)) +
  geom_histogram() +
  scale_x_continuous(limits = c(48,84)) + 
  facet_grid(drinks~sex)

profiles %>%
  mutate(num.languages = str_count(profiles$speaks, ",") + 1) %>%
  filter(age < 70) %>%
  group_by(age) %>%
  summarise(avg.num.lang = mean(num.languages)) %>%
  ggplot(aes(x = age, y = avg.num.lang)) +
  geom_point() 
  

profiles %>%
  ggplot(aes(x = body_type)) +
  geom_bar(aes(fill = body_type)) +
  facet_wrap(~sex) +
  coord_flip()

profiles <- file.choose()

profiles %>% 
  mutate(family = ifelse(str_detect(profiles$essay5, "family")))

profiles %>%
  ggplot(aes(x = sex)) +
  geom_bar(aes(fill = body_type),
           position = "fill")

profiles2 <- profiles

#reorder drinks column to make sense

profiles2$drinks <- factor(profiles2$drinks,
                           levels = c("", "not at all", 
                                      "rarely", 
                                      "socially", 
                                      "often", 
                                      "very often", 
                                      "desperately"))

profiles2 %>%
  filter(age < 80) %>%
  ggplot(aes( x = drinks, y = age)) +
  geom_boxplot(aes(fill = sex)) 


#web scraping

install.packages("rvest")


url1 <- profiles2$drinks <- factor(profiles2$drinks,
                                   levels = c("", "not at all", 
                                              "rarely", 
                                              "socially", 
                                              "often",
                                              "very often",
                                              "desperately"))

profiles2 %>%
  filter(age < 80) %>%
  ggplot(aes( x = drinks, y = age)) +
  geom_boxplot(aes(fill = sex)) 


#web scraping

install.packages("rvest")

url1 <- "https://en.m.wikipedia.org/wiki/Baker_Mayfield"

url1 %>%
  read_html() %>%
  html_nodes(xpath = '')

# make a copy

num.with.commas <- c("1,000", "2,000")

as.numeric(num.with.commas)

num.with.commas.without.commas <- str_replace_all(num.with.commas,
                ",",
                "")

as.numeric(num.with.commas.without.commas)

#`` for variable names with spaces

cnn.url <- "https://www.cnn.com/2019/10/03/politics/us-envoys-trump-ukraine-investigate/index.html"

cnn.text <- cnn.url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="body-text"]/div[1]') %>%
  html_text()

bbc.url <- "https://www.bbc.com/news/world-us-canada-40104063"


bbc.text <- bbc.url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="page"]/div[1]') %>%
  html_text()
  

cnn.url <- 'https://www.cnn.com/2017/05/31/politics/donald-trump-covfefe/index.h
tml"https://www.cnn.com/2019/10/03/politics/us-envoys-trump-ukraine-investigate/index.html"'

cnn.text <- cnn.url %>%
  read_html() %>%
  html_nodes(xpath = '/html/body/div[8]/article/div[1]') %>%
  html_text()

install.packages("tidytext")

trump.url <- "https://en.m.wikipedia.org/wiki/Donald_Trump"

trump.text <- trump.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text()

#convert vector to text
trump.data <- data.frame(text = trump.text,
                         stringsAsFactors = FALSE)


#tokenize my data
trump.data %>%
  unnest_tokens("word", "text")


#count up tokens
trump.data %>%
  unnest_tokens("word", "text") %>%
  group_by(word) %>%
  summarise(count = n()) %>%
  arrange(-count)

#count function

trump.data %>%
  unnest_tokens("word", "text") %>%
  count(word) %>%
  arrange(-n)

#removing stop words

stop_words

#anti-join

trump.data %>%
  unnest_tokens("word", "text") %>%
  count(word) %>%
  arrange(-n) %>%
  anti_join(stop_words) %>%
  head(10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text = element_text(size = 18))

install.packages("gridExtra")


#sentiment analysis

#highlight 3 sentiment dictionaries
install.packages("textdata")

get_sentiments("afinn") %>%
  View() 

get_sentiments("bing") %>%
  View()

get_sentiments("nrc") %>%
  View()


trump.data.sentiments <- trump.data %>%
  unnest_tokens("word", "text") %>%
  count(word) %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(something = n * value) %>%
  summarise(sum(something)) %>%
  .[[1]]

trump.text <- str_replace_all(trump.text,
                              "\n",
                              "")
  
trump.data <- data.frame(text = trump.text,
                         stringsAsFactors = FALSE) %>%
  filter(text != "") %>%
  mutate(paragraph = row_number())


#visualize by pgraph
trump.data %>%
  unnest_tokens("word", "text") %>%
  count(paragraph, word) %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(something = n * value) %>%
  group_by(paragraph) %>%
  summarise(sum = sum(something)) %>%
  mutate(is.positive = ifelse(sum > 0,
                              TRUE,
                              F)) %>%
  ggplot() +
  geom_bar(aes(fill = is.positive, x = paragraph, 
               y = sum),
           stat = "identity") 
  # geom_hline(yintercept = 0,
  #            color = "red",
  #            size = 2)

trump.data %>%
  unnest_tokens("word", "text") %>%
  count(paragraph, word) %>%
  inner_join(get_sentiments("afinn")) %>%
  mutate(something = n * value) %>%
  group_by(paragraph) %>%
  summarise(sum = sum(something)) %>%
  filter(sum < -19)

#repeat analysis for all presidents

presidents.vector <- c("Bill_Clinton",
                       "Barack_Obama",
                       "Donald_Trump")

president.data <- NULL

wiki.url <- "https://en.wikipedia.org/wiki/Marriage_age_in_the_United_States"

pres.url <- "https://www.plaintextlist.com/politics/list_of_us_presidents/"

presidents.vector <- pres.url %>%
  read_html() %>%
  html_nodes("p") %>%
  html_text() %>%
  unique() %>%
  str_replace_all(" ", "_") %>%
  str_replace("\\.", "")


for(i in 1:length(presidents.vector)){
  
  trump.url <- paste0("https://en.m.wikipedia.org/wiki/",
                      presidents.vector[i])
  
  trump.text <- trump.url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text()
  
  trump.data <- data.frame(text = trump.text,
                           stringsAsFactors = FALSE) %>%
    filter(text != "") %>%
    mutate(paragraph = row_number())
  
  president.data[[i]] <- trump.data %>%
    unnest_tokens("word", "text") %>%
    count(paragraph, word) %>%
    inner_join(get_sentiments("afinn")) %>%
    mutate(something = n * value) %>%
    group_by(paragraph) %>%
    summarise(sum = sum(something)) %>%
    mutate(is.positive = ifelse(sum > 0,
                                TRUE,
                                F)) %>%
    ggplot() +
    geom_bar(aes(fill = is.positive, x = paragraph, 
                 y = sum),
             stat = "identity") +
    ggtitle(presidents.vector[i]) +
    theme(legend.position="none")
  
  print(i)
  
}

presidents.vector

grid.arrange(president.data[[1]],
             president.data[[2]],
             president.data[[3]],
             president.data[[4]],
             president.data[[5]],
             president.data[[6]]
             )

install.packages("shiny")


occupation.vector <- c("Consultant", "Consultant", "Consultant", "Consultant", "Consultant",
                       "Instructor", "Instructor", "Instructor", "Instructor", "Instructor", 
                       "Research_Assistant", "Research_Assistant", "Research_Assistant", 
                       "Research_Assistant", "Research_Assistant")

years.vector <- c(2012, 2013, 2014, 2015, 2016,
                  2012, 2013, 2014, 2015, 2016,
                  2012, 2013, 2014, 2015, 2016)

salary.vector <- c(0, 4000, 4000, 10000, 30000, 
                   0, 4000, 5000, 6500, 6500, 
                   22000, 25000, 25000, 25000, 26000)


occ.data <- data.frame(years = years.vector,
                       salary = salary.vector,
                       occupation = occupation.vector)

occ.data %>%
  group_by(years) %>%
  ggplot(aes(x = years, 
             y = salary,
             fill = occupation)) +
  geom_area(position = 'stack') +
  theme_classic() +
  scale_y_continuous(breaks = seq(0,60000, by = 20000)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Alex's Income in Various Jobs",
       y = "Income($)", 
       x = "Year") +
  scale_fill_manual(values = c("#56b4e9", "#f0e442", "#cc3c1c"),
                    labels=c("Consultant", "Instructor", "Research Assistant"))


titlePanel(
  fluidRow(
    column(9, "Fenologische modellen"), 
    column(3, img(height = 105, width = 300, src = "logo_pcfruit.jpg"))
  )
)


life <-read_csv("life.csv")
income <- read_csv("income.csv")

life.data <- life
income.data <- income

#wide to long
life.gathered <- life.data %>%
  gather(key = "year",
         value = "life.expectancy",
         -country)

income.gathered <- income.data %>%
  gather(key = "year",
         value = "income",
         -country)

joined.data <- life.gathered %>%
  inner_join(income.gathered,
             by = c("country", "year"))

income.data %>%
  anti_join(life.data,
             by = c("country"))

#state of the world, 2000

joined.data %>%
  filter(year == 2000) %>%
  ggplot(aes(x = income,
             y = life.expectancy)) +
  geom_point()

region.url <- "https://meta.wikimedia.org/wiki/List_of_countries_by_regional_classification"

regions <- region.url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id= "mw-content-text"]/div/table') %>%
  html_table() %>%
  .[[1]]

joined.data <- joined.data %>%
  inner_join(regions %>% select(-`Global South`),
             by = c("country" = "Country"))

wiki.pop.url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

wiki.pop <- wiki.pop.url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table') %>%
  html_table() %>%
  .[[1]]

pop.data <- wiki.pop

colnames(pop.data)[c(2)] <- c("country")

pop.data <- pop.data %>%
  select(country, Population)

pop.data$Population <- str_remove_all(pop.data$Population,
                                      ",") %>%
  as.numeric()

#remove extra chars
pop.data$country <- str_replace_all(pop.data$country,
                                "\\[.+\\]",
                                "")
joined.data <- joined.data %>%
  left_join(pop.data)

daily.df %>% 
  group_by() %>% 
  add_count() %>%  View()

joined.data %>%
  filter(year == 2018) %>%
  ggplot(aes(x = income,
             y = life.expectancy)) +
  geom_point(aes(size = Population,
                 color = Region)) +
  scale_x_log10() +
  scale_size_continuous(breaks = c(0, 10000, 100000, 1000000, 10000000, 100000000, Inf))


#save joined_data to our computer
write_csv(joined.data, 
          "gapminder.csv")


#Making maps
install.packages("leaflet")


leaflet() %>%
  addTiles() %>%
  addMarkers(lat = 44.0153, 
             lng = -73.1673,
             label = "midd",
             popup = "here is a description")

leaf.data <- data.frame(latitude = c(30, 18, -20),
                        longitude = c(12, 100, -50),
                        names = c("foo", "bar", "cheeks"))

#map the leaf data

leaf.data %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat = ~latitude,
             lng = ~longitude)

noaa.url <- "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=
Like&query_0=&op_8=eq&v_8=&type_10=EXACT&query_10=None+Selected&le_
2=&ge_3=&le_3=&ge_2=&op_5=eq&v_5=&op_6=eq&v_6=&op_7=eq&v_7=&t=102557&s=5&d=5"


noaa.volc <- noaa.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table() 

noaa.volc %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat = ~Latitude,
             lng = ~Longitude,
             label = ~`Volcano Name`)

#clustering markers

noaa.volc %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat = ~Latitude,
             lng = ~Longitude,
             #label = ~`Volcano Name`,
             clusterOptions = markerClusterOptions())

#lets make a cloropleth
install.packages("geojsonio")

states.data <- geojson_read("states.json",
                            what = "sp")

View(states.data@data)

state <- states.data

states.data %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons() %>%
  setView(-96,37.8,3)

install.packages("readxl")


#grad graph

majors <- read_excel("grad_data.xls")

majors.data <- majors

#remove NAs
majors.data <- na.omit(majors.data)

majors.data$

#remove first 2 rows
majors.data <- majors.data[-c(1,2),]

#gather data
majors.gathered <- majors.data %>%
  gather(key = "year",
         value = "count",
         -`Field of Study`)


majors.gathered %>%
  ggplot(aes(x = Year,
             y = Count)) +
  geom_line(aes(color = `Field of Study`)) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_text(aes(label = `Field of Study`),
            data = majors.gathered %>%
              filter(Year == max(majors.gathered$Year)))


dib.url <- "https://www.comparitech.com/vpn-privacy/the-worlds-most-surveilled-cities/"

dib.df <- dib.url %>% 
  read_html() %>%
  html_nodes(xpath = '//*[@id="tablepress-489-no-2"]') %>% 
  html_table() %>% 
  .[[1]]

colnames(dib.df)[5] <- "num.?eras"


dib.df %>%
  ggplot(aes(x = reorder(City, num.cameras), y = num.cameras)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic() +
  labs(y = element_text("Number of cameras per 1000 people"),
       x = element_text("City"))
  
install.packages('rsconnect')

rsconnect::setAccountInfo(name='mikechirico',
                          token='8C5546A55E086D75460E59833ECE5DDA',
                          secret='5Ov4iT3aKTVpNfp8AO/iEZyFBbH+L218BRG03Aus')


boolean.players %>% 
  spread(key = is.full.moon,
         value = `mean(pts)`) %>%
  filter(`max(fm.count)` != 0) %>%
  mutate(diff = `TRUE` - `FALSE`) %>%
  head(100) %>%
  ggplot(aes(x = `max(fm.count)`,
             y = diff)) +
  geom_point()


install.packages("ggimage")

fake.data <- data.frame(platypi = c("babyP",
                                    "baby2",
                                    "ppusplush"),
                        cuteness = c(3,
                                     6,
                                     10),
                        picture = c("/Users/mikechirico2/Desktop/MATH216/babyp.jpeg",
                        "/Users/mikechirico2/Desktop/MATH216/baby2.jpeg",
                        "/Users/mikechirico2/Desktop/MATH216/ppuswhack.jpeg"))

fake.data %>%
  ggplot(aes(x = platypi,
             y = cuteness)) +
  geom_bar(stat ="identity") +
  geom_image(aes(image = picture,
                 y = cuteness - 1),
             size = .2)


states <- geojson_read("states_medium.json",
                       what = "sp")

states.data <- states


# states.data %>%
#   leaflet() %>% 
#   addTiles() %>% 
#   addPolygons() %>% 
#   setView(-96, 37.8, 3)

bins1 <- c(fivenum(states.data@data$CENSUSAREA))
colors1 <- colorBin(palette = "YlOrRd",
                    bins = bins1,
                    domain = states.data@data$CENSUSAREA)

states.data %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons() %>% 
  addPolygons(fillColor = ~colors1(CENSUSAREA),
              weight = 2,
              color = "white",
              dashArray = "3",
              opacity = 1,
              fillOpacity = .7) %>% 
  setView(-96, 37.8, 3)

pop.url <- "https://en.wikipedia.org/wiki/List_of_states_and_terri
tories_of_the_United_States_by_population"

pop.table <- pop.url %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table() %>%
  .[[1]]

pop.data.clean <- pop.table[,c(3,4)]

colnames(pop.data.clean)[2] <- "population"

pop.data.clean$population <- str_replace_all(pop.data.clean$population,
                                             "[:punct:]",
                                             "") %>%
  as.numeric()

#join data sets
states.data@data <- left_join(states.data@data,
                              pop.data.clean,
                              by = c("NAME" = "Name"))
View(states.data@data)  

bins2 <- c(fivenum(states.data@data$population))
colors2 <- colorBin(palette = "Blues",
                    bins = bins2,
                    domain = states.data@data$population)

states.data %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons() %>% 
  addPolygons(fillColor = ~colors2(population),
              weight = 2,
              color = "white",
              dashArray = "3",
              opacity = 1,
              fillOpacity = .7) %>% 
  setView(-96, 37.8, 3) %>% 
  addLegend(pal = colors2)


install.packages("rtweet")


solar.tweets <- search_tweets("#solarpanels",
                              n = 100,
                              include_rts = F)

get_timeline("realDonaldTrump")

badTweet <- get_timeline("fleisherjake")

multiple.tweets <- get_timelines(c("realDonaldTrump",
                                   "HillaryClinton",
                                   "Pontifex"),
                                 n = 100)

multiple.tweets %>% 
  group_by(screen_name) %>% 
  ts_plot("days")

#benfords law

#how to write a function in r
#dumb function

wrongmean <- function(numbers){
  x <- sum(numbers)/length(numbers) + 1
  return(x)
}

median()
  
newMedian <- function(numbers) {
  sort(numbers) %>% 
    ifelse(mod(length(numbers), 2) == 2,
           return(mean(numbers[length(numbers) /2:(length(numbers)/2 + 1)])),
           return(numbers[length(numbers/2)])
           )
}


cleanNumbers <- function(vector1) {
  str_replace_all(vector1,
                  ",",
                  "")
}

benford.data <- data.frame(digit = c(1:9),
                           prob = c(.301,
                                    .176,
                                    .125,
                                    .097,
                                    .079,
                                    .067, 
                                    .058,
                                    .051,
                                    .046))



first.digits <- str_sub(diamonds$price,
        start = 1,
        end = 1)

data.frame(digit = first.digits) %>% 
  count(digit) %>% 
  mutate(p = n/sum(n)) %>% 
  ggplot(aes(x = digit,
             y = p)) +
  geom_bar(stat = "identity",
           fill = "blue",
           alpha = .5) +
  geom_bar(data = benford.data,
           mapping = aes(y = prob),
           stat = "identity",
           fill = "red",
           alpha = .5)
  

benford.gg <-   geom_bar(data = benford.data,
                         mapping = aes(y = prob),
                         stat = "identity",
                         fill = "red",
                         alpha = .5)
  

benfordGraph <- function(vector1) {
  vector2 <- str_replace_all(vector1,
                  ",",
                  "") %>% 
    str_sub(start = 1,
            end = 1)
    data.frame(digit = vector2) %>% 
    count(digit) %>% 
    mutate(p = n/sum(n)) %>% 
    ggplot(aes(x = digit,
               y = p)) +
    geom_bar(stat = "identity",
             fill = "blue",
             alpha = .5) +
    benford.gg
}

  benfordGraph(pop.data$Population)
  
  
  scrape.tables <- function(url) {
    url %>% 
      read_html() %>% 
      html_nodes("table") %>%
      html_table() %>% 
      .[[1]]
    
  }
  
  View(scrape.tables("https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population"))
  
  
lyrics.df <- fread("lyrics.csv")

lyrics.df$artist %>% 
  unique() %>% 
  View()

##linear regression
#Predicting the value of a quantitative variable: outdated, machine learning is better
#Understand and quantify the relationship between two quantitive variables

install.packages("spotifyr")

?spotifyr




vt.schools <- read_xls("VT_School_Data.xls")

vt.schools2 <- vt.schools %>% 
  mutate(percentFRL = StudentsFreeReducedLunch/TotalEnrollment)

model1 <- lm(M8~percentFRL,
             data = vt.schools2)

summary(model1)

vt.schools2 %>% 
  ggplot(aes(x = percentFRL,
             y = M8)) +
  geom_point()

vt.schools %>% 
  filter(LEA == 28) %>% 
  View()
  

model1 <- lm(M8~percentFRL,
             data = vt.schools2)

vt.schools.no.outlier <- vt.schools2 %>% 
  filter(M8 != 0)

model2 <- lm(M8~percentFRL,
             data = vt.schools.no.outlier)

summary(model2)


vt.schools.no.outlier %>% 
  ggplot(aes(x = percentFRL,
             y = M8)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = F)


#wine tasting data

wine.data <- read_csv("wine150k.csv")

#lets randomly select 5 varieties of wine
#only varieties with 50 wines


wine.data.sort <- wine.data %>% 
  count(variety) %>% 
  filter(n > 50) %>% 
  sample_n(5) %>% 
  .$variety

wine.data %>% 
  filter(variety %in% wine.data.sort) %>% 
  ggplot(aes(x = price)) +
  geom_histogram() +
  facet_wrap(aes(variety))

model3 <- lm(price ~ variety,
             data = wine.data %>% filter(variety %in% wine.data.sort))


summary(model3)

train.prac.data <- read_csv("train.csv")

house.model <- lm(SalePrice ~ LotArea + YearBuilt,
                  data = train.prac.data) 

summary(house.model)

train.prac.data

#splitting data set into training and validation set

selectedRows <- sample(1:nrow(train.prac.data),
                       size = .5*nrow(train.prac.data))

training.set <- train.prac.data[selectedRows, ]
validation.set <- train.prac.data[-selectedRows, ]


#introduction to machine learning
#decision trees


library(nycflights13)
library(rpart)
library(rattle)

flights.model1 <- lm(dep_delay ~ carrier + dest + sched_dep_time,
                     data = flights)

summary(flights.model1)

#tree is better

flights2 <- flights %>% 
  mutate(is.delayed = dep_delay > 0)

tree1 <- rpart(is.delayed ~ . -dep_delay,
               data = flights2)

fancyRpartPlot(tree1)


tree2 <- rpart(is.delayed ~ carrier + dest + sched_dep_time,
               data = flights2)

fancyRpartPlot(tree2)

# capitalized <- read_csv("senior_work.csv",
#          col_names = F) %>%
#   .[,2]
# 
# capitalized$X2 <- capitalized$X2 %>% 
#   capitalize()

write_csv(capitalized,
          "capped.csv")

jobbR::


math_df <- read_csv("math_scores.csv")
math_df$Date <- dmy(math_df$Date)

math_df %>% group_by(Date) %>% 
  ggplot(aes(x= Date, y=Score)) +
  geom_boxplot() 
  #geom_point() +
  #coord_flip()
  #geom_quasirandom() + 
line_plot <- math_df %>% 
  group_by(Date) %>% 
  summarise(mean(Score)) 
  
line_plot %>%  
  ggplot(aes(y = `mean(Score)`, x = Date)) +
  geom_point() +
  xlim(min(line_plot$Date), max(line_plot$Date))


roi_df <- read_csv("ROI.csv") %>% 
  head(29)

roi_df %>% 
  ggplot(aes(x= `Are you currently unemployed or underemployed`)) + 
  theme_bw() +
  geom_bar()

roi_df %>% 
  ggplot(aes(x= `Will you be actively looking for a remote job within the next 12 months`)) + 
  theme_bw() +
  geom_bar() + 
  facet_wrap(~`Are you currently unemployed or underemployed`) +
  #theme(panel.spacing = unit(2, "lines")) +
  coord_flip()


roi_df %>% 
  group_by(`Please select the main reason you are applying for this course`) %>% 
  ggplot(aes(x= `Are you currently unemployed or underemployed`)) + 
  theme_bw() +
  geom_bar()


jobSearch(publisher = 5447663788014200, 
          query = "data", 
          all = T, 
          location = 05150,
          radius = 150) %>% View()


jobSearch(publisher = 5447663788014200, 
          query = "farm",
          location = "05150",
          all = T) %>% 
  
  View()

marketing.jobs.vector <- c("SEO+manager", 
                 "SEO+analyst", 
                 "SEO+partnership+specialist", 
                 "SEO+strategist",
                 "Social+media+analyst",
                 "Social+media+manager",
                 "Social+media+community+manager",
                 "Social+media+coordinator",
                 "Social+media+marketer",
                 "Social+media+strategist", 
                 "Google+adwords+and+analytics",
                 "SEM+analyst",
                 "SEM+manager",
                 "SEM marketing+coordinator")

jobs_tj <- c("data+analyst",
             "data+science",
             "data+scientist",
             "data+engineer",
             "business+analyst")


jobs.df <- NULL

for(i in 1:length(marketing.jobs.vector)){
  
  query.string <- marketing.jobs.vector[i]
  df <- NULL
  df <- jobSearch2(publisher2 = 5447663788014200, 
            query2 = query.string,
            location2 = "05156",
            radius2 = 60)
  
  jobs.df <- rbind(df, jobs.df)
  
}
write_csv(jobs.df,
          "SEO_results.csv")

foo <- jobSearch2(publisher2 = 5447663788014200, 
                  query2 = marketing.jobs.vector[1],
                  location2 = "05156", 
                  radius2 = 60)

jobSearch2 <- function(publisher2, query2, location2, radius2){
  temp <- NULL
  tryCatch(
    expr = {
      temp <- jobSearch(publisher = publisher2,
                        query = query2,
                        location = location2,
                        radius = radius2,
                        limit = 25) %>% 
        select(query,
              location,
              totalResults,
              results.jobtitle,
              results.city,
              results.state,
              results.formattedLocation,
              results.formattedLocationFull,
              results.source,
              results.date,
              results.snippet) %>% 
        ifelse(nrow(temp) == 25, temp <- rbind(temp, jobSearch(publisher = publisher2,
                                                                  query = query2,
                                                                  location = location2,
                                                                  radius = radius2,
                                                                  start = 25,
                                                                  limit = 25)))
      
      },
    error = function(cond){
      temp <- NA
    })

  return(temp)
}



jobSearch2(5447663788014200, "farm", "05150", 50)

temp <- jobSearch(publisher = 5447663788014200,
                  query = "from",
                  location = "05156",
                  radius = 60)
                  #limit = 25)

Cs <- team.data %>% 
  filter(nameTeam == "Boston Celtics",
         yearSeason > 2013 | slugSeason == "2013-14")


Cs %>% 
  group_by(Phase) %>% 
  arrange(-win) %>% 
  ggplot(aes(y = (mean(win)), x = Phase, fill=win)) +
  geom_bar(stat = "identity") + 
  theme_bw()

all_signups <- read_csv(file.choose())
springfield_zips <- read_csv(file.choose())
springfield_zips <- springfield_zips %>% 
  filter(`Service Area` == "Springfield")

springfield_zips$ZIP <- as.numeric(springfield_zips$ZIP)

springfield_signups <- all_signups %>% 
  inner_join(springfield_zips, by = c("Please enter your home Zip code" = "ZIP"))

write_sheet(springfield_signups %>% select(-enrolled_at),
            ss = "https://docs.google.com/spreadsheets
            /d/1Wuxi47ubEjTuZpV8yFhMoQn9l1M7dgrResZNnRBT5jw/edit#gid=0")
springfield_signups %>% 
  unique() %>% View()


kitty_function <- function(kitty) {
    babynames %>%
    filter(name == kitty) %>%
    group_by(name, year) %>%
    mutate(totalNameYear = sum(n)) %>%
    mutate(Femaleness = (n / totalNameYear)) %>%
    filter(sex == "F") %>%
    ggplot(aes(x = year, y = Femaleness)) +
    geom_line(size = 1) +
    aes(color = Femaleness) +
    scale_color_gradient(low = "purple", high = "green" )   #bc gender norms suck
    # geom_text(aes(label = "Ashley", x = 2000, y = .9, size = 8))
}

kitty_function("Joey")

prop.babynames<- babynames %>%
  #filter(name=="Sidney") %>% 
  #ample_n(100000) %>% 
  group_by(name, year) %>%
  mutate(totalNameYear = sum(n)) %>%
  mutate(Femaleness = (n / totalNameYear)) %>%
  filter(sex == 'F') %>% 
  select(year,name,totalNameYear, Femaleness)
  #summarise(max(Femaleness)-min(Femaleness)) #%>% 

  #group_by(name) %>% 
  #summarise(Femaleness) %>% 

  
prop.babynames %>% 
    group_by(name) %>% 
    summarise(max(Femaleness) -min(Femaleness), sum(totalNameYear)) %>% View()
  
  bbnames <- babynames %>% 
    group_by(name,year)%>%
    mutate(femaleness=n/sum(n))%>%
    filter(sex=="F")
  
  
  bbnames%>%
    filter(year %in% c("1880","2017"))%>%
    group_by(name)%>%
    summarize(difference = max(femaleness)-min(femaleness))%>%
    arrange(-difference)
  
  bbnames%>%
    filter(name %in% c("Donnie","Leslie","Robbie","Clair","Lynn"))%>%
    ggplot(aes(x= year,
               y= femaleness))+
    geom_line(aes(color=name))
  
  
  signups <- read_csv(file.choose())
  
  signups.graph <- signups %>% 
    distinct(email, .keep_all = T)
    count(`Please enter your home Zip code`) 
  
  signups.graph <- signups.graph[order(signups.graph$n, decreasing= TRUE),]
  
  signups.graph %>% 
    ggplot(aes(x = `Please enter your home Zip code`, y = n)) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw()
  
  my_list <- list()
  my_list[[1]] <- udacityDataTrack
  my_list[[2]] <- udacityFrontendTrack
  my_list[[3]] <- udacityMarketingTrack
    
  
 drac <- "http://www.horrorlair.com/scripts/draclugo.txt"
 
 
 udacity_sheet <- read_csv("Master3.csv") %>% 
   mutate(accepted = ifelse(`FA Status` == "Accepted", T, F))

 udacity_sheet %>% 
   #filter(`Community Name` == "Springfield") %>% 
   group_by(`What is your primary reason for enrolling in this program?`) %>% 
   summarise(mean(`# of Completed Requirements`), sum(`# of Completed Requirements`), n()) %>% View()
 
 udacity_sheet %>% 
   group_by(`What's your current employment status?`) %>% 
   summarise(mean(`# of Completed Requirements`), sum(`# of Completed Requirements`), n()) %>% View()
 
 
 slim_data <- udacity_sheet %>% 
   filter(`Community Name` == "Springfield") %>% 
   mutate(is_springfield = ifelse(`Please enter your home Zip code` == "05156", T,F)) %>% 
   group_by(is_springfield) %>% 
   summarise(total = n(),
             num_completed = sum(`# of Completed Requirements`), 
             percent_completed = mean(`# of Completed Requirements`))
 
 
   slim_data2 <- udacity_sheet %>% 
   filter(`Community Name` == "Springfield") %>% 
   group_by(`Please enter your home Zip code`) %>%
   summarise(total = n(), percent_completed = mean(`# of Completed Requirements`))
 
 
 slim_data <- slim_data[order(slim_data$total, decreasing= TRUE),] %>% 
   head(10)
 
   
 
 slim_data %>% 
   ggplot(aes(x = `Please enter your home Zip code`, y = total)) +
   geom_bar(stat = "identity") +
   coord_flip() +
   theme_bw()
   
 
udacity_sheet %>% 
   filter(`Please enter your home Zip code` == "05048") %>% 
   select(`I identify my gender as‚Ä¶`,
          `Which of the following best represents your racial or ethnic heritage? Choose all that apply`,
          `What is your primary reason for enrolling in this program?`, 
          `What's your current employment status?`,
          Course,
          `# of Completed Requirements`) %>% View()
 
 spring_short <- udacity_sheet %>% 
   filter(`Please enter your home Zip code` == "05301") %>% 
   select(`I identify my gender as‚Ä¶`,
          `Which of the following best represents your racial or ethnic heritage? Choose all that apply`,
          `What is your primary reason for enrolling in this program?`, 
          `What's your current employment status?`,
          Course,
          `# of Completed Requirements`)
 
 
 slim <- udacity_sheet %>% 
   group_by(`Community Name`) %>% 
   summarise(total = n(), completed = sum(`# of Completed Requirements`), phaseTwo = sum(accepted)) 
 
 
 slim2 <- slim %>% 
   mutate(completedPer = signif(completed / total, 3), phaseTwoPer = signif(phaseTwo / completed, 3))
 
 avgs <- summarise(slim2, completedAvg = mean(completedPer), avgPhaseTwo = mean(phaseTwoPer)) 


 
 slim %>% ggplot(aes(fill=condition, y=value, x=`Community Name`)) + 
   geom_bar(position="dodge", stat="identity")  
 
 
 specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
 condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
 value <- abs(rnorm(12 , 0 , 15))
 data <- data.frame(specie,condition,value)
 
 
 x.county.output %>% View()
 
 county.zip <- read.xlsx("COUNTY_ZIP.xlsx")
 
 czip <- county.zip %>% 
   group_by(COUNTY) %>% 
   top_n(1) %>% 
   select(COUNTY, ZIP)
 
 old.county.output <- read_rds("x.county.output.rds")
 
 new.county.output <- old.county.output %>% 
   left_join(czip, by = c("geoid" = "COUNTY"))

 
 write_rds(new.county.output, "x.county.output.rds")
 
 
 dater <- flights %>% 
   sample_n(3000)
 
test <- read_csv(file.choose())

test.list <- as.vector(test$`Array String`)

test.list <- test.list[!is.na(test.list)]


 
start.time <- Sys.time()
jobs <- jobSearchVectorShiny(test.list, "02030", 50)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



skoop <- seasons_schedule(seasons = 2019, 
                          season_types = "Regular Season", 
                          parse_boxscores = F, 
                          box_score_tables = c("Traditional"), 
                          nest_data = F, 
                          return_message = T)


NumbersCouldKnow <- c(4, 5, 7, 9, 13, 15, 19, 21, 25, 31, 33, 39, 43, 45, 
                      49, 55, 61, 63, 69, 73, 75, 81, 85, 91, 99, 6, 8, 10, 14, 16, 
                      20, 22, 26, 32, 34, 40, 44, 46, 50, 56, 62, 64, 70, 74, 76, 82, 86, 
                      92, 12, 18, 24, 28, 36, 42, 48, 52, 58, 66, 72, 78, 84, 88, 94, 30, 38, 
                      54, 60, 68, 80, 90, 96, 98)

allSums <- c(5:100)

yerr <- allSums - NumbersCouldKnow


notKnownNumbers <- c(11,17,23,27,29,35,37,41,47,51,53,57,59,65,67,71,77,79,83,87,89,93,95,97)

  
possibleProducts <- c()


#special cases
for(i in 1:length(notKnownNumbers)){
  
  curNum <- notKnownNumbers[i]
  print(curNum)
  for(j in 2:(curNum/2)){
    product <- (curNum - j) * (j)
    print(product)
    possibleProducts <- append(possibleProducts, product)
  }
  
}

for(i in 1:length(NumbersCouldKnow)){
  
  curNum <- NumbersCouldKnow[i]
  print(curNum)
  for(j in 2:(curNum/2)){
    product <- (curNum - j) * (j)
    print(product)
    possibleProducts2 <- append(possibleProducts2, product)
  }
  
}


x <- 2
y <- 3

full_df <- tibble(x,y)


while(x < 50){
  y = x + 1
  while(x + y < 100){
    full_df <- add_row(full_df, x, y)
    y = y + 1
  }
  x = x + 1
}

math_df <- full_df[-c(1),] %>% 
  mutate(sum = x + y,
         product = x * y,
         p_cant_know = sum %in% notKnownNumbers) %>%
  filter(product %in% list_of_possible_products)
  

list_of_possible_products <- filter(math_df, p_cant_know) %>% .$product


math_df %>% 
  group_by(product) %>% 
  mutate(count_unknown_sums = sum(is_unknown)) %>% 
  filter(count_unknown_sums == 1 & is_unknown) %>% 
  View()

