#https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,gganimate, data.table, knitr, gridExtra, plotly)

setwd("C:/Users/zman7/OneDrive/Olympics_data_analysis")


#Loading events table data
eventsdata <- read_csv("athlete_events.csv", 
                       col_types = cols(
                         ID = col_character(),
                         Name = col_character(),
                         Sex = col_factor(levels = c("M","F")),
                         Age =  col_integer(),
                         Height = col_double(),
                         Weight = col_double(),
                         Team = col_character(),
                         NOC = col_character(),
                         Games = col_character(),
                         Year = col_integer(),
                         Season = col_factor(levels = c("Summer","Winter")),
                         City = col_character(),
                         Sport = col_character(),
                         Event = col_character(),
                         Medal = col_factor(levels = c("Gold","Silver","Bronze"))
                       )
)

#loading in region data NOC = National Olympic Country
regionsdata <- read_csv("noc_regions.csv", 
                        col_types = cols( NOC = col_character(),
                                          region = col_character()
                        ))


#Getting a glimpse (get it?) of the datasets
glimpse(eventsdata)
glimpse(regionsdata)


#lets see how the number of events has changed over the years
eventsdata %>%
  select(Year, Event) %>%
  group_by(Year) %>%
  summarize(count_events = length(unique(Event)))


#highest medal winner per country(NOC)
#join the other NOC table to this data pull
eventsdata %>%
  filter(!is.na(Medal))%>%
  group_by(NOC, Name) %>%
  summarize(medalcount = length(Medal)) %>%    #count and sum do not work on character variables
  arrange(desc(medalcount)) %>%                #This sets the top entry for each NOC to be the person with the most medals
  slice(1:1)%>%                                #This gets the top 1 persons from each group (NOC and Name)
  print(n = Inf)                               #This prints all records n = infinity


#males vs females in events
eventsexcount <- eventsdata %>%
  filter(Sport != "Art Competitions")


# AFTER 1992, CHANGE THE YEAR OF THE WINTER GAMES TO COINCIDE WITH THE NEXT SUMMER GAMES. THE TERM "YEAR" CURRENTLY REFERS TO THE OLYMPICS TOOK PLACE
wintergamesyears <- c(1994,1998,2002,2006,2010,2014)
wintergameschangedyears <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(wintergamesyears)) {
  eventsexcount$Year <- gsub(wintergamesyears[i], wintergameschangedyears[i], eventsexcount$Year)
}
eventsexcount$Year <- as.integer(eventsexcount$Year)

countsex <- eventsexcount %>%
  group_by(Year,Sex) %>%
  summarise(count = length(unique(ID)))

countsex$Year <- as.integer(countsex$Year)

ggplot(countsex, aes(x=Year, y=count, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  transition_reveal(Year)+
  scale_color_manual(values=c("deepskyblue4","red4")) +
  labs(x = "Year", y = "Athletes Count", 
       title="Male and Female athletes over time", 
       subtitle = "Olympic Games from 1896 to 2016")


#which NOC had the highest number of participants
eventsdata_2 <- eventsdata
events_region_joined <- dplyr::left_join(eventsdata_2, regionsdata, by = "NOC")
events_region_joined
participantscount <- events_region_joined %>% select (ID, region, Year)
participantscount

participantcounts <- participantscount %>%
  group_by(region) %>%
  summarize(idcount = length(unique(ID))) %>%
  arrange(desc(idcount))


participantcounts


#scatter plot with number of male and female participants per each Region


sexcount.perregion <- events_region_joined %>%
  select(region , Sex) %>%
  group_by(region) %>%
  count(Sex)

sexcount.perregion

#alternative way
events_region_joined %>%
  group_by(region , Sex) %>%
  summarize(count = n())

#using spread function from tidyr to make the dataset more readable
data.plot <- sexcount.perregion %>% spread(Sex, n)
data.plot

#ploting this graph
ggplot(data.plot) +
  geom_point(aes(x = F, y = M, color = region)) +
  theme(legend.position = "none") +
  ggtitle("Plot of Regions and the number of Male and Female participants (Totals)") +
  xlab("Female Count Total") + ylab("Male Count Total")

#Avg age, weight and height of basketball players (pre 2000 and post 2000)
events_region_joined %>%
  filter(Event == "Basketball Men's Basketball" & region == "USA") %>%
  select(Height, Weight, Age, Event, Year, Name, region) %>%
  group_by(Year, Event, Name) %>%
  summarise(avgheight = mean(Height),
            avgweight = mean(Weight),
            avgage = mean(Age))


#which country is the best at certain sports
#look at sport and get a count of number of medals and number of gold medals per country/location

events_region_joined %>%
  select(region, Sport, Medal) %>%
  group_by(Sport, region) %>%
  summarise(total_medals = length(Medal)) %>%
  arrange(desc(total_medals)) %>%
  slice(1:1)


events_region_joined %>%
  filter(Medal == "Gold") %>%
  select(region, Sport, Medal) %>%
  group_by(Sport, region) %>%
  summarise(total_medals = length(Medal)) %>%
  arrange(desc(total_medals)) %>%
  slice(1:1)


#looking at average height per sport

#10 tallest sports
eventsdata %>%
  select(Sport, Height) %>%
  na.omit(Height) %>%
  group_by(Sport) %>%
  summarize(avgheight = mean(Height)) %>%
  arrange(desc(avgheight)) %>%
  slice(1:10)

#10 shortest sports
eventsdata %>%
  select(Sport, Height) %>%
  na.omit(Height) %>%
  group_by(Sport) %>%
  summarize(avgheight = mean(Height)) %>%
  arrange(avgheight) %>%
  slice(1:10)


