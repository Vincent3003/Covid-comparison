# data source: 
# https://ourworldindata.org/explorers/coronavirus-data-explorer?tab=map&zoomToSelection=true&facet=none&pickerSort=asc&pickerMetric=location&Interval=7-day+rolling+average&Relative+to+Population=true&Color+by+test+positivity=false&country=USA~CHN~VNM&Metric=Confirmed+cases
# Use 

# install package
install.packages(tidyverse) # for data manipulation
install.packages(lubridate) # for dates
install.packages("ggplot2")

# activate packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library("ggplot2")

# load the dataset & choose columns to analyze
covid = read.csv(file.choose(), header = TRUE, sep = ",", na.strings = c(NA, ""," ")) %>% 
  select(location, date, total_cases, total_deaths, population)
show(covid)

# get china COVID data
data_china = covid %>% 
  filter(location == "China")
show(data_china)
tail(data_china)

# get USA COVID data
data_usa = covid %>% 
  filter(location == "United States")
show(data_usa)
tail(data_usa)

# get Vietnam COVID data
data_vietnam = covid %>% 
  filter(location == "Vietnam")
show(data_vietnam)
tail(data_vietnam)

#-------------------------------------------------------------------------------#
# quarter time range
start_Q1_2020 = as.Date('2020-01-22')
end_Q1_2020 = as.Date('2020-03-31')

start_Q2_2020 = as.Date('2020-04-01')
end_Q2_2020 = as.Date('2020-06-30')

start_Q3_2020 = as.Date('2020-07-01')
end_Q3_2020 = as.Date('2020-09-30')

start_Q4_2020 = as.Date('2020-10-01')
end_Q4_2020 = as.Date('2020-12-31')

start_Q4_2020 = as.Date('2020-10-01')
end_Q4_2020 = as.Date('2020-12-31')

start_Q1_2021 = as.Date('2021-01-01')
end_Q1_2021 = as.Date('2021-03-31')

start_Q2_2021 = as.Date('2021-04-01')
end_Q2_2021 = as.Date('2021-06-30')

start_Q3_2021 = as.Date('2021-07-01')
end_Q3_2021 = as.Date('2021-09-30')

start_Q4_2021 = as.Date('2021-10-01')
end_Q4_2021 = as.Date('2021-12-31')

start_Q1_2022 = as.Date('2022-01-01')
end_Q1_2022 = as.Date('2022-03-31')

start_Q2_2022 = as.Date('2022-04-01')
end_Q2_2022 = as.Date('2022-06-30')
#-------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------#
# Calculate the avg of total cases of three different countries
# calculate the avg of total cases of Quarter 1 2020 in China, USA, & Vietnam
Q1_2020_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2020_covid_china

Q1_2020_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2020_covid_usa

Q1_2020_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2020_covid_vietnam

# calculate the avg of total cases of Quarter 2 2020 in China, USA, & Vietnam
Q2_2020_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q2_2020_covid_china

Q2_2020_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_cases), digits = 2))  # calculate the avg of that quarter
Q2_2020_covid_usa

Q2_2020_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q2_2020_covid_vietnam

# calculate the avg of total cases of Q3 2020 in China, USA, & Vietnam
Q3_2020_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q3_2020_covid_china

Q3_2020_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q3_2020_covid_usa

Q3_2020_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q3_2020_covid_vietnam

# calculate the avg of total cases of Q4 2020 in China, USA, & Vietnam
Q4_2020_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q4_2020_covid_china

Q4_2020_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q4_2020_covid_usa

Q4_2020_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q4_2020_covid_vietnam

# calculate the avg of total cases of Q1 2021 in China, USA, & Vietnam
Q1_2021_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2021_covid_china

Q1_2021_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2021_covid_usa

Q1_2021_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2021_covid_vietnam

# calculate the avg of total cases of Q2 2021 in China, USA, & Vietnam
Q2_2021_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q2_2021_covid_china

Q2_2021_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q2_2021_covid_usa

Q2_2021_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q2_2021_covid_vietnam

# calculate the avg of total cases of Q3 2021 in China, USA, & Vietnam
Q3_2021_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q3_2021_covid_china

Q3_2021_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q3_2021_covid_usa

Q3_2021_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q3_2021_covid_vietnam

# calculate the avg of total cases of Q4 2021 in China, USA, & Vietnam
Q4_2021_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q4_2021_covid_china

Q4_2021_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q4_2021_covid_usa

Q4_2021_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q4_2021_covid_vietnam

# calculate the avg of total cases of Q1 2022 in China, USA, & Vietnam
Q1_2022_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2022_covid_china

Q1_2022_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2022_covid_usa

Q1_2022_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q1_2022_covid_vietnam

# calculate the avg of total cases of Q2 2022 in China, USA, & Vietnam
Q2_2022_covid_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q2_2022_covid_china

Q2_2022_covid_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q2_2022_covid_usa

Q2_2022_covid_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that quarter
Q2_2022_covid_vietnam
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
# create a new table for China
china_covid <- c(Q1_2020_covid_china, Q2_2020_covid_china, Q3_2020_covid_china, 
           Q4_2020_covid_china, Q1_2021_covid_china, Q2_2021_covid_china, 
           Q3_2021_covid_china, Q4_2021_covid_china, Q1_2022_covid_china, Q2_2022_covid_china)
china_covid

# create a new table for USA
usa_covid <- c(Q1_2020_covid_usa, Q2_2020_covid_usa, Q3_2020_covid_usa, Q4_2020_covid_usa, 
         Q1_2021_covid_usa, Q2_2021_covid_usa, Q3_2021_covid_usa, Q4_2021_covid_usa, 
         Q1_2022_covid_usa, Q2_2022_covid_usa)
usa_covid

# create a new table for china
vietnam_covid <- c(Q1_2020_covid_vietnam, Q2_2020_covid_vietnam, Q3_2020_covid_vietnam, 
             Q4_2020_covid_vietnam, Q1_2021_covid_vietnam, Q2_2021_covid_vietnam, 
             Q3_2021_covid_vietnam, Q4_2021_covid_vietnam, Q1_2022_covid_vietnam, Q2_2022_covid_vietnam)
vietnam_covid
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
# merge the mean data of China, USA, Vietnam into one table
table1 = rbind(china_covid, usa_covid)
table1
table2 = rbind(table1, vietnam_covid)
table2

# convert columns of an table2 data frame into rows
table2 <- as.data.frame(t(table2))
table2

# assigning the unnamed first column to a new name of a data frame
table3 = setNames(cbind(rownames(table2), table2, row.names = NULL), 
                  c("timeperiod", "China", "USA", "Vietnam"))
table3
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
line_graph <- tidyr::pivot_longer(table3, c(row, timeperiod))
line_graph

line_graph_new <- ggplot(data = table3, aes(x = timeperiod),
       main = "The average of total case in three different countries")
line_graph <- line_graph + geom_line(aes(y = China), color = "red")
line_graph <- line_graph + geom_line(aes(y = USA), color = "blue")
line_graph <- line_graph + geom_line(aes(y = Vietnam), color = "yellow")
line_graph <- line_graph + theme_classic()
line_graph <- line_graph + scale_color_manual(name = "Country", 
                                              labels = c("China", "USA", "Vietnam"),
                                              values = c("red", "blue", "yellow"))
line_graph

