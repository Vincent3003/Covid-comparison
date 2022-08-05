# data source: 
# https://ourworldindata.org/explorers/coronavirus-data-explorer?tab=map&zoomToSelection=true&facet=none&pickerSort=asc&pickerMetric=location&Interval=7-day+rolling+average&Relative+to+Population=true&Color+by+test+positivity=false&country=USA~CHN~VNM&Metric=Confirmed+cases
# use "owid-covid-data" file

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
# calculate the avg of total cases of Q1 2020 in China, USA, & Vietnam
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

# calculate the avg of total cases of Q2 2020 in China, USA, & Vietnam
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
# Calculate the total deaths of three different countries
# calculate the total deaths of Q1 2020 in China, USA, & Vietnam
Q1_2020_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2020_deaths_china

Q1_2020_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2020_deaths_usa

Q1_2020_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2020_deaths_vietnam

# calculate the total deaths of Q2 2020 in China, USA, & Vietnam
Q2_2020_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q2_2020_deaths_china

Q2_2020_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_deaths), digits = 2))  # calculate the avg of that quarter
Q2_2020_deaths_usa

Q2_2020_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q2_2020_deaths_vietnam

# calculate the total deaths of Q3 2020 in China, USA, & Vietnam
Q3_2020_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q3_2020_deaths_china

Q3_2020_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q3_2020_deaths_usa

Q3_2020_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q3_2020_deaths_vietnam

# calculate the total deaths of Q4 2020 in China, USA, & Vietnam
Q4_2020_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q4_2020_deaths_china

Q4_2020_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q4_2020_deaths_usa

Q4_2020_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q4_2020_deaths_vietnam

# calculate the total deaths of Q1 2021 in China, USA, & Vietnam
Q1_2021_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2021_deaths_china

Q1_2021_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2021_deaths_usa

Q1_2021_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2021_deaths_vietnam

# calculate the total deaths of Q2 2021 in China, USA, & Vietnam
Q2_2021_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q2_2021_deaths_china

Q2_2021_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q2_2021_deaths_usa

Q2_2021_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q2_2021_deaths_vietnam

# calculate the total deaths of Q3 2021 in China, USA, & Vietnam
Q3_2021_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q3_2021_deaths_china

Q3_2021_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q3_2021_deaths_usa

Q3_2021_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q3_2021_deaths_vietnam

# calculate the total deaths of Q4 2021 in China, USA, & Vietnam
Q4_2021_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q4_2021_deaths_china

Q4_2021_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q4_2021_deaths_usa

Q4_2021_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q4_2021_deaths_vietnam

# calculate the total deaths of Q1 2022 in China, USA, & Vietnam
Q1_2022_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2022_deaths_china

Q1_2022_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2022_deaths_usa

Q1_2022_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q1_2022_deaths_vietnam

# calculate the total deaths of Q2 2022 in China, USA, & Vietnam
Q2_2022_deaths_china = data_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q2_2022_deaths_china

Q2_2022_deaths_usa = data_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q2_2022_deaths_usa

Q2_2022_deaths_vietnam = data_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_deaths), digits = 2)) # calculate the avg of that quarter
Q2_2022_deaths_vietnam
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
# calculate the percentage of total case to see how many percentage of case do we have in a country
# Quarter 1, 2020
Q1_2020_china_percent = round((Q1_2020_deaths_china/Q1_2020_covid_china)*100, digits = 6)
Q1_2020_china_percent
Q1_2020_usa_percent = round((Q1_2020_deaths_usa/Q1_2020_covid_usa)*100, digits = 6)
Q1_2020_usa_percent
Q1_2020_vietnam_percent = round((Q1_2020_deaths_vietnam/Q1_2020_covid_vietnam)*100, digits = 6)
Q1_2020_vietnam_percent

# Quarter 2, 2020
Q2_2020_china_percent = round((Q2_2020_deaths_china/Q2_2020_covid_china)*100, digits = 6)
Q2_2020_china_percent
Q2_2020_usa_percent = round((Q2_2020_deaths_usa/Q2_2020_covid_usa)*100, digits = 6)
Q2_2020_usa_percent
Q2_2020_vietnam_percent = round((Q2_2020_deaths_vietnam/Q2_2020_covid_vietnam)*100, digits = 6)
Q2_2020_vietnam_percent

# Quarter 3, 2020
Q3_2020_china_percent = round((Q3_2020_deaths_china/Q3_2020_covid_china)*100, digits = 6)
Q3_2020_china_percent
Q3_2020_usa_percent = round((Q3_2020_deaths_usa/Q3_2020_covid_usa)*100, digits = 6)
Q3_2020_usa_percent
Q3_2020_vietnam_percent = round((Q3_2020_deaths_vietnam/Q3_2020_covid_vietnam)*100, digits = 6)
Q3_2020_vietnam_percent

# Quarter 4, 2020
Q4_2020_china_percent = round((Q4_2020_deaths_china/Q4_2020_covid_china)*100, digits = 6)
Q4_2020_china_percent
Q4_2020_usa_percent = round((Q4_2020_deaths_usa/Q4_2020_covid_usa)*100, digits = 6)
Q4_2020_usa_percent
Q4_2020_vietnam_percent = round((Q4_2020_deaths_vietnam/Q4_2020_covid_vietnam)*100, digits = 6)
Q4_2020_vietnam_percent

# Quarter 1, 2021
Q1_2021_china_percent = round((Q1_2021_deaths_china/Q1_2021_covid_china)*100, digits = 6)
Q1_2021_china_percent
Q1_2021_usa_percent = round((Q1_2021_deaths_usa/Q1_2021_covid_usa)*100, digits = 6)
Q1_2021_usa_percent
Q1_2021_vietnam_percent = round((Q1_2021_deaths_vietnam/Q1_2021_covid_vietnam)*100, digits = 6)
Q1_2021_vietnam_percent

# Quarter 2, 2021
Q2_2021_china_percent = round((Q2_2021_deaths_china/Q2_2021_covid_china)*100, digits = 6)
Q2_2021_china_percent
Q2_2021_usa_percent = round((Q2_2021_deaths_usa/Q2_2021_covid_usa)*100, digits = 6)
Q2_2021_usa_percent
Q2_2021_vietnam_percent = round((Q2_2021_deaths_vietnam/Q2_2021_covid_vietnam)*100, digits = 6)
Q2_2021_vietnam_percent

# Quarter 3, 2021
Q3_2021_china_percent = round((Q3_2021_deaths_china/Q3_2021_covid_china)*100, digits = 6)
Q3_2021_china_percent
Q3_2021_usa_percent = round((Q3_2021_deaths_usa/Q3_2021_covid_usa)*100, digits = 6)
Q3_2021_usa_percent
Q3_2021_vietnam_percent = round((Q3_2021_deaths_vietnam/Q3_2021_covid_vietnam)*100, digits = 6)
Q3_2021_vietnam_percent

# Quarter 4, 2021
Q4_2021_china_percent = round((Q4_2021_deaths_china/Q4_2021_covid_china)*100, digits = 6)
Q4_2021_china_percent
Q4_2021_usa_percent = round((Q4_2021_deaths_usa/Q4_2021_covid_usa)*100, digits = 6)
Q4_2021_usa_percent
Q4_2021_vietnam_percent = round((Q4_2021_deaths_vietnam/Q4_2021_covid_vietnam)*100, digits = 6)
Q4_2021_vietnam_percent

# Quarter 1, 2022
Q1_2022_china_percent = round((Q1_2022_deaths_china/Q1_2022_covid_china)*100, digits = 6)
Q1_2022_china_percent
Q1_2022_usa_percent = round((Q1_2022_deaths_usa/Q1_2022_covid_usa)*100, digits = 6)
Q1_2022_usa_percent
Q1_2022_vietnam_percent = round((Q1_2022_deaths_vietnam/Q1_2022_covid_vietnam)*100, digits = 6)
Q1_2022_vietnam_percent

# Quarter 2, 2022
Q2_2022_china_percent = round((Q2_2022_deaths_china/Q2_2022_covid_china)*100, digits = 6)
Q2_2022_china_percent
Q2_2022_usa_percent = round((Q2_2022_deaths_usa/Q2_2022_covid_usa)*100, digits = 6)
Q2_2022_usa_percent
Q2_2022_vietnam_percent = round((Q2_2022_deaths_vietnam/Q2_2022_covid_vietnam)*100, digits = 6)
Q2_2022_vietnam_percent
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
# create a new table for China
china_percentage <- c(Q1_2020_china_percent, Q2_2020_china_percent, Q3_2020_china_percent, 
                      Q4_2020_china_percent, Q1_2021_china_percent, Q2_2021_china_percent, 
                      Q3_2021_china_percent, Q4_2021_china_percent, Q1_2022_china_percent, 
                      Q2_2022_china_percent)
china_percentage

# create a new table for USA
usa_percentage <- c(Q1_2020_usa_percent, Q2_2020_usa_percent, Q3_2020_usa_percent, 
                    Q4_2020_usa_percent, Q1_2021_usa_percent, Q2_2021_usa_percent, 
                    Q3_2021_usa_percent, Q4_2021_usa_percent, Q1_2022_usa_percent, 
                    Q2_2022_usa_percent)
usa_percentage

# create a new table for china
vietnam_percentage <- c(Q1_2020_vietnam_percent, Q2_2020_vietnam_percent, Q3_2020_vietnam_percent, 
                        Q4_2020_vietnam_percent, Q1_2021_vietnam_percent, Q2_2021_vietnam_percent, 
                        Q3_2021_vietnam_percent, Q4_2021_vietnam_precent, Q1_2022_vietnam_precent, 
                        Q2_2022_vietnam_percent)
vietnam_percentage

# merge the mean data of China, USA, Vietnam into one table
table1.2 = rbind(china_percentage, usa_percentage)
table1.2
table2.2 = rbind(table1.1, vietnam_percentage)
table2.2

# convert columns of an table2 data frame into rows
table2.2 <- as.data.frame(t(table2.2))
table2.2

# assigning the unnamed first column to a new name of a data frame
table3.2 = setNames(cbind(rownames(table2.2), table2.2, row.names = NULL), 
                    c("quarteryear", "China", "USA", "Vietnam"))
table3.2
#-------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------#
line_graph_3 <- tidyr::pivot_longer(table3.1, -c(quarteryear))
line_graph_3

ggplot(data = line_graph_3, aes(x = quarteryear, y = value, 
           group = name, color = name)) +
  geom_line() + theme_classic() +
  scale_color_manual(name = "Variable",
                     labels = c("China", "USA", "Vietnam"),
                     values = c("red", "blue", "yellow"))

