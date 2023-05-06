# Notes: Setting up my environment by installing and loading the "here", "tidyverse", "lubridate", "skimr", and "janitor"  packages:

install.packages("here")
library(here)
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
install.packages("skimr")
library(skimr)
install.packages("janitor")
library(janitor)
install.packages("knitr")
library(knitr)


# Notes: Setting working directory to "/cloud/project/case-study-bellabeat"

getwd()
setwd("/cloud/project/case-study-bellabeat")


# Notes: Importing [...] .csv files and re-labeling them for simplicity

activity_daily <- read_csv("dailyActivity_merged1.csv")
sleep_daily <- read_csv("sleepDay_merged1.csv")
weight_daily <- read_csv("weightLogInfo_merged1.csv")
steps_intensities_hourly <- read_csv("hourlyStepsAndIntensities_merged1.csv")
intensities_daily <- read_csv("dailyIntensities_merged1.csv")


# Notes: Allows us to see the column names we may need to rename

colnames(activity_daily)
head(weight_daily)
head(steps_intensities_hourly)
str(sleep_daily)
skim_without_charts(activity_daily)
skim_without_charts(weight_daily)
skim_without_charts(steps_intensities_hourly)
skim_without_charts(sleep_daily)

# Notes: renaming columns for simplicity

activity_daily <- activity_daily %>% 
  rename(SedentaryDistance = SedentaryActiveDistance)


# Notes: Understanding some summary statistics


activity_participants <- activity_daily %>% 
  summarize(n_distinct(activity_daily$Id), 
            n_distinct(sleep_daily$Id), 
            n_distinct(weight_daily$Id), 
            n_distinct(steps_intensities_hourly$Id))

as_tibble(activity_participants)


# Notes: using summary statistics to get select data frames

activity_daily %>%  
  select(TotalSteps,
         TotalTrackerDistance,
         SedentaryMinutes) %>%
  summary()
## The CDC recommends 10,000 steps a day. The data shows that most people take around 7,500 steps a day, roughly 75% of the recommended amount.


sleep_daily %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()
## Most people record daily sleep cycle and sleep around 7 hours.

# Notes: Visualizations can help paint a clearer picture of the data.

ggplot(data = activity_daily) + geom_point(mapping=aes(x=TotalSteps, y=SedentaryMinutes))


ggplot(data = activity_daily) + geom_point(mapping=aes(x=TotalSteps, y=SedentaryMinutes)) + geom_smooth(mapping=aes(x=TotalSteps, y=SedentaryMinutes))


ggplot(data = activity_daily) + geom_point(mapping=aes(x=TotalSteps, y=Calories))

ggplot(data = activity_daily) + geom_point(mapping=aes(x=TotalSteps, y=SedentaryMinutes, alpha=TotalSteps, color=TotalSteps)) + labs(title="Total Steps vs Sedentary Minutes")
## There are two main groups of people who roughly take the same amount of steps but have largely different amounts of sedentary minutes in a day.
## This may indicate that one group sleeps less or that other group may have obligations (work, school, commuting, etc.) that may keep them sedentary for some part of the day and they make up their steps through exercise.
## Another explanation for sedentary behavior could be related to the increase of time spent sitting around watching television or consuming other forms of media.

ggplot(data = sleep_daily) + geom_point(mapping=aes(x=TotalMinutesAsleep, y=TotalTimeInBed), color="skyblue4") + geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + labs(title="Total Time Asleep vs. Total Time in Bed")
## While the general expectation of there being a completely linear relationship with total time asleep and total time in bed, there are some outliers.
## Most interestingly, there is a cluster of people who spend more time in bed relative to how long everyone else is asleep.
## This segment of customer could potentially be targeted with in-app notifications during the times they are in bed but not asleep, encouraging them to use this free time exercise.


distance_comparison <- activity_daily %>% 
  select(VeryActiveDistance,
         ModeratelyActiveDistance,
         LightActiveDistance,
         SedentaryDistance)

# Notes: combining data to get a clearer picture of how different variables interact with each other.

activity_daily_with_sleep <- merge(sleep_daily, activity_daily, by=c("Id","Date"), all = TRUE) %>% 
  drop_na()
View(activity_daily_with_sleep)

activity_daily_with_sleep %>% 
  summarize(n_distinct(activity_daily_with_sleep$Id))

ggplot(data = activity_daily_with_sleep) + geom_point(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes, color=TotalTimeInBed)) + labs(title="Total Minutes Asleep vs. Sedentary Minutes")
## Most sedentary time is not spent while in bed, indicating that most inactive time occurs during the day.
## This can be explained by a higher propensity to sit down and consume media as well as time spent commuting (in trains and in cars) and time seated at work or in school. 

## Notes: creating a merged dataset that reflects the relationship between weight and intensity

intensities_daily$ActiveIntensity <- (intensities_daily$VeryActiveMinutes)/60

weight_and_intensities <- merge(weight_daily, intensities_daily, by="Id", all=TRUE) %>% 
  drop_na()
View(weight_and_intensities)

ggplot(data=weight_and_intensities, aes(x=Date.x, y=ActiveIntensity)) + geom_histogram(stat = "identity", fill='steelblue4') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Active Intensity vs. Time ") + 
  scale_x_discrete(name="Date and Time") +
  scale_y_discrete(name="Active Intensity")
## This histogram shows the relationship between active intensity and time.
## You can see that Active Intensity is generally highest in the mornings and lowest in the evenings.

