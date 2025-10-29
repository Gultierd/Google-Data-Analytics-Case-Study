library(tidyverse)

# Reading all csv files into data frames and into correct columns
Cyclistic2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
Cyclistic2020 <- read.csv("Divvy_Trips_2020_Q1.csv")

# First we need to account just the columns that we need - start and end times, user types and duration of each ride.
Cyclistic2019 <- Cyclistic2019[c("start_time","end_time","usertype")]
names(Cyclistic2019) <- c("start", "end", "usertype")

# We have to reformat datatypes of dates in this dataset, as they are taken as chars instead of date and times.
Cyclistic2019$start <- as.POSIXct(Cyclistic2019$start, format = "%Y-%m-%d %H:%M:%S")
Cyclistic2019$end <- as.POSIXct(Cyclistic2019$end, format = "%Y-%m-%d %H:%M:%S")


# Same goes with 2020 dataset, we will rename it accordingly so it will fit with the first one
Cyclistic2020 <- Cyclistic2020[c("started_at","ended_at","member_casual")]
names(Cyclistic2020) <- c("start", "end", "usertype")

Cyclistic2020$start <- as.POSIXct(Cyclistic2020$start, format = "%Y-%m-%d %H:%M:%S")
Cyclistic2020$end <- as.POSIXct(Cyclistic2020$end, format = "%Y-%m-%d %H:%M:%S")

# Cleaning data - removing rows with missing values and duplicate rows
Cyclistic2019 <- Cyclistic2019 %>%
  distinct(.keep_all = TRUE) %>% 
  na.omit()

# Creating new columns with trip durations and weekdays
Cyclistic2019$duration <- difftime(Cyclistic2019$end, Cyclistic2019$start, units = "mins")
Cyclistic2019$weekday <- wday(Cyclistic2019$start, week_start = 1) 

# Removing outliers - trips shorter than 1 minute and longer than 3 weeks
Cyclistic2019 <- subset(Cyclistic2019, duration > 1 & duration < 30240)
  

# Cleaning data - removing rows with missing values and duplicate rows
Cyclistic2020 <- Cyclistic2020 %>%
  distinct(.keep_all = TRUE) %>% 
  na.omit()

# Creating new columns with trip durations and weekdays
Cyclistic2020$duration <- difftime(Cyclistic2020$end, Cyclistic2020$start, units = "mins")
Cyclistic2020$weekday <- wday(Cyclistic2020$start, week_start = 1) 

# Removing outliers - trips shorter than 1 minute and longer than 3 weeks
Cyclistic2020 <- subset(Cyclistic2020, duration > 1 & duration < 30240)

# Renaming usertype values so both datasets are ready to merge
Cyclistic2019 <- Cyclistic2019 %>% 
  mutate(usertype = recode(usertype, "Subscriber" = "member", "Customer" = "casual"))

# Merging both datasets
CyclisticDataset <- bind_rows(Cyclistic2019,Cyclistic2020)

# Count of rides by type of user, also adding percentage of each!
RideCount <- CyclisticDataset %>% 
  group_by(usertype) %>% 
  tally() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>% 
  mutate(labels = scales::percent(perc))

# Count of rides by type of user and day of the week
RideCountGrouped <- CyclisticDataset %>% 
  group_by(usertype, weekday) %>% 
  tally()

# Mean duration of rides by user group
MeanDuration<- CyclisticDataset %>%
  group_by(usertype) %>% 
  summarize(MeanDuration = mean(duration))

# Mean duration of rides by users and day of the week
MeanDurationGrouped <- CyclisticDataset %>% 
  group_by(usertype, weekday) %>% 
  summarize(MeanDuration = mean(duration))

# Now it is time to visualise our data using ggplot2
library(ggplot2)

RidesChart <- ggplot(RideCount, aes(x ="", y = n, fill = usertype)) + # defining data
  geom_col(color = "black") + # painting ridges to separate values
  coord_polar(theta = "y") + # making it a pie chart
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) + # adding percentage to each pie slice
  guides(fill = guide_legend(title = "User type:")) + # Changing the legend title
  scale_fill_discrete(labels = c("Casual rider", "Cyclistic member")) + # Changing names of labels in legend
  theme_void() + # deleting base theme
  theme(legend.position = "bottom") + # adjusting legend to one side
  ggtitle("Percentage of total rides per user group")

RidesDayChart <- ggplot(RideCountGrouped, aes(x=weekday, y=n, fill=usertype)) + # defining data
  geom_col(position="dodge") + # using a column chart, with columns that dodge each other
  geom_text(
    aes(label = scales::comma(n), # setting text for each label
        hjust = ifelse(n <20000, 0.3,1), # moving text higher or lower depending on the values
        ),
    angle = 85, # for the text to be vertical instead
    position = position_dodge(width = 0.9), # aligning the labels with the columns
    size = 3 # text size
  ) +
  labs( # new labels
    title = "Traffic by day of the week: Casual riders vs. Members",
    x = "Day of the week",
    y = "Total number of rides",
    fill = "User type:",
  ) +
  scale_y_continuous(labels = NULL) + # removing numeric labels on axes
  scale_x_continuous(labels = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.line.y = element_blank(),
  )

MeanDuration$MeanDuration <- as.numeric(MeanDuration$MeanDuration, units = "mins") # converting to numeric to remove a warning

TripChart <- ggplot(MeanDuration, aes(x = usertype, y = MeanDuration, fill = usertype)) +
  geom_col() +
  labs( # new labels
    title = "Average trip duration",
    x = "User type",
    y = "Mean trip duration (minutes)",
    fill = "User type:"
  ) +
  scale_fill_discrete(labels = c("Casual rider", "Cyclistic member")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(
      size = 10,
      margin = margin(t=10,r=80,l=80))
  )

TripChart
