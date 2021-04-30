library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data
library(sf)           # to handle spatial vector data
library(terra)        # To handle raster data
library(lubridate)    # To handle dates and times

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv",",") # adjust path

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056, remove = FALSE)

# Task 1

wildschwein_BE <-wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

str(wildschwein_BE)

#### Questions to answer
##  How many individuals were tracked?
group_by(wildschwein_BE, TierID) # --> In the outcome of the function before, we can read, that there are three groups,so there were 3 individuals tracked. 

##  For how long were the individual tracked? Are there gaps? The individuals were tracked for 138.6 days. 
Min <- min(wildschwein_BE$DatetimeUTC)
Max <- max(wildschwein_BE$DatetimeUTC)
difftime(Max, Min, unit = "days")  # Time difference of 338.5834 days
# Yes there are gaps. Most timedifferences between recordings is about 900 seconds. Some are over 10000
# gaps seem to occur from 5 am to 2 pm every day. 

##  Were all individuals tracked concurrently or sequentially?
Min1 <- wildschwein_BE %>%
  group_by(TierID) %>%
  summarise(min(DatetimeUTC))
Max1 <-  wildschwein_BE %>%
  group_by(TierID) %>%
  summarise(max(DatetimeUTC))
# Looking at the two dataframes Min1 & Max 1 we can see that the individuals were tracked concurrently

##  What is the temporal sampling interval between the locations? --> Most of the time its about 900 seconds.
wildschwein_BE %>% 
  group_by(TierID) %>%
  summarise(mean(timelag))


  