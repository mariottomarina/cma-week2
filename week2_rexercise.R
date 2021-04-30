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

# Task 2
wildschwein_BE <- wildschwein_BE %>%
  group_by(TierID) %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

# calculating speed  distance devided by zeitinterval
wildschwein_BE <- wildschwein_BE %>% 
  group_by(TierID) %>%
  mutate(speed = steplength/timelag)

## Unit of Speed??

# Task 3
caro <- read_delim("caro60.csv",",") # adjust path

caro <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

caro_3 <- caro[seq(1,nrow(caro),by=3),]
caro_6 <- caro[seq(1,nrow(caro),by=6),]
caro_9 <- caro[seq(1,nrow(caro),by=9),]
nrow(caro)
nrow(caro_3)
nrow(caro_6)
nrow(caro_9)


#calculating timelag, steplength and speed for Caro
caro <-caro %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro <- caro %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro <- caro %>%
  mutate(speed = steplength/timelag)

#calculating timelag, steplength and speed for Caro_3
caro_3 <-caro_3 %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_3 <- caro_3 %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_3 <- caro_3 %>%
  mutate(speed = steplength/timelag)

#calculating timelag, steplength and speed for Caro_6
caro_6 <-caro_6 %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_6 <- caro_6 %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_6 <- caro_6 %>%
  mutate(speed = steplength/timelag)

#calculating timelag, steplength and speed for Caro_9
caro_9 <-caro_9 %>%
  mutate(timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_9 <- caro_9 %>%
  mutate(steplength = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_9 <- caro_9 %>%
  mutate(speed = steplength/timelag)

# Lineplot with different speeds
Lineplot <- ggplot() +
  geom_line(data = caro, aes(x= DatetimeUTC, y = speed), color = "orange", size = 0.6) +
  geom_line(data = caro_3, aes(x= DatetimeUTC, y = speed), color = "darkgreen", size =0.6) +
  geom_line(data = caro_6, aes(x= DatetimeUTC, y = speed), color = "blue", size =0.6)  +
  geom_line(data = caro_9, aes(x= DatetimeUTC, y = speed), color = "violet", size =0.6) +
  ylab("Speed (m/s)") +
  xlab("Time") +
  theme_bw() +
  theme(axis.title = element_text(vjust = 2, size = 15),
        panel.border = element_blank()) 

# The higher the sampling rate the more fluctuations there are in the data. The lower the sampling rate the more information is "lost" and extrema don't look as extreme.

# how do i add a legend?

Plot1 <- ggplot(data = caro, aes(x= E, y= N)) +
  geom_sf(color = "orange") +
  geom_path(color = "orange") + 
  geom_sf(data = caro_3, color = "blue") +
  geom_path(data = caro_3, color = "blue") +
  coord_sf(datum = st_crs(2056)) +
  scale_x_continuous("E", limits = c(1205050, 1205130), breaks = seq(1205050, 1205130, by = 20)) +
  theme_bw() +
  theme(panel.border = element_blank())

Plot2 <- ggplot(data = caro, aes(x= E, y= N)) +
  geom_sf(color = "orange") +
  geom_path(color = "orange") + 
  geom_sf(data = caro_6, color = "blue") +
  geom_path(data = caro_6, color = "blue") +
  coord_sf(datum = st_crs(2056)) +
  scale_x_continuous("E", limits = c(1205050, 1205130), breaks = seq(1205050, 1205130, by = 20)) +
  theme_bw() +
  theme(panel.border = element_blank())

Plot3 <- ggplot(data = caro, aes(x= E, y= N)) +
  geom_sf(color = "orange") +
  geom_path(color = "orange") + 
  geom_sf(data = caro_9, color = "blue") +
  geom_path(data = caro_9, color = "blue") +
  coord_sf(datum = st_crs(2056)) +
  scale_x_continuous("E", limits = c(1205050, 1205130), breaks = seq(1205050, 1205130, by = 20)) +
  theme_bw() +
  theme(panel.border = element_blank())

# Task 4

library(zoo)
example <- rnorm(10)
example
rollmean(example,k = 3,fill = NA,align = "left")
rollmean(example,k = 4,fill = NA, align = "left")

carowindow <- caro
carowindow$k2 <- rollmean(carowindow$speed, k=2, fill = NA, align = "left")
carowindow$k3 <- rollmean(carowindow$speed, k=3, fill = NA, align = "left")
carowindow$k4 <- rollmean(carowindow$speed, k=4, fill = NA, align = "left")
carowindow$k6 <- rollmean(carowindow$speed, k=6, fill = NA, align = "left")
carowindow$k10 <- rollmean(carowindow$speed, k=10, fill = NA, align = "left")


windowfunc_plot <- ggplot(carowindow, aes(x=DatetimeUTC, y=speed)) +
  geom_line(color = "black", size = 0.75) +
  geom_line(data = carowindow, aes(y=k2, color = "k2"), size =0.6)+
  geom_line(data = carowindow, aes(y=k3, color = "k3"), size =0.6)+
  geom_line(data = carowindow, aes(y=k4, color = "k4"), size =0.6)+
  geom_line(data = carowindow, aes(y=k6, color = "k6"), size =0.6)+
  geom_line(data = carowindow, aes(y=k10, color = "k10"), size =0.6)+
  theme_light() +
  labs(x = "Time", y= "Speed", color = "Legend") +
  scale_color_manual(values = c("k2" = "firebrick3", "k3" = "dodgerblue3", "k4" = "darkorchid3", "k6" = "darkolivegreen3", "k10"= "goldenrod3"))


