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
  mutate(timelag3 = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_3 <- caro_3 %>%
  mutate(steplength3 = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_3 <- caro_3 %>%
  mutate(speed3 = steplength3/timelag3)

#calculating timelag, steplength and speed for Caro_6
caro_6 <-caro_6 %>%
  mutate(timelag6 = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_6 <- caro_6 %>%
  mutate(steplength6 = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_6 <- caro_6 %>%
  mutate(speed6 = steplength6/timelag6)

#calculating timelag, steplength and speed for Caro_9
caro_9 <-caro_9 %>%
  mutate(timelag9 = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC, units = "secs")))

caro_9 <- caro_9 %>%
  mutate(steplength9 = sqrt((E- lead(E,1))^2 + (N -lead(N,1))^2))

caro_9 <- caro_9 %>%
  mutate(speed9 = steplength9/timelag9)

# Lineplot with different speeds
Lineplot <- ggplot() +
  geom_line(data = caro, aes(x= DatetimeUTC, y = speed, color = "speed"), size = 0.8) +
  geom_line(data = caro_3, aes(x= DatetimeUTC, y = speed3, color = "speed3"), size =0.8) +
  geom_line(data = caro_6, aes(x= DatetimeUTC, y = speed6, color = "speed6"), size =0.8)  +
  geom_line(data = caro_9, aes(x= DatetimeUTC, y = speed9, color = "speed9"), size =0.8) +
  theme_light() +
  labs(x = "Time", y= "Speed", color = "Colour", title = "Comparing derived speed at different sampling intervals") +
  scale_color_manual(values = c("speed" = "black", "speed3" = "orange", "speed6" = "green", "speed9" = "violet"), labels = c("1 minute", "3 minutes", "6 minutes", "9 minutes")) +
  theme(axis.title = element_text(vjust = 2, size = 15),
        panel.border = element_blank(), 
        title = element_text(vjust = 2, size =15)) 


# The higher the sampling rate the more fluctuations there are in the data. The lower the sampling rate the more information is "lost" and extrema don't look as extreme.

# how do i add a legend?

Plot1 <- ggplot(data = caro, aes(x= E, y= N)) +
  geom_sf(aes(color = "N")) +
  geom_path(aes(color = "N")) + 
  geom_sf(data = caro_3, aes(color = "E")) +
  geom_path(data = caro_3, aes(color = "E")) +
  coord_sf(datum = st_crs(2056)) +
  scale_x_continuous("E", 
                     limits = c(1205030, 1205140), 
                     breaks = seq(1205030, 1205140, by = 20)) +
  scale_y_continuous("N", limits = c(2570480,2570640), breaks = seq(2570480,2570640, by = 40)) +
  theme_light() +
  labs(color = "Trajectory", 
       title = "Comparing original- with 3 minutes-resample data") +
  scale_color_manual(values = c("N" = "black", "E" = "orange"), 
                     labels = c("3 minutes", "1 minute")) +
  theme(panel.border = element_blank(),
        title = element_text(vjust=2, size=15),
        axis.title = element_text(vjust = 2, size = 15))


Plot2 <- ggplot(data = caro, aes(x= E, y= N)) +
  geom_sf(aes(color = "N")) +
  geom_path(aes(color = "N")) + 
  geom_sf(data = caro_6, aes(color = "E")) +
  geom_path(data = caro_6, aes(color = "E")) +
  coord_sf(datum = st_crs(2056)) +
  scale_x_continuous("E", 
                     limits = c(1205030, 1205140), 
                     breaks = seq(1205030, 1205140, by = 20)) +
  scale_y_continuous("N", limits = c(2570480,2570640), breaks = seq(2570480,2570640, by = 40)) +
  theme_light() +
  labs(color = "Trajectory", 
       title = "Comparing original- with 6 minutes-resample data") +
  scale_color_manual(values = c("N" = "black", "E" = "orange"), 
                     labels = c("6 minutes", "1 minute")) +
  theme(panel.border = element_blank(),
        title = element_text(vjust=2, size=15),
        axis.title = element_text(vjust = 2, size = 15))

Plot3 <- ggplot(data = caro, aes(x= E, y= N)) +
  geom_sf(aes(color = "N")) +
  geom_path(aes(color = "N")) + 
  geom_sf(data = caro_9, aes(color = "E")) +
  geom_path(data = caro_9, aes(color = "E")) +
  coord_sf(datum = st_crs(2056)) +
  scale_x_continuous("E", 
                     limits = c(1205030, 1205140), 
                     breaks = seq(1205030, 1205140, by = 20)) +
  scale_y_continuous("N", limits = c(2570480,2570640), breaks = seq(2570480,2570640, by = 40)) +
  theme_light() +
  labs(color = "Trajectory", 
       title = "Comparing original- with 9 minutes-resample data") +
  scale_color_manual(values = c("N" = "black", "E" = "orange"), 
                     labels = c("9 minutes", "1 minute")) +
  theme(panel.border = element_blank(),
        title = element_text(vjust=2, size=15),
        axis.title = element_text(vjust = 2, size = 15))


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


