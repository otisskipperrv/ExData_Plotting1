setwd('/Users/oskipper/Documents/Data Science/DSAccelerator/SecondPhase/Coursera/ExploratoryDataAnlysis/ExData_Plotting1/')
rm(list =)
data <- read.table('household_power_consumption.txt', sep = ";", header = TRUE)
library(tidyverse)
library(lubridate)
library(ggpubr)

d1 <- data %>% 
  filter(Date == '1/2/2007' | Date == '2/2/2007')

d2 <- d1 %>% mutate(Date = dmy(Date))

d2$TheDateTime <- strptime(d1$Time, '%H:%M:%S') # Makes the date the current date
d2$TheDateTime <- as.POSIXct(d2$TheDateTime)

d2$TheDateTime <- update(d2$TheDateTime, year = year(d2$Date), month = month(d2$Date), day = day(d2$Date))

d2 <- d2 %>% 
  mutate(Global_active_power_kw = as.numeric(Global_active_power)) %>% 
  mutate(Global_active_power_kw = Global_active_power_kw/1000)



#Plot 2

p2 <- d2 %>% 
  # mutate(Global_active_power = as.numeric(Global_active_power)) %>% 
  #  mutate(Global_active_power = Global_active_power/1000) %>% 
  ggplot(aes(x = TheDateTime, y= Global_active_power_kw, group = 1))+
  geom_line() +
  ylab("Global Active Power (kilowats)") 

png(filename = "Plot2.png")
plot(p2)
dev.off()

