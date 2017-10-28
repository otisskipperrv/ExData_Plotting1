setwd('/Users/oskipper/Documents/Data Science/DSAccelerator/SecondPhase/Coursera/ExploratoryDataAnlysis/ExData_Plotting1/')
rm(list =)
data <- read.table('household_power_consumption.txt', sep = ";", header = TRUE)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(scales)

d1 <- data %>% 
  filter(Date == '1/2/2007' | Date == '2/2/2007')

d2 <- d1 %>% mutate(Date = dmy(Date))

d2$TheDateTime <- strptime(d1$Time, '%H:%M:%S') # Makes the date the current date
d2$TheDateTime <- as.POSIXct(d2$TheDateTime)

d2$TheDateTime <- update(d2$TheDateTime, year = year(d2$Date), month = month(d2$Date), day = day(d2$Date))

d2 <- d2 %>% 
  mutate(Global_active_power_kw = as.numeric(Global_active_power)) %>% 
  mutate(Global_active_power_kw = Global_active_power_kw/1000)



#Plot 1


p1 <- d2 %>% 
  ggplot(aes(x = Global_active_power_kw)) +
  geom_histogram(stat = "bin", bins = 10, fill = "red", color = "black") +
  xlab("Global Active Power (kilowats)") +
  ylab("Frequency") +
  ggtitle("Global Active Power") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_datetime(labels=date_format("%m-%d"))


#Plot 2

p2 <- d2 %>% 
  # mutate(Global_active_power = as.numeric(Global_active_power)) %>% 
  #  mutate(Global_active_power = Global_active_power/1000) %>% 
  ggplot(aes(x = TheDateTime, y= Global_active_power_kw, group = 1))+
  geom_line() +
  ylab("Global Active Power (kilowats)") +
  scale_x_datetime(labels=date_format("%m-%d"))


#Plot 3
p3 <- d2 %>% 
  select(TheDateTime, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>% 
  gather(Sub_metering_1, Sub_metering_3, Sub_metering_2, key = sub_metering_type, value = sub_metering_value) %>% 
  mutate(sub_metering_value = as.numeric(sub_metering_value)) %>% 
  ggplot(aes(x = TheDateTime, y = sub_metering_value, group = sub_metering_type, color = sub_metering_type)) +
  geom_line() +
  ylab("Energy Sub Metering") +
  xlab("") +
  scale_x_datetime(labels=date_format("%m-%d"))

#Plot 4
t1<- d2 %>% 
  mutate(Voltage = as.numeric(Voltage)/5) %>% 
  ggplot(aes(x = TheDateTime, y = Voltage)) +
  geom_line() +
scale_x_datetime(labels=date_format("%m-%d"))



t2 <- d2 %>% 
  mutate(Global_reactive_power = as.numeric(Global_reactive_power)) %>% 
  mutate(Global_reactive_power = Global_reactive_power/1000) %>% 
  ggplot(aes(x = TheDateTime, y = Global_reactive_power)) +
  geom_line() +
  scale_x_datetime(labels=date_format("%m-%d"))

# Arrange all 4 plots on same plot

tfinal <- ggarrange(p2, t1, p3, t2 ,
                    #labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2)
png(filename = "Plot4.png")
plot(tfinal)
dev.off()


