library(ggplot2)
library(plyr)

# Canada Collision Data
fatalities <- c(3230,3313,3129,3076,2919,2980,2904,2758,2921,2777,2735,2898,2871,2753,2431,2216,2238,2023,2076,1923)
serious_injuries <- c(21564,20188,18734,17294,16410,16187,15581,15296,15894,15110,15572,15792,16044,14410,12851,11955,11796,10940,11116,103125)
total_injuries <- c(241899,238458,227283,217401,213319,218457,222848,216542,222665,216123,206104,204701,199976,192745,176394,170770,172081,167740,166872,165306)
date <- 1994:2013
non_serious_injuries <- total_injuries - serious_injuries

# Combine data into single data frame
injuries_data <- data.frame(date, fatalities, serious_injuries, non_serious_injuries, total_injuries)

# Plot Total Fatalities
plot1 <- ggplot() + geom_line(data=injuries_data, aes(x=date, y=fatalities)) + ggtitle("Total Fatalities from Collisions: 1994->2013") +theme(plot.title = element_text(lineheight=.8, face="bold"))+
    ylab("Fatalities") + xlab("Year") + scale_y_continuous(limits=c(0, 4000))
ggsave(file="Fatalities.png")

# Plot Total Injuries
plot2 <- ggplot() + geom_line(data=injuries_data, aes(x=date, y=total_injuries))+ ggtitle("Total Injuries from Collisions: 1994->2013") +theme(plot.title = element_text(lineheight=.8, face="bold"))+
    ylab("Injuries") + xlab("Year") + scale_y_continuous(limits=c(0, 250000))
ggsave(file="Injuries.png")












