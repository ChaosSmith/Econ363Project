library(ggplot2)
library(plyr)

# Yearly registration data

yearly_registrations <- read.csv("YearlyAutoRegistrations1999-2014.csv", skip = 4,nrows=16)
# Create main data frame
fleet_turnover <- rename(yearly_registrations,c("Type.of.vehicle"="Year","Total..road.motor.vehicle.registrations"="Registered"))

# Monthly sales data
monthly_sales <- read.csv("MonthlyCarSales1999-2014.csv",skip = 7,nrows=192)
monthly_sales <- rename(monthly_sales,c("Seasonal.adjustment"="Month", "Unadjusted"="Sales"))

# Sum monthly sales into yearly sales
year_sales = list()
for (i in fleet_turnover$Year){
    year_sales <- append(sum(monthly_sales[grepl(toString(i), monthly_sales$Month),2]), year_sales)
}

# Include yearly sales in main data frame
fleet_turnover$Sales <- rev(as.numeric(year_sales))

# Calculate yearly fleet turnover ratios Sales/Registered
fleet_turnover$Ratio <- fleet_turnover$Sales / fleet_turnover$Registered

#Calculate 15 year mean
mean(fleet_turnover$Ratio)

FleetTurnOver <- ggplot(fleet_turnover, aes(x=Year, y=Ratio)) + geom_line() + ggtitle("Turnover Ratio 1999->2014")+ 
theme(plot.title = element_text(lineheight=.8, face="bold")) + scale_x_continuous(breaks=c(1999,2001,2003,2005,2007,2009,2011,2013,2015))+
geom_line(stat = "hline", yintercept = "mean", aes(colour = "red")) + theme(legend.position="none") + ylab("Sales / Total Registrations")
ggsave(file="TurnOver.png")