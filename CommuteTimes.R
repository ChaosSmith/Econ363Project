library(ggplot2)
library(plyr)

#Take a look at Public Transit vs Cars

# Pull data from Stat Can website
url <- "http://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/File.cfm?S=0&LANG=E&A=R&PID=107643&GID=1118296&D1=5&D2=0&D3=0&D4=0&D5=0&D6=0&OFT=CSV"
commute_data_60 <- read.csv(url, skip = 8)[0:20,0:2] # 60 + min
commute_data_60 <-rename(commute_data_60,c("Mode.of.transportation..20."="type","Total...Commuting.type"="total"))

url <- "http://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/File.cfm?S=0&LANG=E&A=R&PID=107643&GID=1118296&D1=4&D2=0&D3=0&D4=0&D5=0&D6=0&OFT=CSV"
commute_data_45 <- read.csv(url, skip = 8)[0:20,0:2] #45 min to 59 min
commute_data_45 <-rename(commute_data_45,c("Mode.of.transportation..20."="type","Total...Commuting.type"="total"))

url <- "http://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/File.cfm?S=0&LANG=E&A=R&PID=107643&GID=1118296&D1=3&D2=0&D3=0&D4=0&D5=0&D6=0&OFT=CSV"
commute_data_30 <- read.csv(url, skip = 8)[0:20,0:2] #30 min to 44 min
commute_data_30 <-rename(commute_data_30,c("Mode.of.transportation..20."="type","Total...Commuting.type"="total"))

url <- "http://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/File.cfm?S=0&LANG=E&A=R&PID=107643&GID=1118296&D1=2&D2=0&D3=0&D4=0&D5=0&D6=0&OFT=CSV"
commute_data_15 <- read.csv(url, skip = 8)[0:20,0:2] #15 min to 29 min
commute_data_15 <- rename(commute_data_15,c("Mode.of.transportation..20."="type","Total...Commuting.type"="total"))

url <- "http://www12.statcan.gc.ca/nhs-enm/2011/dp-pd/dt-td/File.cfm?S=0&LANG=E&A=R&PID=107643&GID=1118296&D1=1&D2=0&D3=0&D4=0&D5=0&D6=0&OFT=CSV"
commute_data_less_15 <- read.csv(url, skip = 8)[0:20,0:2] #less than 15
commute_data_less_15 <- rename(commute_data_less_15,c("Mode.of.transportation..20."="type","Total...Commuting.type"="total"))

#Combine data into single data frame
commute_list <- list(commute_data_60,commute_data_45,commute_data_30,commute_data_15,commute_data_less_15)
commute_list = rev(commute_list)
public_transit_func <- function(x){
    x[11,2]
}
total_freq_func <- function(x){
    x[1,2]
}
car_freq_func <- function(x){
    x[2,2]
}
public_transit_freq = as.numeric(lapply(commute_list,public_transit_func))
total_frequency = as.numeric(lapply(commute_list,total_freq_func))
car_frequency = as.numeric(lapply(commute_list,car_freq_func))
times <- c("<15","15-29","30-44","45-59","60<")
commute_by_type <- data.frame(times,total_frequency,car_frequency,public_transit_freq)

# Generate Plots
levels(commute_by_type$times) <- c("<15","15:29","30:44","45:59",">60")
public_transit_plot <-ggplot(commute_by_type, aes(x=times, y= public_transit_freq)) + geom_bar()+ ggtitle("Public Transit Commute Distribution") +theme(plot.title = element_text(lineheight=.8, face="bold"))+
    ylab("Frequency") + xlab("Commute Time")
ggsave(file="publictransit.png")
car_plot <-ggplot(commute_by_type, aes(x=times, y= car_frequency)) + geom_bar()+ ggtitle("Personal Car Commute Distribution") + theme(plot.title = element_text(lineheight=.8, face="bold"))+
    ylab("Frequency") + xlab("Commute Time")
ggsave(file="carplot.png")

# Calculating Yearly Commute Hours
avg_times <- c(7.5,22,37,52,60)
total_commute <- data.frame(avg_times, total_frequency)
total_commute$total_time <- total_commute$avg_times * total_commute$total_frequency * 2
yearly_commute_hours <- sum(total_commute$total_time) * 253 / 60 # total minutes per day * 253 working days in a year divided by 60 minutes in an hour

