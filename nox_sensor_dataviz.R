# NOX observations 
require(ggplot2)
require(lubridate)
require(dplyr)
require(reshape2)
require(gridExtra)
require(RCurl)
require(rjson)

#TODO 
# aarhus univ prof modeling outdoor air quality

# this gives column names by itself 
# TODO
# should factor the sensors and create buckets for different sensors
# we should just apply time conversion once a month is selected
#keen <- read.table(file = "546356193831445dc0f141be-FirstTest-log-1447690322-J69FIW", stringsAsFactors = F, 
#                   sep = ",", header = T, skip = 0)

#nox14 <- bind_rows(l) # l is the list of all the csv inputs
nox14 <- read.table(file = "data/nox2014.csv", sep = ",", stringsAsFactors = F, header = T, skip=0)[-1]
nox14$Street <- rep("1",nrow(nox14))

# can also consider rbindlist from data.table might be faster for larger datasets
# while creating databse create headers such as Sensor 1

# convert into specific data frames
no <- as.data.frame(nox14[,grep(pattern = "^DateTime$",x = names(nox14), value=T)])
no <- cbind(no, nox14$Street, as.data.frame(nox14[,grep(pattern = "NO",x = names(nox14), fixed = T ,value=T)]))
date <- data.frame(do.call(rbind,strsplit(as.character.POSIXt(no$`nox14[, grep(pattern = "^DateTime$", x = names(nox14), value = T)]`), " ")))
no$date <- as.character(date[,1])
noMelted <- melt(data = no, id.vars = c(names(no)[1],names(no)[2],names(no)[6]))  #the last column for the date selector will change according to the number of sensors
noMelted$variable <- ifelse(test = noMelted$variable=="Sensor.1...NO...ppm.","Sensor 1",ifelse(test = noMelted$variable=="Sensor.2...NO...ppm.","Sensor 2","Sensor 3"))

co <- as.data.frame(nox14[,grep(pattern = "^DateTime.3$",x = names(nox14), value=T)])  # because the interval is different
co <- cbind(co, nox14$Street, as.data.frame(nox14[,grep(pattern = "CO",x = names(nox14), fixed = T ,value=T)]))
date <- data.frame(do.call(rbind,strsplit(as.character.POSIXt(co[,1]), " ")))
co$date <- as.character(date[,1])
coMelted <- melt(data = co, id.vars = c(names(co)[1],names(co)[2],names(co)[6]))
coMelted$variable <- ifelse(test = coMelted$variable=="Sensor.1...CO..ppm.","Sensor 1",ifelse(test = coMelted$variable=="Sensor.2...CO..ppm.","Sensor 2","Sensor 3"))

temperature <- as.data.frame(nox14[,grep(pattern = "^DateTime$",x = names(nox14), value=T)])
temperature <- cbind(temperature, nox14$Street, as.data.frame(nox14[,grep(pattern = "Temperature",x = names(nox14), fixed = T ,value=T)]))
date <- data.frame(do.call(rbind,strsplit(as.character.POSIXt(temperature$`nox14[, grep(pattern = "^DateTime$", x = names(nox14), value = T)]`), " ")))
temperature$date <- as.character(date[,1])
temperatureMelted <- melt(data = temperature, id.vars = c(names(temperature)[1],names(temperature)[2],names(temperature)[6]))
temperatureMelted$variable <- ifelse(test = temperatureMelted$variable=="Sensor.1...Temperature...C.","Sensor 1",
                                     ifelse(test = temperatureMelted$variable=="Sensor.2...Temperature...C.","Sensor 2","Sensor 3"))

humidity <- as.data.frame(nox14[,grep(pattern = "^DateTime$",x = names(nox14), value=T)])
humidity <- cbind(humidity, nox14$Street, as.data.frame(nox14[,grep(pattern = "Humidity",x = names(nox14), fixed = T ,value=T)]))
date <- data.frame(do.call(rbind,strsplit(as.character.POSIXt(humidity$`nox14[, grep(pattern = "^DateTime$", x = names(nox14), value = T)]`), " ")))
humidity$date <- as.character(date[,1])
humidityMelted <- melt(data = humidity, id.vars = c(names(humidity)[1],names(humidity)[2],names(humidity)[6]))
humidityMelted$variable <- ifelse(test = humidityMelted$variable=="Sensor.1...Humidity....","Sensor 1",
                                     ifelse(test = humidityMelted$variable=="Sensor.2...Humidity....","Sensor 2","Sensor 3"))

battery <- as.data.frame(nox14[,grep(pattern = "^DateTime$",x = names(nox14), value=T)])
battery <- cbind(battery, nox14$Street, as.data.frame(nox14[,grep(pattern = "Battery",x = names(nox14), fixed = T ,value=T)]))
date <- data.frame(do.call(rbind,strsplit(as.character.POSIXt(battery$`nox14[, grep(pattern = "^DateTime$", x = names(nox14), value = T)]`), " ")))
battery$date <- as.character(date[,1])
batteryMelted <- melt(data = battery, id.vars = c(names(battery)[1],names(battery)[2],names(battery)[6]))
batteryMelted$variable <- ifelse(test = batteryMelted$variable=="Sensor.1...Battery....","Sensor 1",
                                     ifelse(test = batteryMelted$variable=="Sensor.2...Battery....","Sensor 2","Sensor 3"))

# convert character timestamps into date objects
timeconvert <- function(dataset){
    if (is.list(dataset) || is.data.frame(dataset)){
        dataset[,1] <- ymd_hms(dataset[,1])
        return(dataset[,1])
    }
}
no[,1] <- timeconvert(no);co[,1] <- timeconvert(co);temperature[,1] <- timeconvert(temperature);humidity[,1] <- timeconvert(humidity);battery[,1] <- timeconvert(battery)
noMelted[,1] <- timeconvert(no);coMelted[,1] <- timeconvert(co);temperatureMelted[,1] <- timeconvert(temperature);humidityMelted[,1] <- timeconvert(humidity);batteryMelted[,1] <- timeconvert(battery)
names(noMelted)<- c("time","street","date","variable","value");names(coMelted)<- c("time","street","date","variable","value")
names(temperatureMelted)<- c("time","street","date","variable","value");names(humidityMelted)<- c("time","street","date","variable","value")
names(batteryMelted)<- c("time","street","date","variable","value")


# ggplot2 graphs
noplot <- function(data){
    #ggplot()+geom_line(data=no, aes(x=no[,1], y=no[,3]))  
    p <- ggplot(data = data)+geom_line(aes(x=time, y=value, color=variable))
    p
}
coplot <- function(){
    ggplot()+geom_line(data=coMelted, aes(x=coMelted[which(coMelted$variable=="Sensor.1...CO..ppm."),1], y=coMelted[which(coMelted$variable=="Sensor.1...CO..ppm."),4]))    
}



    

