# NOX observations 
require(ggplot2)
require(lubridate)
require(ggvis)
require(dygraphs)
require(dplyr)
require(reshape2)
require(gridExtra)

noxOct <- read.table(file = "data/data_oct.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxSept <- read.table(file = "data/data_sept.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxAug <- read.table(file = "data/data_aug.csv", stringsAsFactors = F, sep = ";")
columns <- c("DateTimeN", "Sensor2_NO", "DateTimeC", "Sensor2_CO", "DateTimeT", "Sensor2_Temperature", 
             "DateTimeH","Sensor2_Humidity", "DateTimeB", "Sensor2_Battery") 
names(noxOct) <- columns; names(noxSept) <- columns

editor <- function(frames){
    names(frames) <- c("DateTimeN", "Sensor2_NO", "DateTimeC", "Sensor2_CO", "DateTimeT", "Sensor2_Temperature", 
                       "DateTimeH","Sensor2_Humidity", "DateTimeB", "Sensor2_Battery") 
    frames$DateTimeN <- ymd_hms(frames$DateTimeN);frames$DateTimeC <- ymd_hms(frames$DateTimeC)
    frames$DateTimeT <- ymd_hms(frames$DateTimeT);frames$DateTimeH <- ymd_hms(frames$DateTimeH)
    frames$DateTimeB <- ymd_hms(frames$DateTimeB)
    frames$Month <- month(frames$DateTimeN)
    frames 
}
#update the original dataframe 
noxSept <- editor(noxSept)
noxOct <- editor(noxOct)

#ggplot2 graphs
myplot <- function(mydata){
    #TODO
    # Edit the plots to add the hlines for the limits and make it look like NOX plots
    # Edit to add multiple sensor readings on a single plot
    no <- ggplot(data = mydata)+geom_line(aes(x=DateTimeN,y=Sensor2_NO),colour= "gold")+
        geom_hline(aes(yintercept=0.1064), colour= "tan4" , linetype= 5)
    co <- ggplot(data = mydata)+geom_line(aes(x=DateTimeC,y=Sensor2_CO),colour= "dodgerblue2")+
        geom_hline(aes(yintercept=8.73), colour= "tan4", linetype= 5)
    temp <- ggplot(data = mydata)+geom_line(aes(x=DateTimeT,y=Sensor2_Temperature),colour= "orangered")
    humi <- ggplot(data = mydata)+geom_line(aes(x=DateTimeH,y=Sensor2_Humidity),colour= "darkolivegreen4")
    grid.arrange(no,co,temp,humi, nrow=4)    # how to add padding to each plot?
}





