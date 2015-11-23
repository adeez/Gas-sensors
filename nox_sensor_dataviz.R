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

# communicate with keenio api to directly fetch the data
#basicurl <- "https://api.keen.io/3.0/projects/546356193831445dc0f141be/queries/extraction?"
#api_key <- "api_key=d2248cf3ef2c0524c54dc51fc1f383a222bee48043978c81cb55cdb84ab9f4f5ed0f3eaa316d129efcb3edb95f31e6ced2579b86d1bda255827b23a4d4bbcf49bb6d817dafc69e03f0343439df0861b0a1e6e94199e76aa96929b09a4418066ded1fdcf1d919b6d0cc2851dfe493f4e9"
#query2 <- "&event_collection=log&latest=1"
#url2 <- paste0(basicurl,api_key,query2)
#keen1 <- fromJSON(getURL(url1))

# wrap this in a function which can be called using the url
#i=1
#keen2 <- vector("list")
#keen2r <- vector("list")
#sleeper <- function(con){
#    for (i in seq(1,3,by=1)){
#        keen2 <- fromJSON(getURL(con))    
#        keen2r <- c(keen2r, keen2$result)
#        Sys.sleep(90)
#    }
#    return(keen2r)
#}
#keencheck <- sleeper(url2)
#ifelse(keencheck$name==1,keendf1 <- data.frame(do.call(cbind.data.frame, keencheck)),
#       keendf2 <- data.frame(do.call(cbind.data.frame, keencheck)))



# this gives column names by itself 
# TODO
# should factor the sensors and create buckets for different sensors
# we should just apply time conversion once a month is selected
#keen <- read.table(file = "546356193831445dc0f141be-FirstTest-log-1447690322-J69FIW", stringsAsFactors = F, 
#                   sep = ",", header = T, skip = 0)

noxJan <- read.table(file = "data/data_jan.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxFeb <- read.table(file = "data/data_feb.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxMarch <- read.table(file = "data/data_march.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxApril <- read.table(file = "data/data_april.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxMay <- read.table(file = "data/data_may.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxJune <- read.table(file = "data/data_june.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxJuly <- read.table(file = "data/data_july.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxAug <- read.table(file = "data/data_aug.csv", stringsAsFactors = F, sep = ";", skip= 1)
noxSept <- read.table(file = "data/data_sept.csv", stringsAsFactors = F, sep = ";", skip = 1)
noxOct <- read.table(file = "data/data_oct.csv", stringsAsFactors = F, sep = ";", skip = 1)

columns3 <- c("DateTimeN","Sensor1_NO","Sensor2_NO", "Sensor3_NO", "DateTimeC", "Sensor1_CO", "Sensor2_CO", "Sensor3_CO", 
              "DateTimeT", "Sensor1_Temperature","Sensor2_Temperature","Sensor3_Temperature", 
              "DateTimeH","Sensor1_Humidity", "Sensor2_Humidity", "Sensor3_Humidity",
              "DateTimeB", "Sensor1_Battery","Sensor2_Battery","Sensor3_Battery") 
columns2 <- c("DateTimeN","Sensor1_NO","Sensor2_NO", "DateTimeC", "Sensor1_CO", "Sensor2_CO", 
              "DateTimeT", "Sensor1_Temperature","Sensor2_Temperature", 
              "DateTimeH","Sensor1_Humidity", "Sensor2_Humidity",
              "DateTimeB", "Sensor1_Battery","Sensor2_Battery") 
columns <- c("DateTimeN", "Sensor2_NO", "DateTimeC", "Sensor2_CO", "DateTimeT", "Sensor2_Temperature", 
             "DateTimeH","Sensor2_Humidity", "DateTimeB", "Sensor2_Battery") 

# resize data with no duplicate date columns 
dataSizing <- function(object){
  ifelse (ncol(object)==30, object <- object[,-c(3,5,9,11,15,17,21,23,27,29)],
            ifelse (ncol(object)==20, object <- object[,-c(3,7,11,15,19)], object))
  return(object)
} 
# update the original dataframe 
noxJan <- dataSizing(noxJan); noxFeb <- dataSizing(noxFeb); noxMarch <- dataSizing(noxMarch)
noxApril <- dataSizing(noxApril);noxMay <- dataSizing(noxMay);noxJune <- dataSizing(noxJune)
noxJuly <- dataSizing(noxJuly);noxAug <- dataSizing(noxAug);noxSept <- dataSizing(noxSept); noxOct <- dataSizing(noxOct)

# convert into date objects
editor <- function(frames){
    if (ncol(frames)==10){
        names(frames) <- columns
        frames$DateTimeN <- ymd_hms(frames$DateTimeN);frames$DateTimeC <- ymd_hms(frames$DateTimeC)
        frames$DateTimeT <- ymd_hms(frames$DateTimeT);frames$DateTimeH <- ymd_hms(frames$DateTimeH)
        frames$DateTimeB <- ymd_hms(frames$DateTimeB)
        frames$Month <- month(frames$DateTimeN)
    }
    if (ncol(frames)==15){
        names(frames) <- columns2
        frames$DateTimeN <- ymd_hms(frames$DateTimeN);frames$DateTimeC <- ymd_hms(frames$DateTimeC)
        frames$DateTimeT <- ymd_hms(frames$DateTimeT);frames$DateTimeH <- ymd_hms(frames$DateTimeH)
        frames$DateTimeB <- ymd_hms(frames$DateTimeB)
        frames$Month <- month(frames$DateTimeN)
    }
    if (ncol(frames)==20){
        names(frames) <- columns3
        frames$DateTimeN <- ymd_hms(frames$DateTimeN);frames$DateTimeC <- ymd_hms(frames$DateTimeC)
        frames$DateTimeT <- ymd_hms(frames$DateTimeT);frames$DateTimeH <- ymd_hms(frames$DateTimeH)
        frames$DateTimeB <- ymd_hms(frames$DateTimeB)
        frames$Month <- month(frames$DateTimeN)
    }
    frames
}
# update the original dataframe 
noxJan <- editor(noxJan);noxFeb <- editor(noxFeb);noxMarch <- editor(noxMarch);noxApril <- editor(noxApril);
noxMay <- editor(noxMay);noxJune <- editor(noxJune);noxJuly <- editor(noxJuly);noxAug <- editor(noxAug);
noxSept <- editor(noxSept);noxOct <- editor(noxOct)

# ggplot2 graphs
myplot <- function(mydata){
    if (ncol(mydata)==11){
        no <- ggplot(data = mydata)+geom_line(aes(x=DateTimeN,y=Sensor2_NO),colour= "gold")+
            geom_hline(aes(yintercept=0.1064), colour= "tan4" , linetype= 5)
        co <- ggplot(data = mydata)+geom_line(aes(x=DateTimeC,y=Sensor2_CO),colour= "dodgerblue2")+
            geom_hline(aes(yintercept=8.73), colour= "tan4", linetype= 5)
        temp <- ggplot(data = mydata)+geom_line(aes(x=DateTimeT,y=Sensor2_Temperature),colour= "orangered")
        humi <- ggplot(data = mydata)+geom_line(aes(x=DateTimeH,y=Sensor2_Humidity),colour= "darkolivegreen4")
        grid.arrange(no,co,temp,humi, nrow=4)    # how to add padding to each plot?
    }
    if (ncol(mydata)==16){
        no <- ggplot(data = mydata)+geom_line(aes(x=DateTimeN,y=Sensor1_NO),colour= "gold")+
            geom_line(aes(x=DateTimeN,y=Sensor2_NO),colour= "darkolivegreen4")+ 
            geom_hline(aes(yintercept=0.1064), colour= "tan4" , linetype= 5)
        co <- ggplot(data = mydata)+geom_line(aes(x=DateTimeC,y=Sensor1_CO),colour= "dodgerblue2")+
            geom_line(aes(x=DateTimeC,y=Sensor2_CO),colour= "orangered")+
            geom_hline(aes(yintercept=8.73), colour= "tan4", linetype= 5)
        temp <- ggplot(data = mydata)+geom_line(aes(x=DateTimeT,y=Sensor1_Temperature),colour= "orangered")+
            geom_line(aes(x=DateTimeT,y=Sensor2_Temperature),colour= "gold")
        humi <- ggplot(data = mydata)+geom_line(aes(x=DateTimeH,y=Sensor1_Humidity),colour= "darkolivegreen4")+
            geom_line(aes(x=DateTimeH,y=Sensor2_Humidity),colour= "dodgerblue2")
        grid.arrange(no,co,temp,humi, nrow=4)    # how to add padding to each plot?
    }
    if (ncol(mydata)==21){
        no <- ggplot(data = mydata)+geom_line(aes(x=DateTimeN,y=Sensor1_NO),colour= "gold")+
            geom_line(aes(x=DateTimeN,y=Sensor2_NO),colour= "dodgerblue2")+ 
            geom_line(aes(x=DateTimeN,y=Sensor3_NO),colour= "orangered")+ 
            geom_hline(aes(yintercept=0.1064), colour= "tan4" , linetype= 5)
        co <- ggplot(data = mydata)+geom_line(aes(x=DateTimeC,y=Sensor1_CO),colour= "dodgerblue2")+
            geom_line(aes(x=DateTimeC,y=Sensor2_CO),colour= "orangered")+
            geom_line(aes(x=DateTimeC,y=Sensor3_CO),colour= "darkolivegreen4")+
            geom_hline(aes(yintercept=8.73), colour= "tan4", linetype= 5)
        temp <- ggplot(data = mydata)+geom_line(aes(x=DateTimeT,y=Sensor1_Temperature),colour= "orangered")+
            geom_line(aes(x=DateTimeT,y=Sensor2_Temperature),colour= "darkolivegreen4")+
            geom_line(aes(x=DateTimeT,y=Sensor3_Temperature),colour= "gold")
        humi <- ggplot(data = mydata)+geom_line(aes(x=DateTimeH,y=Sensor1_Humidity),colour= "darkolivegreen4")+
            geom_line(aes(x=DateTimeH,y=Sensor2_Humidity),colour= "gold")+
            geom_line(aes(x=DateTimeH,y=Sensor3_Humidity),colour= "dodgerblue2")
        grid.arrange(no,co,temp,humi, nrow=4)    # how to add padding to each plot?
    }
    
}





