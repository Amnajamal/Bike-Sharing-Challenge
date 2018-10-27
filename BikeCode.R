#---
#title: "Bike sharing challenge"
#author: "Amna Jamal"
#date: "January 01, 2017"
#output: pdf_document
#---
  
#  ```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE)
#```
########### Function outputs city for given latitude longitude pair ###########
reverseGeoCode <- function(latlng) {
  #latlng = c(37.329732, -121.901782)
  latlngStr <-  gsub(' ','%20', paste(latlng, collapse=","))#Collapse and Encode URL Parameters
  library("RJSONIO") #Load Library
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&latlng=',latlngStr, sep="")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  #print(data.json["results.formatted_address"])
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK")
    address <- data.json["results.formatted_address"]
  return (address)
}

############## Reading Input Files ##########
setwd('C:\\...')
station_data = read.csv('station_data.csv', stringsAsFactors = FALSE)

###### changes were made in trip_data.csv to do data manipulation. I am unable to upload it on the portal
trip_data    = read.csv('trip_data.csv'   , stringsAsFactors = FALSE)
weather_data = read.csv('weather_data.csv', stringsAsFactors = FALSE)


############# Merging weather data for each station ################
station_data$City = "0"
for(i in (1:76)){
  abc = reverseGeoCode(c(station_data$Lat[i],station_data$Long[i]))
  abc = unlist(strsplit(abc, split=","))
  station_data$City[i] = as.character(abc[2])
}
#station_data$City = sub("^\\s+", "", station_data$City)
weather_data$City = "0"
weather_data$City[which(weather_data$Zip == 94107)] = "San Francisco"
weather_data$City[which(weather_data$Zip == 94063)] = "Redwood City"
weather_data$City[which(weather_data$Zip == 94301)] = "Palo Alto"
weather_data$City[which(weather_data$Zip == 94041)] = "Mountain View"
weather_data$City[which(weather_data$Zip == 95113)] = "San Jose"
weather_data$Date = as.Date(weather_data$Date, format =  "%d/%m/%Y")

agg_weather_data = merge(weather_data,station_data[,c("Id","City")], by = "City")
agg_weather_data$Id[agg_weather_data$Id == 23] = 85
agg_weather_data$Id[agg_weather_data$Id == 25] = 86
agg_weather_data$Id[agg_weather_data$Id == 49] = 87
agg_weather_data$Id[agg_weather_data$Id == 69] = 88
agg_weather_data$Id[agg_weather_data$Id == 72 | agg_weather_data$Id == 89] = 90

# Data exploration
################ Evaluating Net Rate at each station ##################
trip_data$End.Hour   = trip_data$End.Hour + 1
trip_data$End.Hour[trip_data$End.Hour == 24] = 0
trip_data$Start.Date = as.Date(trip_data$Start.Date, format =  "%m/%d/%Y")
trip_data$End.Date   = as.Date(trip_data$End.Date  , format =  "%m/%d/%Y")

TotalHours = difftime(max(trip_data$Start.Date),min(trip_data$Start.Date), units = "hours")

UniqueStns = unique(trip_data$Start.Station)

length(unique(trip_data$Start.Date))

#trip_data1 = trip_data[trip_data$Start.Station == 66,]
IncomingBike = aggregate(trip_data$Start.Station, list(Start.Station = trip_data$Start.Station, Start.Date = trip_data$Start.Date, Start.Hour= trip_data$Start.Hour), length)
IncomingBike = IncomingBike[order(IncomingBike$Start.Date, IncomingBike$Start.Hour),]

OutgoingBike = aggregate(trip_data$End.Station  , list(End.Station = trip_data$End.Station, End.Date   = trip_data$End.Date  , End.Hour= trip_data$End.Hour  ), length)   
OutgoingBike = OutgoingBike[order(OutgoingBike$End.Date, OutgoingBike$End.Hour),]

########### Creating data frame for each station ##############
NetRateBike = merge(IncomingBike, OutgoingBike, by.x=c("Start.Station", "Start.Date", "Start.Hour"),
                    by.y = c("End.Station", "End.Date", "End.Hour")   , all.x = T, all.y = T )
NetRateBike[is.na(NetRateBike)] = 0
NetRateBike$Rate   = NetRateBike$Incoming - NetRateBike$Outgoing 

############## creating an empty data frame with dates and hours of the day #############
EmptyDF            = data.frame(sort(unique(NetRateBike$Date)))
EmptyH             = data.frame(sort(unique(NetRateBike$Hour)))
EmptyDF            = merge(EmptyDF,EmptyH,all.x=T)
names(EmptyDF)     = c("Date","Hour")
names(NetRateBike) = c("Station", "Date", "Hour", "Incoming", "Outgoing")

############ setting old station ids to new ones ##############
NetRateBike$Station[NetRateBike$Station == 23] = 85
NetRateBike$Station[NetRateBike$Station == 25] = 86
NetRateBike$Station[NetRateBike$Station == 49] = 87
NetRateBike$Station[NetRateBike$Station == 69] = 88
NetRateBike$Station[NetRateBike$Station == 72 | NetRateBike$Station == 89] = 90

################ Features used in the analysis ################
Features             = c("Hour", "Rate", "Max.TemperatureF","Mean.TemperatureF",
                         "Min.TemperatureF", "Max.Dew.PointF", "MeanDew.PointF", "Min.DewpointF", "Max.Humidity", "Mean.Humidity", "Min.Humidity",
                         "Max.Sea.Level.PressureIn", "Mean.Sea.Level.PressureIn", "Min.Sea.Level.PressureIn", "Max.VisibilityMiles", "Mean.VisibilityMiles",
                         "Min.VisibilityMiles", "Max.Wind.SpeedMPH", "Mean.Wind.SpeedMPH", "Max.Gust.SpeedMPH", "PrecipitationIn", "CloudCover", "Events",
                         "WindDirDegrees","LagRate3", "LagRate2", "LagRate1") 

################ Function to run, learn and predict for each station ID ##########
PredictFtn <- function(Datatemp)
{
  #print(head(Datatemp))
  #Datatemp = NetRateBike[NetRateBike$Station == 2,]
  TStation = Datatemp$Station[1]
  #TCity    = Datatemp$City[1]
  
  NetRateBikeDF27                         = merge(EmptyDF, Datatemp, by = c("Date","Hour"), all.x = T)
  NetRateBikeDF27$Station                 = TStation
  #NetRateBikeDF27$City                    = TCity
  NetRateBikeDF27[is.na(NetRateBikeDF27)] = 0
  NetRateBikeDF27                         = merge(NetRateBikeDF27, agg_weather_data, by.x = c("Date", "Station"), by.y = c("Date","Id"), all.x = T)
  
  ############# Evaluating Lag values #####################
  NetRateBikeDF27  <-cbind(LagRate1 = lag(NetRateBikeDF27$Rate, -1),NetRateBikeDF27)
  NetRateBikeDF27  <-cbind(LagRate2 = lag(NetRateBikeDF27$Rate, -2),NetRateBikeDF27)
  NetRateBikeDF27  <-cbind(LagRate3 = lag(NetRateBikeDF27$Rate, -3),NetRateBikeDF27)
  
  SampleDF       = NetRateBikeDF27[,Features]
  SampleDF       = na.omit(SampleDF) 
  SampleDF$Events = factor(SampleDF$Events)
  library(randomForest)
  library(caTools)
  split        = sample.split(SampleDF, SplitRatio = 0.8)
  training_set = subset(SampleDF     , split == TRUE   )
  test_set     = subset(SampleDF     , split == FALSE  )
  
  fit  <- randomForest(Rate~., data = training_set)
  pred <-round(as.integer(predict(fit, newdata=test_set)))
  error = abs(test_set$Rate - pred)
  
  rmse <- function(error)
  {
    sqrt(mean(error^2)) 
  }
  RMSE_rpart <- rmse(error)
  #RMSE_rpart
  
  ReturnDF = NULL
  ReturnDF$Station = TStation
  ReturnDF$RMSE    = RMSE_rpart
  return(ReturnDF)
}


df_list      <- by(NetRateBike, NetRateBike[, c("Station")], PredictFtn)
StationError <- data.frame(do.call(rbind, df_list))

hist(as.numeric(StationError$RMSE),xlab = "Error in RMSE", title = "Error Distribution for stations")
write.csv(StationError, "RMSEError.csv")
