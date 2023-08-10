#### Function to calculate Short-term centers of activity positions from passive telemetry data
## Last Edited: 2018-02-16 (VU)
##
## Based on technique described in: 
## 
## Simpfendorfer, C. A., M. R. Heupel, and R. E. Hueter. 2002. 
## Estimation of short-term centers of activity from an array of omnidirectional 
## hydrophones and its use in studying animal movements. 
## Canadian Journal of Fisheries and Aquatic Sciences 59:23-32.

COA<-function(tagdata, id , timestep, ...){
  ####################################################################################
  ## data         data frame containing passive telemetry dataset; VEMCO field naming accepted 
  ## id           field in 'data' with unique individual tag/animal identifier
  ## timestep     temporal bin size of center of activity calculations (in minutes)
  ####################################################################################
  
  sapply(c("lubridate","plyr"), require, character.only=TRUE)
  
  ## Format date time
  data<-as.data.frame(tagdata)
  data$dt<-ymd_hms(data[,grep("Date",colnames(data))])
  data[,id]<-droplevels(as.factor(data[,id]))
  
  ## Convert timestep from minutes to seconds
  step_sec<-timestep*60 
  
  ## Setup temporal bins based on timesteps
  ex<-seq(from=trunc(min(data$dt, na.rm=TRUE), "day"), to=trunc(max(data$dt, na.rm=TRUE), "day")+86400, by=step_sec)
  data$DateTime<-cut(data$dt, breaks=ex)

  ## Calculate short term center of activity positions (3D if depth data available) 
  cenac<-ddply(data, c("DateTime",id), summarize,
               Transmitter=Transmitter[1], Transmitter.Name=Transmitter.Name[1],
               Transmitter.Serial=Transmitter.Serial[1], Sensor.Value.coa=mean(Sensor.Value),Sensor.Unit=Sensor.Unit[1],
               Latitude.coa=mean(Latitude, na.rm=T), Longitude.coa=mean(Longitude, na.rm=T), ...)
  cenac<-cenac[!is.na(cenac$Latitude.coa),]
  if(length(levels(cenac[,id]))>1){cenac<-dlply(cenac, id)}
  return(cenac)
}

