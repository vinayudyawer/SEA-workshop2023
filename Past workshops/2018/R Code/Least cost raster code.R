library(VTrack)
barra <- read.csv('C:/Users/uqrdwye2/Downloads/barra.csv', header = TRUE)
barra[,1] <- as.POSIXct(barra[,1])
barra <- subset(barra, barra[,1] < as.POSIXct("2016-01-31 14:00:00"))
unique(barra$Station.Name)

barra <- subset(barra,barra$Station.Name!="WENMTH2M")
barra <- subset(barra,barra$Station.Name!="PM2")

barra$Station.Name <- factor(barra$Station.Name)
unique(barra$Station.Name)

Vbarra <- ReadInputData(infile = barra,
                        iHoursToAdd = 10,
                        sVemcoFormat = "Default")

POINTS <- read.csv("GIS/All wenlock stations snapped to river UTM.csv")
POINTS2 <- POINTS[-c(4,10,11,12,13,17:26,40:45,58,62:63,71,74:77,99,103,107),] # Removes points that dont fall on raster
POINTS3 <- POINTS2[POINTS2[, 1] %in%  ExtractUniqueValues(Vbarra,6), ]  # Removes points that were not accessed by our fish
POINTS3$LOCATION <- factor(POINTS3$LOCATION)
levels(POINTS3$LOCATION)
row.names(POINTS3) <- NULL

### Rename csiro vr2Ws
levels(barra$Station.Name)[7] <- "TentpoleTwin" 
levels(barra$Station.Name)[8] <- "Wenmouth" 
levels(barra$Station.Name)[9] <- "WenTentHud" 
levels(barra$Station.Name)[10] <- "WenTentDS" 

### Rename csiro vr2Ws
levels(POINTS3$LOCATION)[7] <- "TentpoleTwin" 
levels(POINTS3$LOCATION)[8] <- "Wenmouth" 
levels(POINTS3$LOCATION)[9] <- "WenTentHud" 
levels(POINTS3$LOCATION)[10] <- "WenTentDS" 

write.csv(POINTS3,"Data/PointsLeastCost_crocs.csv",row.names=FALSE)
write.csv(barra,"Data/barra.csv",row.names=FALSE)


library(sp)
#library(maptools)
library(raster)
#library(rgeos)
library(rgdal)
#library(biomod2)
library(gdistance)


r5 <- raster("GIS/wenlock raster UTM.tif")
# Convert to transition object - required for the river distance algorythm
tr <- transition(r5, transitionFunction=mean, directions=8) # Create a Transition object from the raster


#convert the raster to points for plotting
map.p <- rasterToPoints(r5)
df <- data.frame(map.p) #Make the points a dataframe for ggplot
colnames(df) <- c("X", "Y", "Water") #Make appropriate column headings

#Now plot the map
ggplot(data=df, aes(y=Y, x=X)) +
  geom_raster(aes(fill=Water)) +
  geom_point(data=POINTS3, 
             aes(x=X, y=Y), color="white", size=3, shape=16) +
  theme_bw() +
  coord_equal() 



# Function to calculate river distances from multi branch system
GenerateRasterDistance <- function(sPointsFile,sTransition)
{
  
  xy <- coordinates(sPointsFile[,2:3])
  SP <- SpatialPoints(xy,proj4string=crs(sTransition))
  
  # Initialise variables
  newDM <- matrix(0,nrow(xy),nrow(xy))
  iCount <- 1
  rDISTANCE <- rep(0,nrow(xy)-1)
  sDISTANCE <- rep(0,nrow(xy)-1)
  
  for(i in 1:nrow(xy))     # For each row in the distance matrix
  {
    for (j in 1:nrow(xy))  # For each column in the distance matrix
    {
      SP1 <- SP[i,]
      SP2 <- SP[j,]
      
      crowdist <- spDists(SP1,SP2, longlat = FALSE) # Extract the direct distance
      
      if(crowdist > 200){
        sPath2 <- shortestPath(sTransition, SP1, SP2, output="SpatialLines")
        riverdist <- SpatialLinesLengths(sPath2,longlat=FALSE)}
      if(crowdist <= 200)
        riverdist <- crowdist
      
      #sDISTANCE[j] <- crowdist      
      rDISTANCE[j] <- riverdist 
    }
    newDM[i,] <- rDISTANCE
    print(paste0(i," of ",nrow(xy)," receivers calculated"))
  }
  newDM <- round(newDM/1000,2) #divide by 1000 to convert from m to km
  newDM[lower.tri(newDM)] = t(newDM)[lower.tri(newDM)] # Ensures matrix symetrical
  
  # Format and return the distance matrix
  DM <- as.character(sPointsFile$LOCATION)
  colnames(newDM) <- DM
  newDM <- data.frame(newDM)
  DM2 <- data.frame(DM = DM,newDM)
  return(DM2)
}

GenerateRasterDistance(POINTS3,tr)
