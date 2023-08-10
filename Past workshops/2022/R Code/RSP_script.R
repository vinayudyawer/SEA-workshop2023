library(tidyverse)
library(actel)
library(RSP)
library(sf)
library(raster)
library(ozmaps)
library(patchwork)
library(geosphere)
library(cmocean)

browseVignettes("RSP") # Check RSP vignettes

#--------------------#
# Estuarine analyses #
#--------------------#

#------------------------------------#
# 1. Filtering detections with actel #
#------------------------------------#
setwd("data/Kalang-Bellinger")
exp.results <- explore(tz = 'Australia/Sydney', report = FALSE, GUI = 'never')
n
n


#-------------------------------------#
# 2. Refining in-water paths with RSP #
#-------------------------------------#

# Load land shapefile
water <- loadShape(path = "shapefile/", shape = "Kalang-Bellinger.shp", 
	size = 0.0001, # Pixel size for the rendered raster (shapefile units)
	buffer = 0.01) # Water area buffer surrounding the shapefile
plot(water) # Plot with raster package

# Check if receivers are inside the water
plotRaster(input = exp.results, base.raster = water, 
  coord.x = "Longitude", coord.y = "Latitude") 

# Create a transition layer with 8 directions
tl <- transitionLayer(x = water, directions = 8)
# save(tl, file = "tl.RData") # Export transition layer 

# Create in-water tracks
rsp.run <- runRSP(input = exp.results, t.layer = tl, verbose = TRUE,
  coord.x = "Longitude", coord.y = "Latitude", 
  er.ad = 2, # Location error increment (metres)
  max.time = 24) # Temporal interval separating new tracks (24 hours = default)
names(rsp.run) # runRSP outputs

# Check track metadata
head(rsp.run$tracks)
rsp.run$tracks$'A69-9001-18784' # Individual tracks

# Plot a track with RSP
plotTracks(input = rsp.run, base.raster = water, 
	tag = "A69-9001-18784", track = 10) + # Select tag and track of interest
	addStations(rsp.run) # add receiver locations

# Check RSP track data
names(rsp.run$detections) # Output saved separated for each tag
head(rsp.run$detections$'A69-9001-18784', 20)

# Plot all individual tracks (sf package)
shp <- st_read("shapefile/Kalang-Bellinger.shp") # Load study area shapefile
detecs <- rsp.run$detections$'A69-9001-18784' # Extract shark RSP tracks
detecs$Year_Month <- substr(detecs$Timestamp, 1, 7) # New time variable
head(detecs)
ggplot() + theme_bw() +
	geom_sf(data = shp, fill = 'brown', alpha = 0.3, size = 0.07, colour = "black") +
	geom_point(data = detecs, aes(x = Longitude, y = Latitude, colour = Year_Month), size = 0.7) +
	geom_path(data = detecs, aes(x = Longitude, y = Latitude, colour = Year_Month), size = 0.3) +
	coord_sf(xlim = c(152.99, 153.05), ylim = c(-30.51, -30.47), expand = FALSE)

# Calculate distances to river mouth:
mouth <- c(153.031006, -30.501217) # River mouth location
rsp.tracks <- do.call(rbind.data.frame, rsp.run$detections) # Extract all shark RSP tracks
rsp.tracks$Distance.mouth <- as.numeric(distm(x = mouth, # Calculate distances
	y = rsp.tracks[,16:17])) / 1000 # Distances in km

# Plot distances to river mouth throughout the tracking period:
ggplot() + theme_bw() +
	geom_path(data = rsp.tracks, aes(x = Date, y = Distance.mouth, colour = Track)) +
	theme(legend.position = "bottom") +
	guides(colour = guide_legend(ncol = 10, nrow = 6)) +
	labs(y = "Distance to river mouth (km)") +
	facet_wrap(~Signal, nrow = 5, ncol = 1) 


#----------------------------------------------------#
# 3. dynamic Brownian Bridge Movement Models (dBBMM) #
#----------------------------------------------------#

# Calculate dBBMM model: 
# Warning: takes around 3 min to run
dbbmm.run <- dynBBMM(input = rsp.run, base.raster = water, UTM = 56, # Provide UTM of study area
	start.time = "2020-02-01 00:00:00", stop.time = "2020-02-15 00:00:00") # Select a subset of the tracking period
# save(dbbmm.run, file = "dBBMM1.RData") 
# load("dBBMM1.RData")

# Plot dBBMM models with RSP
dbbmm.run$valid.tracks # Valid tracks metadata
plotContours(input = dbbmm.run, tag = "A69-9001-18767", track = 16)
plotContours(input = dbbmm.run, tag = "A69-9001-14230", track = 27) +
	addStations(rsp.run) # add receiver locations

#############################################
# Bonus 1: custom plot (dBBMM + RSP tracks) #
#############################################

# Raw dBBMM raster file
dbbmm.run$group.rasters$F$"A69.9001.18784_Track_40" # Raw dBBMM raster file
plot(dbbmm.run$group.rasters$F$"A69.9001.18784_Track_40") # Plot with raster package

# Reproject raw raster and transform to dataframe (ggplot2)
projected_raster <- projectRaster(dbbmm.run$group.rasters$F$"A69.9001.18784_Track_40", 
	crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") # CRS of interest
plot(projected_raster) # Check coordinates are reprojected
df.raster <- as.data.frame(projected_raster, xy = TRUE) # Convert raster to data.frame (ggplot2)
names(df.raster)[3] <- "values"
head(df.raster)
df.raster <- df.raster[-which(is.na(df.raster$values) == TRUE), ] # Remove empty values (land)
df.raster <- df.raster[which(df.raster$values <= 0.95), ] # Select only <95% levels 
summary(df.raster)

# Select RSP track for that tag and track of interest
df.track <- subset(rsp.run$detections$"A69-9001-18784", 
	Track == "Track_40")
head(df.track) # Check track data
head(rsp.run$spatial$stations) # Receiver locations info

# Plot map
ggplot() + theme_bw() +
	geom_sf(data = shp, fill = 'forestgreen', alpha = 0.2, size = 0.07, colour = "black") +
  geom_tile(data = df.raster, aes(x = x, y = y, fill = values)) +
  scale_fill_gradientn(colours = rev(cmocean('matter')(100))) +
	coord_sf(xlim = c(152.99, 153.05), ylim = c(-30.51, -30.47), expand = FALSE) +
	geom_path(data = df.track, aes(x = Longitude, y = Latitude), size = 0.2, colour = "darkgray") +
	geom_point(data = df.track, aes(x = Longitude, y = Latitude), pch = 21, fill = "black", colour = "darkgray", size = 1.3, stroke = 0.2) +
	geom_point(data = rsp.run$spatial$stations, aes(x = Longitude, y = Latitude), pch = 21, fill = "red", colour = "black", size = 1.5, stroke = 0.2) +
	labs(x = "", y = "", fill = "dBBMM (%)",
		title = "A69-9001-18784: 01 Feb - 15 Feb")


#--------------------------------------#
# 4. Calculate size of space-use areas #
#--------------------------------------#

# Run dBBMM with daily resolutions
# Warning: takes around 8 min to run
dbbmm.time <- dynBBMM(input = rsp.run, base.raster = water, UTM = 56, 
	timeframe = 24, # Temporal interval of interest (in hours) = timeslots
	start.time = "2020-02-01 00:00:00", stop.time = "2020-02-15 00:00:00") 
# save(dbbmm.time, file = "dBBMM2.RData") 
# load("dBBMM2.RData")
head(dbbmm.time$timeslots) # Timeslot metadata

# Calculate size of space-use areas in squared metres
areas.group <- getAreas(input = dbbmm.time, breaks = c(0.5, 0.95), # 50% and 95% contours (default)
	type = "group") # for individual areas use type = "track" 
areas.group$areas$F
areas.group$areas$M

##################################################
# Bonus 2: exporting area contours as shapefiles #
##################################################

## Extract raw rater objects for timeslot 9
names(areas.group)

# Females 50% contour
areas.group$rasters$F$"9"$"0.5" # Raster of interest
projected_raster <- projectRaster(areas.group$rasters$F$"9"$"0.5", # Reproject
	crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
plot(projected_raster) # Check raster is reprojected
polygon <- rasterToPolygons(projected_raster, fun=function(x){x > 0},
	dissolve = TRUE, na.rm = FALSE) # Select only positive pixels
plot(polygon) # Not all raster cells are dissolved (post processing in QGIS!)
shapefile(polygon, "shapefile/Female_50.shp") # Export polygon to shapefile

# Females 95% contour
areas.group$rasters$F$"9"$"0.95" # Raster of interest
projected_raster <- projectRaster(areas.group$rasters$F$"9"$"0.95", # Reproject
	crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
plot(projected_raster) # Check raster
polygon <- rasterToPolygons(projected_raster, fun=function(x){x > 0},
	dissolve = TRUE, na.rm = FALSE) # Select only positive pixels
plot(polygon) # Not all cells are dissolved (post processing in QGIS)
shapefile(polygon, "shapefile/Female_95.shp") # Export polygon to shapefile

# Plot shapefiles externally using QGIS

# Back to RSP...

# Temporal variation in space-use
head(areas.group$areas$F) # Space-use size areas (m2)
areas.group$areas$F$Date <- dbbmm.time$timeslots$start[match(areas.group$areas$F$Slot, 
	as.character(dbbmm.time$timeslots$slot))] # Match Female timeslots to date variable
areas.group$areas$F$Group <- "F" # Add group information (Females)
areas.group$areas$M$Date <- dbbmm.time$timeslots$start[match(areas.group$areas$M$Slot, 
	as.character(dbbmm.time$timeslots$slot))] # Match Male timeslots to date variable
areas.group$areas$M$Group <- "M" # Add group information (Male)
plot.areas <- rbind(areas.group$areas$F, areas.group$areas$M)
plot.areas
ggplot() + theme_bw() +
	geom_line(data = plot.areas, aes(x = Date, y = area.95 / 1000, colour = Group)) +
	labs(y = expression(paste('95% contour area (',km^2,')')))

# Calculate overlaps between groups
overlap.save <- getOverlaps(input = areas.group)
names(overlap.save) # Overlaping area info + raw rasters
names(overlap.save$areas) # List by dBBMM contour
names(overlap.save$areas$'0.95') # Values in m2 and %
names(overlap.save$areas$'0.95'$absolutes) # List by timeslot
overlap.save$areas$'0.95'$absolutes[9] # m2
overlap.save$areas$'0.95'$percentage[9] # %
plotOverlaps(overlaps = overlap.save, areas = areas.group, base.raster = water, 
  groups = c("M", "F"), timeslot = 9, level = 0.95) 


#-------------------------------------------#
# 5. Analyse trends in space-use with steps #
#-------------------------------------------#

# Warning: takes a couple of hours to run. Output is loaded on line 232.
getAreaStep(input = rsp.run, base.raster = water, UTM = 56,
	timeframe = 1, save = TRUE, 
	start.time = "2020-02-01",
	name.new = "dBBMM_data_new.csv", groups = c("M", "F"))

# Load results
df.step <- read.csv("dBBMM_data.csv")
head(df.step)
df.step$Start.time <- as.POSIXct(df.step$Start.time, format = "%Y-%m-%d %H:%M:%S", 
	tz = "Australia/Sydney")

# Convert long format for plotting
df.step.plot <- df.step %>%
	gather(Group, N,  "M_n", "F_n", factor_key=TRUE) %>%
	gather(Area.contour, Size, 
		"Area.M.50", "Area.M.95", 
		"Area.F.50", "Area.F.95",
		"Overlap.50.tot", "Overlap.95.tot",
		"Overlap.50.freq", "Overlap.50.freq")
head(df.step.plot)

# Plot space-use area variation through time
aux.plot <- subset(df.step.plot, Area.contour %in% c("Area.M.50", "Area.F.50", "Overlap.50.tot"))
aux.plot$Area.contour[aux.plot$Area.contour == "Area.M.50"] <- "Male 50%" # Rename for plotting
aux.plot$Area.contour[aux.plot$Area.contour == "Area.F.50"] <- "Female 50%" # Rename for plotting
aux.plot$Area.contour[aux.plot$Area.contour == "Overlap.50.tot"] <- "Overlap 50%" # Rename for plotting
ggplot() + theme_classic() +
	geom_line(data = aux.plot, 
		aes(x = Start.time, y = Size / 1000, 
			colour = Area.contour)) +
	labs(y = expression(paste('Area (',km^2,')')), x = "Date", colour = "Level")


#------------------------------------------------------------------------------------------------

#------------------#
# Coastal analyses #
#------------------#

rm(list = ls()) # Remove all estuarine files
gc() # run garbage collection = improve memory use

#------------------------------------#
# 1. Filtering detections with actel #
#------------------------------------#
setwd("../Coastal")
exp.results <- explore(tz = 'Australia/Sydney', report = TRUE, # Check out actel's report
	GUI = 'never')
n
n
n
n


#-------------------------------------#
# 2. Refining in-water paths with RSP #
#-------------------------------------#

# Load land shapefile
# Warning: takes a couple of minutes to run
water <- loadShape(path = "shapefile/", shape = "Australia_WGS.shp", size = 0.01, buffer = 0.05)
# save(water, file = "water_bad.RData") 
# load("water_bad.RData")

# Check if receivers are inside the water: one receiver is on land! Show how to fix in QGIS
plotRaster(input = exp.results, base.raster = water, 
  coord.x = "Longitude", coord.y = "Latitude") 

# Load fixed shapefile
# Warning: takes a couple of minutes to run
water <- loadShape(path = "shapefile/", shape = "Australia_WGS_fixed.shp", size = 0.01, buffer = 0.05)
# save(water, file = "water_good.RData") 
# load("water_good.RData")
plotRaster(input = exp.results, base.raster = water, 
  coord.x = "Longitude", coord.y = "Latitude") # Check all stations are inside the water

# Create a transition layer: 16 is usually better for coastal areas
# Warning: takes a couple of minutes to run. Output is loaded bellow
tl <- transitionLayer(x = water, directions = 16)

# Create in-water tracks:
# Warning: takes a couple of minutes to run
rsp.coast <- runRSP(input = exp.results, t.layer = tl, verbose = TRUE,
  coord.x = "Longitude", coord.y = "Latitude", 
  distance = 10000, # Add RSP locations every 10 km
  max.time = 50000) # Make it very big to get a single track for entire tracking
# save(rsp.coast, file = "rsp_coast.RData") 
# load("rsp_coast.RData")

# Extract total tracking dataset
rsp.tracks <- do.call(rbind.data.frame, rsp.coast$detections) # Extract all shark tracks
rsp.tracks$Year_Month <- as.numeric(paste( # Add new numeric time variable
	substr(rsp.tracks$Timestamp, 1, 4), substr(rsp.tracks$Timestamp, 6, 7), sep = "."))
head(rsp.tracks)

# Plot individual tracks
oz_states <- ozmap_states # Load Aus state shapefile 
shp <- st_read("shapefile/Australia_WGS_fixed.shp") 
# 14230
ggplot() + theme_bw() +
	annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'dodgerblue', alpha = 0.3) +
	geom_sf(data = shp, fill = 'lightgray', size = 0.07, colour = "black") +
	geom_sf(data = oz_states, fill = NA, colour = "darkgray", lwd = 0.2) + 
	annotate("text", x = 150, y = -30.501191, label = "Kalang-Bellinger", size = 3) +
	geom_path(data = subset(rsp.tracks, Signal == 14230), # Select animal of interest
		aes(x = Longitude, y = Latitude, colour = Year_Month), size = 1) +
	scale_colour_gradientn(colours = cmocean("thermal")(100), breaks = c(2019, 2020, 2021)) +
	coord_sf(xlim = c(141, 155), ylim = c(-35, -8), expand = FALSE) +
	labs(x = "", y = "", colour = "Year", title = "14230 - Female")
# 14270
ggplot() + theme_bw() +
	annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'dodgerblue', alpha = 0.3) +
	geom_sf(data = shp, fill = 'lightgray', size = 0.07, colour = "black") +
	geom_sf(data = oz_states, fill = NA, colour = "darkgray", lwd = 0.2) + 
	annotate("text", x = 150, y = -30.501191, label = "Kalang-Bellinger", size = 3) +
	geom_path(data = subset(rsp.tracks, Signal == 14270), # Select animal of interest
		aes(x = Longitude, y = Latitude, colour = Year_Month), size = 1) +
	scale_colour_gradientn(colours = cmocean("thermal")(100), breaks = c(2019, 2020, 2021)) +
	coord_sf(xlim = c(141, 155), ylim = c(-35, -8), expand = FALSE) +
	labs(x = "", y = "", colour = "Year", title = "14270 - Female") 
# 18784
ggplot() + theme_bw() +
	annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'dodgerblue', alpha = 0.3) +
	geom_sf(data = shp, fill = 'lightgray', size = 0.07, colour = "black") +
	geom_sf(data = oz_states, fill = NA, colour = "darkgray", lwd = 0.2) + 
	annotate("text", x = 150, y = -30.501191, label = "Kalang-Bellinger", size = 3) +
	geom_path(data = subset(rsp.tracks, Signal == 18784), # Select animal of interest
		aes(x = Longitude, y = Latitude, colour = Year_Month), size = 1) +
	scale_colour_gradientn(colours = cmocean("thermal")(100), breaks = c(2019, 2020, 2021)) +
	coord_sf(xlim = c(141, 155), ylim = c(-35, -8), expand = FALSE) +
	labs(x = "", y = "", colour = "Year", title = "18784 - Female") 
# 18831
ggplot() + theme_bw() +
	annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'dodgerblue', alpha = 0.3) +
	geom_sf(data = shp, fill = 'lightgray', size = 0.07, colour = "black") +
	geom_sf(data = oz_states, fill = NA, colour = "darkgray", lwd = 0.2) + 
	annotate("text", x = 150, y = -30.501191, label = "Kalang-Bellinger", size = 3) +
	geom_path(data = subset(rsp.tracks, Signal == 18831), # Select animal of interest
		aes(x = Longitude, y = Latitude, colour = Year_Month), size = 1) +
	scale_colour_gradientn(colours = cmocean("thermal")(100), breaks = c(2019, 2020, 2021)) +
	coord_sf(xlim = c(141, 155), ylim = c(-35, -8), expand = FALSE) +
	labs(x = "", y = "", colour = "Year", title = "18831 - Female") 
# 18767
ggplot() + theme_bw() +
	annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = 'dodgerblue', alpha = 0.3) +
	geom_sf(data = shp, fill = 'lightgray', size = 0.07, colour = "black") +
	geom_sf(data = oz_states, fill = NA, colour = "darkgray", lwd = 0.2) + 
	annotate("text", x = 150, y = -30.501191, label = "Kalang-Bellinger", size = 3) +
	geom_path(data = subset(rsp.tracks, Signal == 18767), # Select animal of interest
		aes(x = Longitude, y = Latitude, colour = Year_Month), size = 1) +
	scale_colour_gradientn(colours = cmocean("thermal")(100), breaks = c(2019, 2020, 2021)) +
	coord_sf(xlim = c(141, 155), ylim = c(-35, -8), expand = FALSE) +
	labs(x = "", y = "", colour = "Year", title = "18767 - Male") 
	
# Calculate distances travelled by each shark
df.dist <- getDistances(input = rsp.coast)
df.dist # Both Receiver and RSP 

# Plot total distances travelled by each shark and group
plot1 <- plotDistances(input = df.dist, group = "F") 
plot2 <- plotDistances(input = df.dist, group = "M") 
(plot1 / plot2) + plot_layout(design = c( 
	area(t = 1, l = 1, b = 4, r = 1), # Controls size of plot1
  area(t = 5, l = 1, b = 5.5, r = 1)), # Controls size of plot2
guides = "collect") # Single legend

# Summary of tracking time (number of days)
rsp.tracks.sum <- rsp.tracks %>%
	group_by(Transmitter) %>%
	summarise(Track.time = as.numeric(difftime(time1 = max(Timestamp), time2 = min(Timestamp), units = "days")))
rsp.tracks.sum
	# Add to tracking time to distance dataset
	df.dist$Time <- rsp.tracks.sum$Track.time[match(df.dist$Animal.tracked, rsp.tracks.sum$Transmitter)]
	df.dist

# Custom plot of distances travelled and tracking times
ggplot(data = subset(df.dist, Loc.type == "RSP")) + theme_bw() +
	geom_col(aes(x = Dist.travel / 1000, y = Animal.tracked, fill = Group)) +
	geom_text(aes(x = (Dist.travel / 1000) + 400, y = Animal.tracked, label = paste(round(Time, 1), "days"))) +
	scale_x_continuous(limits = c(0, 5000)) +
	labs(x = "Distance travelled (km)", y = "", fill = "Sex")

	