## Code for Session 2
## ------------------------------------------------------------------------------------------------------------------------------ ##

## Load useful libraries
library(tidyverse)
library(aniMotum)
library(sf)
library(mapview)
mapviewOptions(fgb = F) ## add this to remove the plotting issue!!!
library(leaflet)


## Processing and visualising satellite tag data using `aniMotum`

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Step 1: Input and format data

raw_data <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%202/Thaa_Whalesharks_ECOCEAN.csv')

head(raw_data)

## Now lets format the data using transmute() so aniMotum can read it properly
tagdat <-
  raw_data %>% 
  transmute(id = DeployID,
            date = Loc.DateTime,
            lc = Loc.Class,
            lon = Longitude,
            lat = Latitude)

head(tagdat)

## Lets look at what proportion of our positional data are within each location class
tagdat %>% 
  group_by(id, lc) %>% 
  summarise(num_pos = n()) %>% 
  ggplot(aes(x = id, y = num_pos, fill = lc)) +
  geom_col(position = "fill") +
  labs(x = "Tag ID", y = "Proportion of fixes", fill = "Location\nClass") +
  theme_bw()

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Step 2: Choose and fit a movement model

## Lets now now fit a Continuous-time Move Persistence (MP) model to our data

fit <-
  fit_ssm(x = tagdat, 
          vmax = 1.5, ## maximum speed of whale sharks (in m/s)
          model = "mp", ## Move persistence model
          time.step = 24) ## predict positions every 24 hours

## Lets have a look at the fitted component of the model 
## (original data, corrected by including positional error)

plot(fit, 
     what = "fitted", ## what component of the model to plot ('fitted', 'predicted' or 'rerouted')
     type = 2, ## type of plot to make
     pages = 1, 
     ncol = 2)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Step 3: Check model fit

## Lets check the model fit, and then see what the model predictions look like

resid <- osar(fit)

## Lets check our model fit for both tracks
plot(resid, type = "qq")
plot(resid, type = "acf")
plot(resid, type = "ts")

plot(fit, 
     what = "predicted", 
     type = 2,
     pages = 1,
     ncol = 2)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Step 4: Visualise movement model estimates

plot(fit, 
     what = "predicted", 
     type = 3,
     pages = 1,
     ncol = 2,
     normalise = TRUE)

plot(fit, 
     what = "predicted", 
     type = 4,
     pages = 1,
     ncol = 2,
     normalise = TRUE)


## Lets plot our own version of the predicted component using mapview
data <- grab(fit, 
             what = "predicted", 
             as_sf = TRUE,
             normalise = TRUE)

## Lets convert the point dataset into a path using the `sf` package
path <- 
  data %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

## Now lets plot a nice interactive plot of the move persistence data using `mapview` and 'leaflet'

color_palette <- colorRampPalette(hcl.colors(10, palette = "Reds 3"))

m_130 <-
  mapview(path %>% filter(id %in% "M-130"), alpha = 1, color = "white", homebutton = F, 
          legend = F, map.type = c("Esri.WorldImagery"), layer.name = "M-130") +
  mapview(data %>% filter(id %in% "M-130"), alpha.regions = 1, alpha = 0, zcol = "g",
          homebutton = F, legend = F, cex = 3, col.regions = color_palette(183), layer.name = "M-130") +
  mapview(data %>% filter(id %in% "M-130") %>% slice(1), alpha.regions = 1, alpha = 0,
          col.regions = "darkgreen", homebutton = F, legend = F, layer.name = "M-130") +
  mapview(data %>% filter(id %in% "M-130") %>% slice(n()), alpha.regions = 1, alpha = 0,
          col.regions = "firebrick", homebutton = F, legend = F, layer.name = "M-130")

m_150 <-
  mapview(path %>% filter(id %in% "M-150"), alpha = 1, color = "white", homebutton = F, 
          legend = F, map.type = c("Esri.WorldImagery"), layer.name = "M-150") +
  mapview(data %>% filter(id %in% "M-150"), alpha.regions = 1, alpha = 0, zcol = "g",
          homebutton = F, legend = F, cex = 3, col.regions = color_palette(100), layer.name = "M-150") +
  mapview(data %>% filter(id %in% "M-150") %>% slice(1), alpha.regions = 1, alpha = 0,
          col.regions = "darkgreen", homebutton = F, legend = F, layer.name = "M-150") +
  mapview(data %>% filter(id %in% "M-150") %>% slice(n()), alpha.regions = 1, alpha = 0,
          col.regions = "firebrick", homebutton = F, legend = F, layer.name = "M-150")

mm <- 
  (m_130 + m_150)@map %>% 
  addLayersControl(
    baseGroups = c("M-130", "M-150"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addLegend(colors = color_palette(11), 
            labels = round(seq(0, 1, by = 0.1), 2),
            title = "g", opacity = 1)

mm


## ------------------------------------------------------------------------------------------------------------------------------ ##
## gganimate
## ------------------------------------------------------------------------------------------------------------------------------ ##
library(gganimate)
library(ggspatial)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## installing the gganimate package if you don't already have it
install.packages("gganimate")
## ------------------------------------------------------------------------------------------------------------------------------ ##

# ESRI satellite imagery
esri_sat <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                   'World_Imagery/MapServer/tile/${z}/${y}/${x}.jpeg')

track_data <- grab(fit, 
                  what = "predicted", 
                  as_sf = FALSE, 
                  normalise = TRUE)
static_plot <-
  ggplot() +
  annotation_map_tile(esri_sat, zoom = 5) +
  geom_spatial_point(data = track_data, aes(x = lon, y = lat, color = g, group = id), crs = 4326) +
  geom_spatial_path(data = track_data, aes(x = lon, y = lat, color = g, group = id), crs = 4326) +
  scale_color_distiller(palette = "Reds") +
  annotation_scale(text_col = "white", line_col = "white", width_hint = 0.15) +
  theme_void() 

static_plot

## ------------------------------------------------------------------------------------------------------------------------------ ##
## change the static plot into an animation by incorporating a transition_*() function
## there are several options for transitions, here we will use a transition_reveal by `date`
anim <-
  static_plot +
  transition_reveal(date)

## animate it
anim_gif <-
  animate(anim, width = 9.5, height = 8.3, units = "in", res = 300, nframes = 100, fps = 20,
        render = gifski_renderer(loop = F))

## you can then save the animation as a .gif or a .mp4 or .mov
save_animation(animation = anim_gif, file = "Whaleshark_animation.gif")


## ------------------------------------------------------------------------------------------------------------------------------ ##
## 3D plotting
## ------------------------------------------------------------------------------------------------------------------------------ ##

## ------------------------------------------------------------------------------------------------------------------------------ ##
## For 3D plots we will need to install the following package (only run the next step if you dont already have it)
install.packages("rayshader")
## more information on this package can be found here https://www.rayshader.com 

## We will also neeed the 'terra' package
install.packages("terra")
## ------------------------------------------------------------------------------------------------------------------------------ ##

## Lets get the movement data
raw_data <- 
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%202/Thaa_Whalesharks_ECOCEAN.csv')

head(raw_data)

## Now lets format the data using transmute() so aniMotum can read it properly
tagdat <-
  raw_data %>% 
  transmute(id = DeployID,
            date = Loc.DateTime,
            lc = Loc.Class,
            lon = Longitude,
            lat = Latitude)

head(tagdat)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Lets add depth data
## I have created some DUMMY data for depth for this example 
## (This is meant to replicate what you would get from a MiniPat depth profile information every hour)[not real data!!]
minipat <- 
  read_csv("https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%202/minipat_data.csv")

head(minipat)

## Lets format the minipat data so we can combine it with the tracking data
depth_data <-
  minipat %>% 
  mutate(date = ymd_hms(paste(Day, Time))) %>% 
  select(id, date, depth = Depth)

# Lets format the minipat data and predict spatial positions for each depth measurement
fit <-
  fit_ssm(x = tagdat, 
          vmax = 1.5, ## maximum speed of whale sharks (in m/s)
          model = "crw", ## random walk model
          time.step = depth_data %>% select(id, date)) ## predict positions for each minipat depth record

## ------------------------------------------------------------------------------------------------------------------------------ ##
## lets extract the predited positions and join it with the depth data
plot_data <-
  grab(fit, 
       what = "predicted", 
       as_sf = FALSE) %>% 
    left_join(depth_data) %>% 
  mutate(depth = -depth) ## make depths a negative value

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Lets get some bathymetry data for our region using the `terra` package
library(terra)

bath <-
  read_csv('https://raw.githubusercontent.com/vinayudyawer/SEA-workshop2023/main/data/Session%202/bath.csv')

bathy <- rast(bath, type = "xyz")

plot(bathy)

## ------------------------------------------------------------------------------------------------------------------------------ ##
## Now lets use rayshader to plot the tracking data with the bathymetry
library(rayshader)

## reconfigure bathymetry data for 3D plotting
bath_mat <- raster_to_matrix(bathy)

## Set depth exaggeration (this can be adjusted based on how the 3D plot looks)
depth_exaggeration <- 300

bath_mat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ambient_shade(bath_mat, zscale = depth_exaggeration), max_darken = 0.1, rescale_original = T) %>%
  plot_3d(
    bath_mat,
    baseshape = "rectangle",
    water = T,                 ## render water
    zscale = depth_exaggeration,
    wateralpha = 0.2,
    waterlinecolor = "white",
    waterlinealpha = 0.2,
    windowsize = c(1200, 700),  ## Size of window
    theta = 80, 
    phi = 20,
    fov = 60,
    zoom = 0.8
  )

# Add tracks as a path
render_path(extent = ext(bathy), 
            lat = plot_data %>% filter(id %in% "M-130") %>% pull(lat), 
            long = plot_data %>% filter(id %in% "M-130") %>% pull(lon), 
            altitude = plot_data %>% filter(id %in% "M-130") %>% pull(depth),
            zscale = depth_exaggeration, color = "darkgreen")

render_path(extent = ext(bathy), 
            lat = plot_data %>% filter(id %in% "M-150") %>% pull(lat), 
            long = plot_data %>% filter(id %in% "M-150") %>% pull(lon), 
            altitude = plot_data %>% filter(id %in% "M-150") %>% pull(depth),
            zscale = depth_exaggeration, color = "firebrick")

## We can now output these in various formats

## keep rgl window open, you can also adjust the plot now if you need to change viewpoint
render_snapshot(dir = "", filename = "3Dsnapshot.png")

## simple orbiting moving of 3D plot
render_movie(dir = "", filename = "3Dvideo.mp4")

## ------------------------------------------------------------------------------------------------------------------------------ ##
## End of Session 2
## ------------------------------------------------------------------------------------------------------------------------------ ##

