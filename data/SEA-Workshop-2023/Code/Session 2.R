## Code for Session 2
## ------------------------------------------------------------------------------------------------------------------------------ ##

## Load useful libraries
library(tidyverse)
library(aniMotum)
library(sf)
library(mapview)
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
pred_data <- grab(fit, 
                  what = "predicted", 
                  as_sf = TRUE, 
                  normalise = TRUE)

## Lets convert the point dataset into a path using the `sf` package
pred_path <- 
  pred_data %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

## Now lets plot a nice interactive plot of the move persistence data using `mapview` and 'leaflet'

color_palette <- colorRampPalette(hcl.colors(10, palette = "Reds 3"))

m_130 <-
  mapview(pred_path %>% filter(id %in% "M-130"), alpha = 1, color = "white", homebutton = F, 
          legend = F, map.type = c("Esri.WorldImagery"), layer.name = "M-130") +
  mapview(pred_data %>% filter(id %in% "M-130"), alpha.regions = 1, alpha = 0, zcol = "g",
          homebutton = F, legend = F, cex = 3, layer.name = "M-130", col.regions = color_palette(100)) +
  mapview(pred_data %>% filter(id %in% "M-130") %>% slice(1), alpha.regions = 1, alpha = 0,
          col.regions = "darkgreen", homebutton = F, legend = F, layer.name = "M-130") +
  mapview(pred_data %>% filter(id %in% "M-130") %>% slice(n()), alpha.regions = 1, alpha = 0,
          col.regions = "firebrick", homebutton = F, legend = F, layer.name = "M-130")

m_150 <-
  mapview(pred_path %>% filter(id %in% "M-150"), alpha = 1, color = "white", homebutton = F, 
          legend = F, map.type = c("Esri.WorldImagery"), layer.name = "M-150") +
  mapview(pred_data %>% filter(id %in% "M-150"), alpha.regions = 1, alpha = 0, zcol = "g",
          homebutton = F, legend = F, cex = 3, layer.name = "M-150", col.regions = color_palette(100)) +
  mapview(pred_data %>% filter(id %in% "M-150") %>% slice(1), alpha.regions = 1, alpha = 0,
          col.regions = "darkgreen", homebutton = F, legend = F, layer.name = "M-150") +
  mapview(pred_data %>% filter(id %in% "M-150") %>% slice(n()), alpha.regions = 1, alpha = 0,
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
## End of Session 2
## ------------------------------------------------------------------------------------------------------------------------------ ##

