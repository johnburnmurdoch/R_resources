install.packages("needs")
library(needs)
needs(tidyverse, magrittr, sf, scales, viridis, gstat)

# Load the data we’ll be using:
# Polygon boundaries for Californian counties...
load("boundaries.Rdata")
# ... and ozone readings from air quality monitoring stations in California
load("points.Rdata")

# Have a quick glimpse at the datasets:
head(CA_boundaries)
head(CA_air_qual)

# Now let’s plot the points on top of the boundaries, just to get an idea of what we’re working with:
ggplot(CA_boundaries) +
  geom_sf(fill="grey95") +
  theme_minimal() +
  geom_point(data = CA_air_qual, aes(x=lon, y=lat, col=ozone)) +
  scale_colour_viridis(option = "viridis", direction=-1)

# Before we get into the interpolation, we’re going to need to transform both our geographical datasets from degrees-based coordinate systems to metres-based systems.

# Our boundaries data already has a defined coordinate system, so it’s ready to transform, but for our points data we need to first assign it a degrees-based coordinate reference system (CRS) before we transform it.

# In the lines below, we’re specifying which columns contain our geographical coordinates (lon and lat must be entered in that order), and then assigning the CRS
air_qual_data_geometry <- CA_air_qual %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +ellps=WGS84")

# Now we can convert both into a metres-based CRS.
# First we define our CRS.
# proj=utm specifies the UTM projection (https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system)
# zone=10 because our data covers California, which is mainly in UTM zone 10
# units=m will set our grid units to be metres
my_crs <- "+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs"

# Now we can transform our California boundaries object into UTM.
# NB: if your point data covers a larger area than the boundaries, you’ll need to add a buffer around the boundaries here because you’ll want your interpolated surface to interpolate between points either side of your boundaries’ border.
# In our case, all points lie within the boundaries, so `st_buffer(dist = 0)` is not doing anything, but if you did want to add, say, a 10km buffer, you would set the dist value to 10000 (since our units are in metres).
CA_buffered_UTM <- CA %>%
  st_transform(my_crs) %>%
  st_buffer(dist = 0)

# Next up, the points data.
# This first line transforms the points into UTM, and excludes any points outside the buffered boundary
air_qual_data_UTM <- st_transform(air_qual_data_geometry,my_crs)[CA_buffered_UTM, ]
# This next line is just adding the separated lon and lat values (in UTM metres) back into the points dataset, so we can access both the combined geometry field and the individual coordinates
air_qual_data_UTM <- air_qual_data_UTM %>%
  mutate(
    lon = st_coordinates(air_qual_data_UTM)[, 1], 
    lat = st_coordinates(air_qual_data_UTM)[, 2] 
  )

# For the interpolation we need to start working with rasters, so next up we rasterize the boundaries data
# A lower number for the `res` argument (raster reslution in units per cell) will mean a more granular raster, i.e smoother interpolation but calculations will take longer.
my_raster <- raster(CA_buffered_UTM, res=10000)

#######################
#######################
#### INTERPOLATION ####
#######################
#######################

# The interpolation section leans heavily on the gstat package.
# We’re going to carry out three different methods: nearest neighbour, inverse distance weighting and kriging.

######################################
######################################
#### First up, NEAREST NEIGHBOUR: ####
######################################
######################################

# In the line below, we start by defining a formula, rainfall~1. The `~1` means we only care about the relationship between our dependent variable (rainfall) and spatial coordinates.
# `locations` is where we pass our points data
# `nmax` is the maximum number of nearest neighbours points to use for interpolating every cell value. We’re using 10 here.
# `idp` is the power applied to any distance weighting we want to do. zero means no weighting, i.e a simple average of the values of the nearest nmax points to every grid cell.
nearest_neighbour_model <- gstat(formula=ozone~1, locations=air_qual_data_UTM, nmax=10, set=list(idp = 0))

# Next we use our nearest neighbour model to populate our raster:
nearest_neighbour_interpolated <- interpolate(my_raster, nearest_neighbour_model)

# Let’s plot the resulting interpolated surface.
# In one chained command, we first turn the raster back into a grid of points, we set its columns names to be `x`, `y`, and `interpolated_value`, we pass this through R’s ggplot graphics library, specifying that we want to plot a raster image, using our `x` and `y` values as the x and y positinos, and our `interpolated_value` values for fill colour. We’re then adding a fill scale using the viridis colour palette, and inverting its direction so higher values are darker.
rasterToPoints(nearest_neighbour_interpolated) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "interpolated_value")) %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=interpolated_value)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()

# As you can see, there’s an issue here: we’ve got the full interpolated surface, not just California. There’s an easy fix: we mask the full surface with our buffered raster of the California boundaries.
nearest_neighbour_interpolated <- mask(nearest_neighbour_interpolated, CA_buffered_UTM)

# And we plot again. Much better:
rasterToPoints(nearest_neighbour_interpolated) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "interpolated_value")) %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=interpolated_value)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()

# There you have it: simple nearest neighbour interpolation.
# You could try using different values for `nmax` in the original interpolation model to see how the results vary.

##############################################
##############################################
#### Next up, INVERSE DISTANCE WEIGHTING: ####
##############################################
##############################################

# We use exactly the same approach as with simple n-nearest-neighbours, but this time we specify an inverse weighting power greater than zero. Here I’m using `idp = 1` to set a simple linear distance weighting.
IDW_model <- gstat(formula=ozone~1, locations=air_qual_data_UTM, nmax=10, set=list(idp = 1))

# Next we use our IDW model to populate our raster:
IDW_model_interpolated <- interpolate(my_raster, IDW_model)

# Then we mask it to the borders of California:
IDW_model_interpolated <- mask(IDW_model_interpolated, CA_buffered_UTM)

# And plot the result:
rasterToPoints(IDW_model_interpolated) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "interpolated_value")) %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=interpolated_value)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()

# The distance weighting makes for a much smoother interpolation. Try increasing the numbers of nearest neighbours via `nmax` to get it even smoother, and note how increasing the power applied to the distance weighting (via `idp`) from 0 through to 1, and then beyond, brings out increasingly local patterns.

####################################
####################################
#### Third, it’s on to KRIGING. ####
####################################
####################################

# This is a more statistical method than either of the previous two, in that it looks for spatial autocorrelation patterns in the data rather than just doing smooth interpolation. e.g in this case, it’s likely that air pollution spreads out in a specific, meaningful way, which may be apparent in the data.

# As before, we start with a `gstat` command, but this time we just enter our data and formula, leaving all interpolation blank.
kriging_formula <- gstat(formula=ozone~1, locations=air_qual_data_UTM)

# Now we calculate a variogram (http://geog.uoregon.edu/GeogR/topics/variograms.html), which calculates spatial autocorrelation at different distances between points. the `width=20000` argument here is specifying how widely we should bin the distance between points, in the units of our input data (metres). Here I’m specifying 20k bins.
air_quality_variogram <- variogram(kriging_formula, width=20000)

# Now we can plot the results:
ggplot(air_quality_variogram, aes(dist, gamma)) +
  geom_point() +
  theme_minimal()
# This looks like an approximately exponential relationship between the distance between points, and the strength of their correlation.

# We can formalise that by fitting an exponential model to the variogram...
air_quality_variogram_fitted <- fit.variogram(air_quality_variogram, vgm("Exp"))

# ... using that fit to model a full range of values...
# (the `400000` here is the maximum distance that should be modelled)
air_quality_variogram_modelled <- variogramLine(air_quality_variogram_fitted, 400000)

# ... and plotting the fit against the observed, binned autocorrelation:
ggplot(air_quality_variogram_modelled, aes(dist, gamma)) +
  geom_line() +
  geom_point(data = air_quality_variogram %>% dplyr::select(c("dist", "gamma"))) +
  theme_minimal() +
  xlim(0,400000) +
  ylim(0,120)

# NB see here for more detail on fitting variograms (https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html)

# Looks like that model was a good approximation of the relationship. Now we can use that for our kriging.

# First we create an empty grid of cells for our interpolated values to fill:
empty_grid <- as(my_raster, 'SpatialGrid')

# Then we use another gstat command to specify our kriging model. 
kriging_model <- gstat(formula=ozone~1, locations=air_qual_data_UTM, model=air_quality_variogram_fitted)

# Next we use our kriging model to populate our raster:
kriging_interpolated <- predict(kriging_model, empty_grid)

# This gives us a matrix with two layers: the predicted value, and the variance around that value
# We want the predicted values, so we first of all turn our interpolated surface matrix into a raster brick (`brick`), then extract (`subset`) the first layer (predicted values), and then mask it to the boundaries of California.
kriging_interpolated <- brick(kriging_interpolated)
kriging_interpolated <- subset(kriging_interpolated, 1)
kriging_interpolated <- mask(kriging_interpolated, CA_buffered_UTM)

# Now we can plot the kriging interpolation raster:
rasterToPoints(kriging_interpolated) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "interpolated_value")) %>%
  ggplot() +
  geom_raster(aes(x,y,fill=interpolated_value)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()

# Even smoother than the IDW version.

# Here are all three one after the other:

rasterToPoints(nearest_neighbour_interpolated) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "interpolated_value")) %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=interpolated_value)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal() +
  labs(title = "n-nearest neighbours")

rasterToPoints(IDW_model_interpolated) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "interpolated_value")) %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=interpolated_value)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal() +
  labs(title = "Inverse distance weighted")

rasterToPoints(kriging_interpolated) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "interpolated_value")) %>%
  ggplot() +
  geom_raster(aes(x=x, y=y, fill=interpolated_value)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal() +
  labs(title = "Kriging")

# Here’s a decent little explainer on when and why kriging is and is not a better method to use than NN or IDW (https://gis.stackexchange.com/questions/83470/choosing-idw-vs-kriging-interpolation-for-dem-creation). 

# Next:

###############################
#### POPULATION ADJUSTMENT ####
############################### 

# A key thing to note at this stage is that if you want to do precise per-capita population adjustments rather than just overlaying the rasters visually, your interpolated surface and population rasters need to be exactly geospatially aligned, i.e same CRS, same projection, same resolution, same origin, same extent.

# This can cause some issues, since the process of transforming them to be aligned can mean that your population data gets interpolated as part of the re-projection process, meaning the exact numbers of people per grid cell can be distorted. I use a few approximations below to get around this, but let me know if you run into any major hurdles here.

# I suspect you’ve got your own data, but for this example I’ve used gridded population from the Global Human Settlement datase (download link on this page https://ghsl.jrc.ec.europa.eu/ghs_pop2019.php). The specific file I’m using is accessible via this URL: http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_MT_GLOBE_R2019A/GHS_POP_E2015_GLOBE_R2019A_54009_1K/V1-0/GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.zip.

# First up I unzip that file:
unzip("~/Downloads/GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.zip", exdir="~/Downloads/gridded_pop")

# Then I load it into R as a raster layer:
global_pop_raster <- raster("~/Downloads/gridded_pop/GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.tif")

# Then I go back to my California boundaries to create a buffered California shape for cutting out of the global population raster.
# I’m doing this because the global population raster is vast, so I only want to be working with a small part of it for all my raster calculations.
# And I’m creating a new buffered geometry because I need this one to be in the same CRS as the population raster, not the same CRS as my other data
# So here comes my new buffered California geometry, matching the CRS of the population raster, which you can find by running:
crs(global_pop_raster)

# I’ve given this one a 10km buffer because I’m going to need to aggregated the population values up to a less granular resolution in order to match my interpolated surfaces, and I want to make sure that aggregation process doesn’t punish cells on the edge of California by aggregating across full cells and cells outside the borders.
CA_buffer_GHS <- CA_boundaries %>%
  st_transform("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% 
  st_buffer(dist = 10000)

# Next I crop California’s spatial bounding box (using the `extent()` command) out of the global population raster using my new buffered geometry:
CA_pop_raw <- crop(global_pop_raster, extent(CA_buffer_GHS))

# If you take a quick glimpse at the geospatial resolution of two rasters — the population one and, say, the kriging one, you’ll notice that the population one is 10x more spatially granular than our interpolated surface:
res(CA_pop_raw) # 1,000m x 1,000m
res(kriging_interpolated) # 10,000m x 10,000m

# We can get around this by aggregating up the population raster by a factor of 10, and summing the population into each 100x smaller grid call as it goes:
CA_pop <- raster::aggregate(CA_pop_raw, fact=10, fun="sum")
res(CA_pop_raw)

# The population raster now has the same resolution as our interpolated surfaces, but it’s still not quite fully aligned. To achieve that, we need to re-project the population raster to exactly match our interpolated surfaces. We do that by passing any one of our interpolated surface rasters as the second argument to the `projectRaster` command:
CA_pop <- projectRaster(CA_pop, kriging_interpolated)

# The slight interpolation that happens in the re-projection process means we may now have some negative values for population. As a quick-and-dirty fix, I’m just replacing anything below zero with zero.
CA_pop[CA_pop < 0] <- 0

# Then it’s the familiar step of masking that raster with our original Californian boundaries:
CA_pop <- mask(CA_pop, CA_buffered_UTM)

# And then we can plot the result: our raster of Californian population:
rasterToPoints(CA_pop) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "pop")) %>%
  ggplot() +
  geom_raster(aes(x,y,fill=pop)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()

# One other tweak I might suggest is bumping up your population values, as they will have been reduced during the process of re-projecting the population raster.
# The crude way I’ve done this is I’ve taken the total of all gridded population values from the raw, un-projected population raster, and divided it by the total for the new, re-projected one. I’ve then multiplied all cells in the new one by this number, to ensure that the total across the whole state is as it should be:
CA_pop_adjustment_ratio <- (
  (CA_pop_raw %>% values() %>% sum(na.rm=T)) / (CA_pop %>% values() %>% sum(na.rm=T))
  )
CA_pop <- CA_pop * CA_pop_adjustment_ratio

# Here’s teh adjusted population raster:
rasterToPoints(CA_pop) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "pop")) %>%
  ggplot() +
  geom_raster(aes(x,y,fill=pop)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()

# Then if you wanted to plot e.g the kriging interpolated surface adjusted for population, you would do this:
rasterToPoints(kriging_interpolated/CA_pop) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "rate")) %>%
  ggplot() +
  geom_raster(aes(x,y,fill=rate)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()

# You’ll see there are a few suboptimal things to note here:
# 1) Anywhere where nobody lives throws an error (the grey cells) because the value is infinite
# 2) Similarly, anywhere with very few people will produce a stratospheric rate
# 3) As a result, if your population data is as spiky as this, and you do want to calculate per-capita rates, I would probably suggest either using a log colour scale, or binning the values into meaningful brackets, as below:

# log scale:
rasterToPoints(kriging_interpolated/CA_pop) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "rate")) %>%
  ggplot() +
  geom_raster(aes(x,y,fill=log(rate))) + # log scale specified here
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()

# binning into deciles:
rasterToPoints(kriging_interpolated/CA_pop) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "rate")) %>%
  mutate(
    binned_rate = cut(rate, breaks=quantile(rate, seq(0,1,0.1)), labels=quantile(rate, seq(0,0.9,0.1))),
    decile = cut(rate, breaks=quantile(rate, seq(0,1,0.1)), labels=1:10) %>% as.character() %>% as.numeric()
  ) %>%
  ggplot() +
  geom_raster(aes(x,y,fill=decile)) +
  scale_fill_viridis(option = "viridis", direction=-1) +
  theme_minimal()