# Start by installing the "needs" package — makes it very straightforward to bring in new packages in future
install.packages("needs")
library(needs)

# Load the packages you need
needs(tidyverse, magrittr, raster, ncdf4, rgdal, RColorBrewer)

# Download all your ice coverage netCDF files from the Copernicus Climate Data Store (https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-sea-ice?tab=overview)

# Unzip the archived files into a folder of your choosing, and note down the path to that folder.
# NB: I would recommend not storing these files in a folder that syncs to e.g Dropbox, as they’re very large

# Get a list of all your netCDF file names — edit the path to the dicretory containing your files
netCDFs <- list.files("path/to/your/netCDF/files", pattern="ice_conc_nh_ease.+\\.nc", full.names = TRUE)

# Store the file names in a tibble, with the filename in one column and the date in the other, and sort by date, so the first is the oldest and the last is the newest
netCDFs_table <- tibble(
  netCDFs,
  date = str_extract(netCDFs, "\\d{12}") # This line is extracting any string of 12 consecutive digits
) %>%
  arrange(date)

# Then pull out just the file names again, but now they’re in the correct order
netCDFs <- netCDFs_table$netCDFs
netCDF_dates <- netCDFs_table$date

# Create an empty list of lists: 12 lists (one for each month of the year), of 41 objects each (one for each year)
arctic_ice_conc_list_months <- vector("list", 12) %>% 
  set_names(as.character(1:12) %>% str_pad(2, "left", "0")) %>%
  map(~ vector("list", 41) %>% set_names(as.character(1979:2019)))

# For each netCDF file, extract its ice_conc layer and its month value, and place the raster layer into the corresponding month/year list object
for(i in seq_along(netCDFs)){
  ice_conc_raster <- raster(netCDFs[i], varname = "ice_conc")
  month <- netCDF_dates[i] %>% str_sub(5,6) # extract the month from the file name
  year <- netCDF_dates[i] %>% str_sub(1,4) # extract the year from the file name
  # Use the month and year values to place the raster into the correct month list, at the correct year point
  arctic_ice_conc_list_months[month][[1]][year] <- ice_conc_raster
  message(paste0("Done file ",i," of ",length(netCDFs)))
}

# Create a new list that will hold the raster layer average for each calendar month
average_list_months <- vector("list", 12) %>% 
  set_names(as.character(1:12) %>% str_pad(2, "left", "0"))

# Calculate the average for each calendar month
# We start by using names(arctic_ice_conc_list_months) to give us the unique list of two-digit zero-padded month values
for(i in names(arctic_ice_conc_list_months)){
  # Then in the line below, here’s what’s happening:
  # We start by taking the nested list of months and years: arctic_ice_conc_list_months
  # We index into it [i] at the month in question
  # We use [[1]] to jump fully into that list element
  # We then use [as.character(1981:2010)] to specify the years that we want to use for our average. If we just put [1981:2010] (i.e withouth the as.character transformation) we would get the 1981st to the 2010th object, but of course our list is nowhere near that long, so we’re converting the numbers to characters in order to pluck the years out by name instead
  # Finally we use compact() to ensure that any NULL (empty) cases are ignored
  # Then we use reduce(mean, na.rm=T) to average them all, ignoring any NA raster cells
  average_list_months[i] <- arctic_ice_conc_list_months[i][[1]][as.character(1981:2010)] %>% compact() %>% reduce(mean, na.rm=T)
  message(paste0("Done month ",i))
}

# Plot the average for e.g August
raster::plot(average_list_months$`08`)
# ...or April
raster::plot(average_list_months$`04`)

# Calculate the anomaly for e.g October 2018, by subtracting the average from your chosen month
arctic_ice_conc_anom_201908 <- arctic_ice_conc_list_months["08"][[1]]["2019"][[1]] %>% 
  subtract(average_list_months$`08`)

# As above, but dividing rather than subtracting (i.e the NSIDC method)
arctic_ice_conc_anom_201908_rel <- (arctic_ice_conc_list_months["08"][[1]]["2019"][[1]] - average_list_months$`08`) %>% 
  divide_by(average_list_months$`08`) %>%
  multiply_by(100)
# Here we set all grid cells where there is never any sea ice to have the value zero — this will let us strip these cells out when we plot the graphic, and allow the ocean beneath to show through
arctic_ice_conc_anom_201908_rel[average_list_months$`08`==0] <- 0

# Here we’re re-projecting our two anomaly rasters to projections that do a better job of showing the area around the North Pole
arctic_ice_conc_anom_201908 <- projectRaster(arctic_ice_conc_anom_201908, crs="+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

arctic_ice_conc_anom_201908_rel <- projectRaster(arctic_ice_conc_anom_201908_rel, crs="+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Plot the results, using the simple plot() function from the raster library
raster::plot(arctic_ice_conc_anom_201908)
raster::plot(arctic_ice_conc_anom_201908_rel)
# Notice how we see very little with the relative anomaly plot? This is because in the process of dividing by the average value, we get some extremely high outlier values. These ara cases where the average value was so low that even a small increase in August 2018 appears as an enormous spike, due to the quirks of dividing by minute numbers. We’ll deal with this when we do our proper plots in a few moments.

# Now for some proper plots!
# First, the percentage-point different anomalies

# IN this first step we convert our raster grid into a table (strictly speaking a tibble()), to prepare for plotting with ggplot, and to allow us to clean up the data a little bit for an optimal colour scale
rasterToPoints(arctic_ice_conc_anom_201908) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "anomaly")) %>% 
  # In the `case_when` block below, we’re setting hard limts for our colour scale at -50 percentage points (half of the average ice coverage lost) and +50 percentage points (an extra half as much ice than normal), and then setting any anomalies of exactly zero to be NA values. This latter step is our way of telling ggplot to treat these values as holes in the data (they represent locations that never have sea ice), and allowing the ocean to appear through the holes.
  mutate(anomaly = case_when(
    anomaly < -50 ~ -50,
    anomaly > 50 ~ 50,
    anomaly == 0 ~ NA_real_,
    T ~ anomaly
  )) %>% 
  ggplot() +
  geom_raster(aes(x,y,fill=anomaly)) +
  # The line below defines our colour sale, using the red-blue diverging palette from ColorBrewer. The `labels` argument is just me being pedantic and adding `+` plus symbols to anomalies above zero, i.e cases where there is more ice than on average. NB the `na.value = "#2D303B` line is telling ggplot to colour any NA values — the holes we created where there is never any ice — in a light shade of grey. This is essentially our "open ocean" colour.
  scale_fill_gradientn(colours = c("#FC0061", "#B80143", "#3D3D4A", "#0292B1", "#01D0FF"), na.value = "#2D303B", breaks = seq(-50, 50, 25), labels = seq(-50, 50, 25) %>% {ifelse(. < 0, as.character(.), paste0("+",.))}) +
  # The line below sets up a cartesian coordinate grid with equal scaling on both axes, preserving the true aspect ratio of the geography we’re plotting
  coord_equal(xlim = c(-3e3, 3e3), ylim = c(-3e3, 3e3)) +
  # In the next block, we’re defining the visual theme of the graphic, setting things like the font family and position of the legend. The `panel.background` colour will determine the colour of the landmasses.
  theme(
    text = element_text(family = "Open Sans"),
    panel.background = element_rect(fill = "#4e5a6a"),
    panel.grid = element_blank(),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  # Below we set the legend title and position
  guides(fill = guide_colourbar(
    title = "Ice concentration anomaly\n(Percentage points)",
    title.position = "top",
    title.theme = element_text(color = "#FFFFFF", family = "Open Sans", size = 11),
    label.position = "top",
    label.theme = element_text(color = "#FFFFFF", family = "Open Sans", size = 10),
    direction = "horizontal",
    barwidth = 9
  ))
ggsave("anomaly_201908.png", width=900/125.9, height=900/125.9, units="cm", scale=2, dpi="retina")

# And now the same for the relative percentage-change anomalies. This is mostly the same as above, but with a couple of small differences:
rasterToPoints(arctic_ice_conc_anom_201908_rel) %>% 
  as_tibble() %>%
  set_names(c("x", "y", "anomaly")) %>% 
  # In the block below, we’re capping the top of our colour scale at a 100% ice coverage increase. We don’t need to cap the bottom, since the lowest the anomaly in ice coverage can be is -100%, for cases where there is usually some ice, but there is now no ice. We’re also converting zero values to NAs again for the open ocean.
  mutate(anomaly = case_when(
    anomaly > 100 ~ 100,
    anomaly == 0 ~ NA_real_,
    T ~ anomaly
  )) %>% 
  ggplot() +
  geom_raster(aes(x,y,fill=anomaly)) +
  scale_fill_gradientn(colours = c("#FC0061", "#B80143", "#3D3D4A", "#0292B1", "#01D0FF"), na.value = "#2D303B", breaks = seq(-100, 100, 50), labels = seq(-100, 100, 50) %>% {ifelse(. < 0, as.character(.), paste0("+",.))}) +
  coord_equal(xlim = c(-3e3, 3e3), ylim = c(-3e3, 3e3)) +
  theme(
    text = element_text(family = "Open Sans"),
    panel.background = element_rect(fill = "#4e5a6a"),
    panel.grid = element_blank(),
    legend.position = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  guides(fill = guide_colourbar(
    title = "Ice concentration anomaly (%)",
    title.position = "top",
    title.theme = element_text(color = "#FFFFFF", family = "Open Sans", size = 11),
    label.position = "top",
    label.theme = element_text(color = "#FFFFFF", family = "Open Sans", size = 10),
    direction = "horizontal",
    barwidth = 10
  ))
ggsave("relative_anomaly_201908.png", width=900/125.9, height=900/125.9, units="cm", scale=2, dpi="retina")

# Finally, we can write our two anomaly rasters into new GeoTIFF files
writeRaster(arctic_ice_conc_anom_201908, "arctic_ice_conc_anom_201908.tif", options = c("TFW=YES"))
writeRaster(arctic_ice_conc_anom_201908_rel, "arctic_ice_conc_anom_201908_rel.tif", options = c("TFW=YES"))

# And here’s a function that wraps all of that above steps into one, for any given month.
# Set `plot` to `FALSE` if you just want to process the data and save the GeoTiffs without plotting the graphics
process_month_year <- function(year, month_two_digits, plot=TRUE){
  
  month_anomaly <- arctic_ice_conc_list_months[month_two_digits][[1]][year][[1]] %>% 
    subtract(average_list_months[month_two_digits][[1]])
  
  month_anomaly_rel <- (arctic_ice_conc_list_months[month_two_digits][[1]][year][[1]] - average_list_months[month_two_digits][[1]]) %>% 
    divide_by(average_list_months[month_two_digits][[1]]) %>%
    multiply_by(100)
  month_anomaly_rel[average_list_months[month_two_digits][[1]]==0] <- 0
  
  month_anomaly <- projectRaster(month_anomaly, crs="+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  month_anomaly_rel <- projectRaster(month_anomaly_rel, crs="+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  plot_ppt_anomaly <- rasterToPoints(month_anomaly) %>% 
    as_tibble() %>%
    set_names(c("x", "y", "anomaly")) %>% 
    mutate(anomaly = case_when(
      anomaly < -50 ~ -50,
      anomaly > 50 ~ 50,
      anomaly == 0 ~ NA_real_,
      T ~ anomaly
    )) %>% 
    ggplot() +
    geom_raster(aes(x,y,fill=anomaly)) +
    scale_fill_distiller(palette = "RdBu", direction=1, breaks = seq(-50, 50, 25), labels = seq(-50, 50, 25) %>% {ifelse(. < 0, as.character(.), paste0("+",.))}, na.value = "grey90") +
    coord_equal(xlim = c(-3e3, 3e3), ylim = c(-3e3, 3e3)) +
    theme(
      text = element_text(family = "Open Sans"),
      panel.background = element_rect(fill = "grey60"),
      panel.grid = element_blank(),
      legend.position = c(1,1),
      legend.justification = c(1,1),
      legend.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    guides(fill = guide_colourbar(
      title = "Ice concentration anomaly (%pts)",
      title.position = "top",
      direction = "horizontal"
    ))
  
  plot_pc_anomaly <- rasterToPoints(month_anomaly_rel) %>% 
    as_tibble() %>%
    set_names(c("x", "y", "anomaly")) %>% 
    mutate(anomaly = case_when(
      anomaly > 100 ~ 100,
      anomaly == 0 ~ NA_real_,
      T ~ anomaly
    )) %>% 
    ggplot() +
    geom_raster(aes(x,y,fill=anomaly)) +
    scale_fill_distiller(palette = "RdBu", direction=1, breaks = seq(-100, 100, 50), labels = seq(-100, 100, 50) %>% {ifelse(. < 0, as.character(.), paste0("+",.))}, na.value = "grey90") +
    coord_equal(xlim = c(-3e3, 3e3), ylim = c(-3e3, 3e3)) +
    theme(
      text = element_text(family = "Open Sans"),
      panel.background = element_rect(fill = "grey60"),
      panel.grid = element_blank(),
      legend.position = c(1,1),
      legend.justification = c(1,1),
      legend.background = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    guides(fill = guide_colourbar(
      title = "Ice concentration anomaly (%)",
      title.position = "top",
      direction = "horizontal"
    ))
  
  if(plot == TRUE){
    print(plot_ppt_anomaly)
    print(plot_pc_anomaly) 
  }
  
  writeRaster(month_anomaly, paste0("arctic_ice_conc_anom_",year,month_two_digits,".tif"), options = c("TFW=YES"))
  writeRaster(month_anomaly_rel, paste0("arctic_ice_conc_anom_",year,month_two_digits,"_rel.tif"), options = c("TFW=YES"))
  
  message(paste0("Done ", month_two_digits, year))
  
}

process_month_year("2019", "08")

########################################################################################
########################################################################################

# P.S. useful footnote:
# If you just want to read all the metadata for a netCDF, you can do this:

# Open a netCDF file. This doesn’t load it into memory, but it makes it accessible to R
your_netcdf_metadata <- nc_open("path/to/your/netCDF/file")

# Show summary information about the netCDF, e.g all the variables it contains, whether it has a geographical projection, what each variable means and what units it’s in, etc
your_netcdf_metadata