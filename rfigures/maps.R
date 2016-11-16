# Make an untitled ternary SVG map for inclusion in print graphics

library(maptools)
library(WDI)
library(rgdal)
library(svglite)
library(Cairo)
library(graticule)

source("slice_world.R")

################################################################################
# Global settings
################################################################################

output_folder = "../outputs/"

# Define our graticule (lines of lat/long)
graticule_lons <- seq(-180, 180, 15)
graticule_lats <- seq(-90, 90, 15)

# Common settings
width=17
height=11
bgcolor="white"

show.graticule = F
show.box = F
show.spine = F

plot.device <- pdf #svg
plot.fn.ext <- '.pdf' #.svg

################################################################################
# Setup
################################################################################

# Load world and remove Antarctica
data("wrld_simpl")
wrld_simpl <- wrld_simpl[wrld_simpl@data$NAME != "Antarctica",]

# Get data on forest protected areas from WDI and join with map data on ISO code
forestdata <- WDI(country = "all", indicator = "ER.LND.PTLD.ZS", start = 2014, end = 2014)
forestdata.o <- forestdata[match(wrld_simpl$ISO2,forestdata$iso2c),]

# Define colors for choropleth
findColors3 <- function(x) {
  if (!is.na(x)){
    if (x <= 5)               return("#ffeda0")
    else if (x > 5 & x <= 20) return("#feb24c")
    else if (x > 20)          return("#f03b20")
    else                      return("black")
  }
  return("white") #shade nulls
}
ternaryColors <- sapply(forestdata.o$ER.LND.PTLD.ZS, findColors3)

plot_annotations <- function() {
  if (show.graticule)
    plot(graticule(lons, lats, proj = proj), add=TRUE, col = "lightgray")
  if (show.box)
    box(col="red")
  if (show.spine)
    abline(v=grconvertX(8.5*72, from = "device", to = "user"))
}

################################################################################
# Plot attempt #1 - Gall-Peters centered on 25 degrees W
################################################################################

proj <- "+proj=cea +lon_0=25w +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
wrld_tx <- slice_world(wrld_simpl, 155)
wrld_tx <- spTransform(wrld_tx, CRS(proj))

plot.device(paste0(output_folder, "gall-peters-25w", plot.fn.ext), width=width, height=height, bg=bgcolor)
par(mai=c(0,0,0,0), oma=c(0,0,0,0)) #bltr
plot(wrld_tx, col=ternaryColors, lwd=0.3)

# Plot graticule, box and book spine, just so we can see how our projection looks
plot_annotations()
dev.off()

################################################################################
# Plot attempt #1b - Squashed Gall-Peters centered on 25 degrees W
################################################################################

# Only difference is lat_ts=40 below instead of 45, changes aspect ratio slightly
proj <- "+proj=cea +lon_0=25w +x_0=0 +y_0=0 +lat_ts=40 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
wrld_tx <- slice_world(wrld_simpl, 155)
wrld_tx <- spTransform(wrld_tx, CRS(proj))

plot.device(paste0(output_folder, "gall-peters-40-25w", plot.fn.ext), width=width, height=height, bg=bgcolor)
par(mai=c(0,0,0,0), oma=c(0,0,0,0)) #bltr
plot(wrld_tx, col=ternaryColors, lwd=0.3)

# Plot graticule, box and book spine, just so we can see how our projection looks
plot_annotations()
dev.off()

################################################################################
# Plot attempt #2 - Winkel-Tripel normally centered, with left margin
################################################################################

proj <- "+proj=wintri +lon_0=0w"
wrld_tx <- spTransform(wrld_simpl, CRS(proj))

plot.device(paste0(output_folder, "winkel-tripel", plot.fn.ext), width=width, height=height, bg=bgcolor)
par(mai=c(0,1.75,0,0), oma=c(0,0,0,0)) #bltr
plot(wrld_tx, col=ternaryColors, lwd=0.3)

# Plot graticule, box and book spine, just so we can see how our projection looks
plot_annotations()
dev.off()

################################################################################
# Plot attempt #3 - Winkel-Tripel centered on 25 deg W
################################################################################

proj <- "+proj=wintri +lon_0=25w"
wrld_tx <- slice_world(wrld_simpl, 155)
wrld_tx <- spTransform(wrld_tx, CRS(proj))

plot.device(paste0(output_folder, "winkel-tripel-25w", plot.fn.ext), width=width, height=height, bg=bgcolor)
par(mai=c(0,0,0,0), oma=c(0,0,0,0)) #bltr
plot(wrld_tx, col=ternaryColors, lwd=0.3)

# Plot graticule, box and book spine, just so we can see how our projection looks
plot_annotations()
dev.off()
