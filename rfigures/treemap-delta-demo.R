library(treemap)
library(RSvgDevice)
library(tidyr)
library(dplyr)
library(WDI)

source('treemap-delta.R')

# Get data on 5yr population by sex
wdi <- WDI(country = "all", indicator = c("SP.POP.TOTL"), start = 1990, end = 2015)

# Drop regional aggregates and NAs
is.region <- grepl("[0-9]|EU|X[A-J,L-Z]|ZF|ZG|ZJ|ZT|ZQ|OE", wdi$iso2c) #XK is Kosovo
wdi <- wdi[!is.region & complete.cases(wdi),]

# Keep only 1990 and 2015
wdi <- wdi[wdi$year %in% c(1990, 2015),]

# Keep only those with data for both periods
all <- spread(wdi, year, SP.POP.TOTL, sep=".") 
all <- all[complete.cases(all),]
all$max <- pmax(all$year.1990, all$year.2015)

# Sort for display
all <- all[order(all$country, -all$max),]
all$sortid <- 1:nrow(all)

# Add regions
country_info <- as.data.frame(WDI_data$country)
all$region <- sapply(all$iso2c, function(x) ifelse(
  x %in% country_info$iso2c,
  as.character(country_info$region[country_info$iso2c == x]),
  'None'))

# Only label those with > 0.5% of the 2nd total
year.2015.total <- sum(all$year.2015)
year.2015.pc <- all$year.2015 / year.2015.total
labelled <- all$iso2c[year.2015.pc > 0.0025]

################################################################################
# Plot standard chart
################################################################################

filename <- "../outputs/treemap-deltas"
devSVG(paste0(filename, ".svg"), width=8.5, height=11)
t <- treemap(all, index=c("region", "iso2c"), vSize="max", vColor="region", algorithm="squarified",
             border.col="white", type="categorical", position.legend = "none", draw=T, title="", mirror.y=T)
dev.off()

create_treemap_delta(paste0(filename, "1.svg"), paste0(filename, ".svg"), t, all, "year.1990", label.only = labelled)
create_treemap_delta(paste0(filename, "2.svg"), paste0(filename, ".svg"), t, all, "year.2015", label.only = labelled)
