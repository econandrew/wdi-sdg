library(WDI)
library(svglite)
library(Cairo)
library(countrycode)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RSvgDevice)

source('multiplot.r')

################################################################################
# Global settings
################################################################################

output_folder = "../output/"

# Common settings
width=17
height=11
bgcolor="white"

show.box = F
show.spine = F

plot.device <- pdf # devSVG svg
plot.fn.ext <- '.pdf'

################################################################################
# Setup
################################################################################

# Get data on female employment by sector
f_emp_sectors <- WDI(country = "all", indicator = c(
  "SL.AGR.EMPL.FE.ZS",
  "SL.IND.EMPL.FE.ZS",
  "SL.SRV.EMPL.FE.ZS"
), start = 2000, end = 2015)

# Drop regional aggregates and NAs
is.region <- grepl("[0-9]|X[A-J,L-Z]|ZT|OE", f_emp_sectors$iso2c) #XK is Kosovo
f_emp_sectors <- f_emp_sectors[!is.region & complete.cases(f_emp_sectors),]

# Get most recent year
f_emp_sectors <- f_emp_sectors %>% group_by(iso2c) %>% filter(year == max(year))

# Rescale to total 100 - IGNORING errors
totals <- f_emp_sectors$SL.AGR.EMPL.FE.ZS + f_emp_sectors$SL.IND.EMPL.FE.ZS + f_emp_sectors$SL.SRV.EMPL.FE.ZS
f_emp_sectors$SL.AGR.EMPL.FE.ZS <- f_emp_sectors$SL.AGR.EMPL.FE.ZS * 100 / totals
f_emp_sectors$SL.IND.EMPL.FE.ZS <- f_emp_sectors$SL.IND.EMPL.FE.ZS * 100 / totals
f_emp_sectors$SL.SRV.EMPL.FE.ZS <- f_emp_sectors$SL.SRV.EMPL.FE.ZS * 100 / totals

# Sort by AGR, IND then SRV
#ordering <- order(f_emp_sectors$SL.AGR.EMPL.FE.ZS,
#                  f_emp_sectors$SL.IND.EMPL.FE.ZS,
#                  f_emp_sectors$SL.SRV.EMPL.FE.ZS)
#f_emp_sectors$country <- factor(f_emp_sectors$country, f_emp_sectors$country[ordering])

f_emp_sectors$page <- c(rep("left", 90), #ceiling(nrow(f_emp_sectors)/2)),
                        rep("right", nrow(f_emp_sectors)-90)) # floor(nrow(f_emp_sectors)/2)))

# Reshape for ggplot
f_emp_sectors <- gather(f_emp_sectors, sector, proportion, SL.AGR.EMPL.FE.ZS:SL.SRV.EMPL.FE.ZS)

plot_annotations <- function() {
  if (show.box)
    box(col="red")
  if (show.spine)
    abline(v=grconvertX(8.5*72, from = "device", to = "user"))
}

################################################################################
# Plot all countries, most recent year, pie chart small multiples
################################################################################

plot.device(paste0(output_folder, "small_multiple_pies", plot.fn.ext), width=width, height=height, bg=bgcolor)
par(mai=c(0,0,0,0), oma=c(0,0,0,0)) #bltr

wrapper = function(x) {
  lapply(strwrap(x, width = 12, simplify = FALSE), paste, collapse="\n")
}

plotfun = function(df, margins) {
  ggplot(df, aes(x="", y=proportion, fill=sector)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    facet_wrap(~country, labeller=labeller(country = wrapper), nrow=9, ncol=10) +
    scale_fill_brewer(palette = "Accent") +
    theme_minimal() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none",
          text = element_text(size=8),
          plot.margin=unit(margins,"in"), #trbl
          panel.margin = unit(0.15, "in"))
}

pleft <- plotfun(f_emp_sectors[f_emp_sectors$page == "left",], c(0.75,0.5,0.75,0.75))
pright <- plotfun(f_emp_sectors[f_emp_sectors$page == "right",], c(0.75,1.0,0.75,0.5))

multiplot(pleft, pright, cols=2)

# Plot box and book spine, just so we can see how our projection looks
plot_annotations()
dev.off()

