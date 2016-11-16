library(WDI)
library(svglite)
library(Cairo)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(RSvgDevice)

################################################################################
# Global settings
################################################################################

output_folder = "../outputs/"

# Common settings
width=8.5
height=11
bgcolor="white"

plot.device <- devSVG # pdf devSVG svg
plot.fn.ext <- '.svg'

################################################################################
# Setup
################################################################################

age.lower <- seq(0, 75, 5)
age.string <- c(sprintf("%02d%02d",age.lower,age.lower + 4), "80UP")
age.human <- c(sprintf("%d-%d",age.lower,age.lower + 4),"80+")
ind.sex.age = data.frame(
  ind = c(paste0("SP.POP.",age.string,".FE.5Y"), paste0("SP.POP.",age.string,".MA.5Y")),
  sex = c(rep("Female",length(age.string)), rep("Male",length(age.string))),
  age = rep(factor(age.human, levels=age.human, ordered=T),2)
)

# Get data on 5yr population by sex
pop <- WDI(country = "all", indicator = ind.sex.age$ind, start = 2010, end = 2010)

# Drop regional aggregates and NAs
is.region <- grepl("[0-9]|X[A-J,L-Z]|ZJ|ZT|OE", pop$iso2c) #XK is Kosovo
pop <- pop[!is.region & complete.cases(pop),]

# Reshape for ggplot
pop <- gather(pop, ind, proportion, SP.POP.0004.FE.5Y:SP.POP.80UP.MA.5Y)

# Add sex and age group variables, and set age order
pop$sex <- sapply(pop$ind, function(x) ind.sex.age$sex[ind.sex.age$ind == x])
pop$age <- sapply(pop$ind, function(x) ind.sex.age$age[ind.sex.age$ind == x])

# Invert male proportions so they run left
pop$proportion[pop$sex == "Male"] <- -pop$proportion[pop$sex == "Male"]

################################################################################
# Plot all countries population pyramids
################################################################################

plot.device(paste0(output_folder, "small_multiple_pyramids", plot.fn.ext), width=width, height=height, bg=bgcolor)
par(mai=c(0,0,0,0), oma=c(0,0,0,0)) #bltr

wrapper = function(x) {
  lapply(strwrap(x, width = 12, simplify = FALSE), paste, collapse="\n")
}

# https://rpubs.com/walkerke/pyramids_ggplot2
ggplot(pop, aes(x=age, y=proportion, fill=sex)) +
  geom_bar(data = pop[pop$sex == "Female",], stat = "identity") + 
  geom_bar(data = pop[pop$sex == "Male",], stat = "identity") + 
  facet_wrap(~country, labeller=labeller(country = wrapper)) + #, nrow=10, ncol=5) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-20, 20, 2)) + 
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(color = "darkgrey", size=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        text = element_text(size=8),
        plot.margin=unit(c(1,1,1,1),"in"), #trbl
        panel.margin = unit(0.0, "in"))

dev.off()

