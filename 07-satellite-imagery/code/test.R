###
# working with satellite imagery
# 220527
###

setwd("08-satellite-imagery")




if (!require("pacman")) install.packages("pacman"); library("pacman")
p_load(terra)
p_load(sf)
p_load(stringr)
p_load(ggplot2)
p_load(data.table)
p_load(ggthemes)
p_load_current_gh("ropenscilabs/rnaturalearth")


# load one image
image = rast("input/20210404_4x.tif")
plot(log(image))
