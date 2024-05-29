###
# working with satellite imagery
# 230031
###

setwd("08-satellite-imagery")

if (!require("pacman")) install.packages("pacman"); library("pacman")
p_load(terra)
p_load(sf)
p_load(stringr)
p_load(data.table)
p_load(ggplot2)
p_load(rnaturalearth)
p_load_current_gh("ropensci/rnaturalearthhires")

# load one image
img <- rast(system.file("ex/elev.tif", package="terra"))
plot(img)

img_nightlight = rast("input/20210404_4x.tif")
plot(log(img_nightlight))

# load multiple images
list_images = list.files("input", pattern = "*.tif", full.names = TRUE)
length(list_images)
images = rast(list_images)

# median image of may 2022 and 2021
list_images_may22 = list_images[str_detect(list_images, "202205")]
images_may22 = rast(list_images_may22)
images_may21 = rast(list_images[str_detect(list_images, "202105")])

images_may22_median = median(images_may22)
images_may21_median = median(images_may21)
plot(log(images_may22_median))
plot(log(images_may21_median))

images_may_diff = (images_may21_median - images_may22_median)
plot(log(images_may_diff))

# extract data
plot_data_may22 = terra::values(images_may22_median, dataframe = T)
plot_data_may21 = terra::values(images_may21_median, dataframe = T)

setDT(plot_data_may22)
setDT(plot_data_may21)

# historgram
ggplot() +
    theme_minimal() +
    geom_density(data = plot_data_may22, aes(x = median), n = 100, color = "red") +
    geom_density(data = plot_data_may21, aes(x = median), n = 100, color = "blue") +
    scale_y_log10("Count") +
    scale_x_continuous("Median nightlight intensity")


# vector data on country borders
plot(ne_countries())
plot(ne_countries(country = "Ukraine"))

shape_ukraine = ne_countries(country = "Ukraine")
# str(shape_ukraine)

# make this an sf object
shape_ukraine = st_as_sf(shape_ukraine)
# str(shape_ukraine)
plot(shape_ukraine)

# check if coordinate reference systems match
st_crs(shape_ukraine)
st_crs(images_may22_median)

all.equal(st_crs(shape_ukraine),
          st_crs(images_may22_median))
shape_ukraine = st_transform(shape_ukraine,
    st_crs(images_may22_median))


# crop image to shape
images_may22_median_ukraine = crop(images_may22_median, shape_ukraine)
plot(log(images_may22_median_ukraine))

# mask image to shape
images_may22_median_ukraine_masked = mask(images_may22_median, shape_ukraine)
plot(log(images_may22_median_ukraine_masked))
