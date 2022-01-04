devtools::install_github("tylermorganwall/rayshader")
library(rayshader)
library(raster)
library(terra)

#Here, I load a map with the raster package.
dem <-rast("/Users/jacktarricone/Desktop/sagehen/1_dec20_jan31/donner_03904_19084-015_20002-003_0042d_s01_L090HH_01.hgt.grd.tiff")
values(dem)[values(dem) == -10000] = NA

# set crop extent
box <-c(-120.351656,-120.197694,39.377975,39.439883)
box_ext <-ext(box)

# crop images down slightly for better visualization
sh_crop <-crop(dem, box_ext)
plot(sh_crop)

#And convert it to a matrix:
elmat <-raster_to_matrix(raster(sh_crop))

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()