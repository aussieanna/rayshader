
library(tidyverse)
library(raster)
library(rayshader)
library(av)
library(magick)
library(brickr)

#Rayshader website https://www.rayshader.com/

#Import Geotif file 
yosemite <- raster("./yosemite_srtm_gl1.tif")

#And convert it to a matrix:
yosmat <- matrix(raster::extract(yosemite, raster::extent(yosemite), buffer = 1000),
                  nrow = ncol(yosemite), ncol = nrow(yosemite))

#Use one of rayshader's built-in textures:
yosmat %>%
  sphere_shade(texture = "imhof4") %>%
  plot_map()

#Generate a 3D plot
yosmat %>%
  sphere_shade(texture = "bw") %>%
  plot_3d(yosmat, zscale = 20, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
render_snapshot()

yosmat %>%
  sphere_shade(zscale = 10, texture = "imhof1") %>%
  add_shadow(ray_shade(yosmat, zscale = 20)) %>%
  add_shadow(ambient_shade(yosmat, zscale = 20)) %>%
  plot_3d(yosmat, zscale = 20, theta = -45, phi = 45, water = TRUE,
          windowsize = c(1000,800), zoom = 0.75, waterlinealpha = 0.3,
          wateralpha = 0.5, watercolor = "lightblue", waterlinecolor = "white")
render_snapshot()

pal <- create_texture("#a68fa4", "#6c718c", "#88babf", "#314037", "#647359")
plot_map(pal)

yosmat %>%
  sphere_shade(zscale = 10, texture = pal) %>%
  add_shadow(ray_shade(yosmat, zscale = 20)) %>%
  add_shadow(ambient_shade(yosmat, zscale = 20)) %>%
  plot_3d(yosmat, zscale = 20, theta = -45, phi = 45, water = TRUE,
          windowsize = c(1000,800), zoom = 0.75, waterlinealpha = 0.3,
          wateralpha = 0.5, watercolor = "lightblue", waterlinecolor = "white")
render_snapshot()
#generate a movie file
yosemite_movie = tempfile()
render_movie(filename = yosemite_movie)

save_3dprint("yosemite3d.stl", maxwidth = 150, unit = "mm")
