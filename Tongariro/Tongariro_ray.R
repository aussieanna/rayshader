library(tidyverse)
library(raster)
library(rayshader)
library(av)
library(magick)
library(brickr)

#Rayshader website https://www.rayshader.com/

#Import Geotif file 
tong <- raster("./tong_srtm_gl1.tif")

#And convert it to a matrix:
tongmat <- matrix(raster::extract(tong, raster::extent(tong), buffer = 1000),
                  nrow = ncol(tong), ncol = nrow(tong))

#Use one of rayshader's built-in textures:
tongmat %>%
  sphere_shade(texture = "imhof2") %>%
  plot_map()

#Generate a 3D plot
tongmat %>%
  sphere_shade(texture = "imhof2") %>%
  plot_3d(tongmat, zscale = 10, fov = 0, theta = 135, zoom = 0.5, phi = 45, windowsize = c(1000, 600))
render_snapshot()

