
library(tidyverse)
library(raster)
library(rayshader)
library(av)
library(magick)
library(brickr)

#Rayshader website https://www.rayshader.com/

#Import Geotif file 
zion <- raster("./zion_srtm_gl1.tif")

#And convert it to a matrix:
zionmat <- matrix(raster::extract(zion, raster::extent(zion), buffer = 1000),
                 nrow = ncol(zion), ncol = nrow(zion))

#Use one of rayshader's built-in textures:
zionmat %>%
  sphere_shade(texture = "imhof2") %>%
  plot_map()

#Generate a 3D plot
zionmat %>%
  sphere_shade(texture = "bw") %>%
  plot_3d(zionmat, zscale = 20, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
render_snapshot()

#And here we add an ambient occlusion shadow layer, which models 
#lighting from atmospheric scattering:
raymat <- ray_shade(zionmat)
ambmat <- ambient_shade(zionmat)

zionmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(zionmat), color = "desert") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  plot_map()

#Make 3D image
zionmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(zionmat), color = "imhof1") %>%
  add_shadow(ray_shade(zionmat, zscale = 3, maxsearch = 300), 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  plot_3d(zionmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
#render_snapshot()
#Save for 3D printing
#save_3dprint("zion_3d.stl", maxwidth = 150, unit = "mm")
save_3dprint("zion_3d_sm.stl", maxwidth = 75, unit = "mm")

#generate a movie file
zion_movie = tempfile()
render_movie(filename = zion_movie)

#zion colours
create_texture("#657a8c", "#65736f", "#d9b391", "#bf847e", "#a64949") %>% 
  plot_map()

zionmat %>%
  sphere_shade(texture = create_texture("#657a8c", "#65736f", "#d9b391", "#bf847e", "#a64949")) %>%
  add_shadow(ray_shade(zionmat, zscale = 3, maxsearch = 300), 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  plot_3d(zionmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
render_snapshot()

#Plot as bricks - throws an error! It could be because the image is not a square?
#zionmat %>% 
  #bricks_from_rayshader(zionmat) %>% 
  #build_bricks(theta = 135, phi = 45)

#render_snapshot(clear = TRUE)

###############Custom functions to generate a GIF. Source: https://github.com/wcmbishop/rayshader-demo/blob/master/R/rayshader-gif.R
save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  require(rayshader)
  require(magick)
  require(rgl)
  require(gifski)
  require(rlang)
  
  # capture dot arguments and extract variables with length > 1 for gif frames
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                 !(names(dots) %in% var_exception_list)]
  # split off dot variables to use on gif frames
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  # build expressions for gif variables that include index 'i' (to use in the for loop)
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  # TODO - can we recycle short vectors?
  if (length(unique(gif_var_lengths)) > 1) 
    stop("all gif input vectors must be the same length")
  n_frames <- unique(gif_var_lengths)
  
  # generate temp .png images
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames))
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  # build gif
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames), 
                          path = file, delay = duration/n_frames)
  message("Done!")
  invisible(file)
}

plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}

transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
  }
  
  middle - half_width * scaling
}
#####################################

#Generate a GIF
# calculate input vectors for gif frames
n_frames <- 180
thetas <- transition_values(from = -45, to = -135, steps = n_frames)
# generate gif
zscale <- 50
zionmat %>%
  sphere_shade(texture = create_texture("#657a8c", "#65736f", "#d9b391", "#bf847e", "#a64949")) %>%
  add_shadow(ray_shade(zionmat, zscale = 3, maxsearch = 300), 0.5) %>%
  add_shadow(ambmat, 0.5) %>%
  save_3d_gif(zionmat, file = "zion.gif", duration = 6,
              solid = TRUE, shadow = TRUE, zscale = 10,
              theta = thetas, phi = 45)
