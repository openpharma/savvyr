library(hexSticker)
library(magick)
library(sysfonts)
library(tidyverse)
library(rsvg)
library(svglite)

savvy_img <- image_read("savvy.png")

x <- sticker(
  subplot = savvy_img,
  s_x = 1,
  s_y = 1,
  s_width = 1.75,
  s_height = 1.75,
  package = "savvyr",
  url = "github.com/openpharma/savvyr",
  u_size = 24,
  u_color = "blue",
  p_x = 1.1,
  p_y = 1.1,
  p_color = "blue",
  p_size = 140,
  p_family = "mono",
  h_fill = "white",
  h_color = "black",
  h_size = 2,
  spotlight = FALSE,
  l_y = 1,
  l_x = 1,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.3,
  dpi = 2000,
  white_around_sticker = TRUE
)

my_save_sticker <- function(filename,
                            sticker = last_plot(),
                            width = 43.9,
                            height = 50.8,
                            ...) {
  args <- list(
    filename = filename, plot = sticker, units = "mm", bg = "transparent",
    width = width, height = height,
    ...
  )
  is_png <- (!is.null(args$device) && args$device == "png") ||
    tools::file_ext(filename) == "png"
  is_win <- .Platform$OS.type == "windows"
  if (is_png && is_win && capabilities("cairo")) {
    args$type <- "cairo-png"
    args$antialias <- "subpixel"
  }
  do.call(ggsave, args)
}

my_save_sticker(
  filename = "../man/figures/logo.svg",
  sticker = x,
  width = 43.9 * 2,
  height = 50.8 * 2
)
