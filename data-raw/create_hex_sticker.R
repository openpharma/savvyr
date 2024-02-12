library(hexSticker)
library(magick)
library(rsvg)
library(svglite)

less_than_black <- "gray27"
very_light_gray <- "gray98"
out_path <- file.path(getwd(), "man/figures/logo-large")
out_file <- paste(out_path, "png", sep = ".")

savvyr_plot <- function() {
  cuminc_exp <- function(t, lambda) {
    # cumulative incidence functions for exponential distribution
    # t: time
    # lambda: vector of rates
    s <- sum(lambda)
    res <- matrix(NA, ncol = length(lambda), nrow = length(t))
    res[, seq_along(lambda)] <- (1 - exp(-s * t)) / s
    res[t <= 0, seq_along(lambda)] <- 0
    res <- t(lambda * t(res))
    rownames(res) <- t
    colnames(res) <- lambda
    return(res)
  }

  ts <- seq(-5, 25, by = 0.01)
  l1 <- c(1, 1.6) / 10
  l2 <- c(1.8, 1.2) / 10


  # cumulative incidence
  plot(0, 0,
    xlim = c(0, 20), ylim = c(0, 1),
    type = "n", xlab = "time", ylab = "cumulative AE risk",
    main = "cumulative AE risk",
    xaxt = "n", yaxt = "n", col.main = less_than_black, col.lab = less_than_black,
    axes = FALSE, mgp = c(1, 1, 0), bg = very_light_gray
  )
  lines(ts, cuminc_exp(ts, l1)[, 1], col = "blue", lwd = 3)
  lines(ts, cuminc_exp(ts, l2)[, 1], col = "green", lwd = 3)
  axis(side = 2, at = c(0, 1), las = 1, col = less_than_black, col.axis = less_than_black)
  axis(side = 1, at = c(0, 20), labels = c(0, NA), las = 1, col = less_than_black, col.axis = less_than_black)
  text(x = 20, y = 0.53, labels = "control", col = "green", adj = c(1, 0.5), cex = 1)
  text(x = 20, y = 0.33, labels = "experimental", col = "blue", adj = c(1, 0.5), cex = 1)
}

savvyr_plot()

tmp <- tempdir()
savvyr_fig <- file.path(tmp, "savvyr_plot.png")
png(filename = savvyr_fig, res = 100, width = 400, height = 400, bg = "transparent")
savvyr_plot()
dev.off()

plot(magick::image_read(savvyr_fig))

x <- sticker(
  subplot = savvyr_fig,
  s_x = 0.9,
  s_y = 0.95,
  s_width = 0.8,
  s_height = 0.8,
  package = "savvyr",
  url = "github.com/openpharma/savvyr",
  u_size = 24,
  u_color = "blue",
  p_x = 1.0,
  p_y = 1.3,
  p_color = "blue",
  p_size = 200,
  p_family = "mono",
  h_fill = very_light_gray,
  h_color = less_than_black,
  h_size = 2,
  spotlight = FALSE,
  l_y = 1,
  l_x = 1,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.3,
  dpi = 2000,
  white_around_sticker = TRUE,
  filename = out_file
)
# But therefore we now need to postprocess the image by transforming white
# to transparent background.
out_img <- magick::image_read(out_file)
out_img <- magick::image_transparent(out_img, color = "white")
magick::image_write(out_img, path = out_file)

usethis::use_logo(out_file)
