## Setup -----
{
  require(ggplot2)
  require(spinifex)
  require(patchwork)
  require(ggplot2)

  dat <- spinifex::penguins_na.rm[, 1:5]
  colnames(dat) <- c("bl", "bd", "fl", "bm", "species")
  X   <- scale_sd(dat[, 1:4])
  Y   <- dat$species
  bas <- basis_olda(X, Y, 4)
  .t  <- theme(panel.border  = element_rect(size = .4, color = "grey60", fill = NA),
               legend.margin = margin(0,0,0,0),
               plot.margin   = margin(0,0,0,0))
}

## penguin_cl_sep -----
left <- ggtour(bas[, c(1,2)], X, 0) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) +
  proto_origin() + .t# + labs(color = "species", shape = "species")
right <- ggtour(bas[, c(3,4)], X, 0) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6)  +
  proto_origin() + .t + theme(legend.position = "off")
(pw <- left + right)
## difficult to get the legend centered, not worth the roi
ggsave("./figures/ch1_fig2_penguin_cl_sep.png", pw,
       device = "png", width = 6, height = 2.45, units = "in")


## penguin_manualtour -----
bas     <- basis_olda(X, Y)
mt_path <- manual_tour(bas, 1, data = X)

ggt <- ggtour(mt_path, X, .3) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) + theme(legend.position = "off")
## Use this to pick frame 2 (full), 8 (half), 10 (0)
(fs <- filmstrip(ggt, ncol = 3))

interp <- spinifex:::interpolate_manual_tour(mt_path, angle = .3)
bas_full <- interp[,,2]
bas_half <- interp[,,8]
bas_zero <- interp[,,10]
attr(bas_full, "manip_var") <-
  attr(bas_half, "manip_var") <-
  attr(bas_zero, "manip_var") <- attributes(mt_path)$manip_var


full <- ggtour(bas_full, X, .3) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) +
  proto_origin() + .t +
  theme(legend.position = "off")
half <- ggtour(bas_half, X, .3) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) + .t +
  proto_origin()
zero <- ggtour(bas_zero, X, .3) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) +
  proto_origin() + .t +
  theme(legend.position = "off")
(pw <- full + half + zero)
ggsave("./figures/ch1_fig3_penguin_manualtour.png", pw, device = "png",
       width = 6, height = 2, units = "in")

## Save a video
animate_gganimate(
  ggtour(mt_path, X, .15) +
    proto_point(list(color = Y, shape = Y)) +
    proto_basis(line_size = .6) +
    proto_origin() +
    theme(legend.position = "off"),
  width = 10, height = 5.625, units = "in",
  res = 128, ## resolution, pixels per dimension unit
  render = gganimate::av_renderer("./figures/ch1_fig3_penguin_manualtour.mp4")) ## Alternative render
beepr::beep(4)
#1280x720 (true 720p)
#720/128=5.625

