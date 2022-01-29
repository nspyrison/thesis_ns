## Setup -----
{
  require(ggplot2)
  require(spinifex)

  dat <- spinifex::penguins_na.rm[, 1:5]
  colnames(dat) <- c("b_l", "b_d", "f_l", "b_m", "species")
  X <- dat[, 1:4] %>% scale_sd
  Y <- dat$species
  bas <- basis_olda(X, Y, 4)
}

## penguin_cl_sep -----
left <- ggtour(bas[, c(3,4)], X, 0) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) + theme(legend.position = "off")
right <- ggtour(bas[, c(1,2)], X, 0) +
    proto_point(list(color = Y, shape = Y)) +
    proto_basis(line_size = .6) + theme(legend.position = "off")
(cp <- cowplot::plot_grid(left, right))
ggsave("./figures/ch1_fig2_penguin_cl_sep.png", cp, device = "png",
       width = 6, height = 2, units = "in")


## penguin_mt_filmstrip -----
bas     <- basis_olda(X, Y)
mt_path <- manual_tour(bas, 1, data = X)

ggt <- ggtour(mt_path, X, 99) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) + theme(legend.position = "off")
(fs <- filmstrip(ggt, ncol = 3))
ggsave("./figures/ch1_fig4_penguin_manualtour.png", fs, device = "png",
       width = 6, height = 2, units = "in")

## Save a video
animate_gganimate(
  ggtour(mt_path, X, .15) +
    proto_point(list(color = Y, shape = Y)) +
    proto_basis(line_size = .6) + theme(legend.position = "off"),
  height = 4, width = 6 , units = "in",
  res = 200, ## resolution, pixels per dimension unit I think
  render = gganimate::av_renderer("./figures/ch1_fig4_penguin_manualtour.mp4")) ## Alternative render
