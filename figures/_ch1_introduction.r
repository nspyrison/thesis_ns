require(ggplot2)
require(spinifex)

## penguin_cl_sep -----
dat <- spinifex::penguins_na.rm[, 1:5]
colnames(dat) <- c("b_l", "b_d", "f_l", "b_m", "species")
X <- dat[, 1:4] %>% scale_sd
Y <- dat$species
bas <- basis_olda(X, Y, 4)

left <- ggtour(bas[, c(3,4)], X, 0) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis() + theme(legend.position = "off")
right <- ggtour(bas[, c(1,2)], X, 0) +
    proto_point(list(color = Y, shape = Y)) +
    proto_basis() + theme(legend.position = "off")
(cp <- cowplot::plot_grid(left, right))

ggsave("./figures/ch1_fig2_penguin_cl_sep.png", cp, device = "png",
       width = 6, height = 2.2, units = "in")


## penguin_gt_filmstrip -----
set.seed(2022)
gt_path <- save_history(X, max_bases = 3)
dim(interpolate(gt_path, angle = .6))
## Stochastic!?? this is supposed to be geodesic interpolation should not vary...

ggt <- ggtour(gt_path, X, .75) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis() + theme(legend.position = "off")
(fs <- filmstrip(ggt, ncol = 3))
ggsave("./figures/ch1_fig3_penguin_gt_filmstrip.png", fs, device = "png",
       width = 6, height = 3.3, units = "in")


