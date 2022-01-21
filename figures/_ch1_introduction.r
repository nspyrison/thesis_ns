require(ggplot2)
require(spinifex)

dat <- spinifex::penguins_na.rm[, 1:5]
colnames(dat) <- c("bill length", "bill depth", "flipper length", "body mass", "species")
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

ggsave("./figures/ch1_fig2_penguin_cl_sep.pdf", cp, device = "pdf",
       width = 6, height = 2.2, units = "in")
