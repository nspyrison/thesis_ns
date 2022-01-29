## Setup -----
{
require(ggplot2)
require(GGally)
require(spinifex)
dat <- spinifex::penguins_na.rm[, 1:5]
colnames(dat) <- c("bill length", "bill depth", "flipper length", "body mass", "species")
}

## Penguins SPLOM -----
X <- dat[, 1:4]
Y <- dat$species
gg <- GGally::ggpairs(X, upper = "blank",
                mapping = ggplot2::aes(color = Y, shape = Y)) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

ggsave("./figures/ch2_fig1_penguin_splom.pdf", gg, device = "pdf",
       width = 4.2, height = 4.2, units = "in")

## Penguins PCP -----
gg2 <- GGally::ggparcoord(
  dat, columns = c(1:4), groupColumn = 5) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x="", y="") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave("./figures/ch2_fig2_penguin_pcp.pdf", gg2, device = "pdf",
       width = 6, height = 4, units = "in")


## Setup2 -----
{
  require(ggplot2)
  require(spinifex)

  dat <- spinifex::penguins_na.rm[, 1:5]
  colnames(dat) <- c("b_l", "b_d", "f_l", "b_m", "species")
  X <- dat[, 1:4] %>% scale_sd
  Y <- dat$species
  bas <- basis_olda(X, Y, 4)
}


## penguin_gt_filmstrip -----
set.seed(2022) ## doesnt seem to impact tourr::interpolate, but wishful thinking
gt_path <- save_history(X, max_bases = 10)
dim(interpolate(gt_path, angle = .6))
## Stochastic!?? this is supposed to be geodesic interpolation should not vary...

ggt <- ggtour(gt_path[,,1:3], X, .75) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) + theme(legend.position = "off")
(fs <- filmstrip(ggt, ncol = 3))
ggsave("./figures/ch2_fig3_penguin_grandtour.png", fs, device = "png",
       width = 6, height = 3, units = "in")

## Save a video
animate_gganimate(
  ggtour(gt_path, X, .15) +
    proto_point(list(color = Y, shape = Y)) +
    proto_basis(line_size = .6) + theme(legend.position = "off"),
  height = 4, width = 6 , units = "in",
  res = 200, ## resolution, pixels per dimension unit I think
  renderer = gganimate::av_renderer("./figures/ch2_fig3_penguin_grandtour.mp4")) ## Alternative render

