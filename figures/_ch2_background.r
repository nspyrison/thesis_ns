## Setup -----
{
  require(ggplot2)
  require(GGally)
  require(spinifex)
  require(datasauRus)
  require(dplyr)

  dat <- spinifex::penguins_na.rm[, 1:5]
  colnames(dat) <- c("bill length", "bill depth", "flipper length", "body mass", "species")
}

## datasauRus ----
dino <- datasauRus::datasaurus_dozen %>%
  filter(dataset != "away") %>%
  mutate(
  dataset = gsub("_", " ", dataset),
  dataset = factor(dataset, unique(dataset))
)
(g <- ggplot(dino, aes(x, y)) +
  geom_point(size = .8) +
  facet_wrap(~dataset, nrow = 2) +
  labs(x=element_blank(), y=element_blank()) + theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.border  = element_rect(size = .4, color = "grey60", fill = NA),
          legend.margin = margin(0,0,0,0),
          plot.margin   = margin(0,0,0,0)
    ) +
    coord_fixed()
)
ggsave("./figures/ch2_fig1_datasaurus.pdf", g, device = "pdf",
       width = 6, height = 3, units = "in")


## Penguins SPLOM -----
X  <- dat[, 1:4]
Y  <- dat$species
gg <- GGally::ggpairs(X, upper = "blank",
                      mapping = ggplot2::aes(color = Y, shape = Y)) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
ggsave("./figures/ch2_fig2_penguin_splom.pdf", gg, device = "pdf",
       width = 4.2, height = 4.2, units = "in")

## Penguins PCP -----
gg2 <- GGally::ggparcoord(
  dat, columns = c(1:4), groupColumn = 5) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x="", y="") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggsave("./figures/ch2_fig3_penguin_pcp.pdf", gg2, device = "pdf",
       width = 6, height = 4, units = "in")


## Setup2 -----
{
  require(ggplot2)
  require(spinifex)
  require(patchwork)

  dat <- spinifex::penguins_na.rm[, 1:5]
  colnames(dat) <- c("bl", "bd", "fl", "bm", "species")
  X   <- scale_sd(dat[, 1:4])
  Y   <- dat$species
  bas <- basis_olda(X, Y, 4)
  .t  <- list(
    theme(panel.border  = element_rect(size = .4, color = "grey60", fill = NA),
          legend.margin = margin(0,0,0,0),
          plot.margin   = margin(0,0,0,0)),
    lims(x = c(-.66, 1.05), y = c(0, 1))
  )
}


## penguin_grandtour -----
set.seed(2022) ## does not seem to impact tourr::interpolate, but wishful thinking
gt_path <- save_history(X, max_bases = 30)
dim(interpolate(gt_path, angle = .6))
## Stochastic!?? this is supposed to be geodesic interpolation should not vary...

ggt <- ggtour(gt_path[,,1:3], X, .75) +
  proto_point(list(color = Y, shape = Y)) +
  proto_basis(line_size = .6) + theme(legend.position = "off")
(fs <- filmstrip(ggt, ncol = 3)) ## ensure 6 frames
ggsave("./figures/ch2_fig4_penguin_grandtour.pdf", fs, device = "pdf",
       width = 6, height = 3, units = "in")

#1280x720 (true 720p)
#720/128=5.625
## Save a video
animate_gganimate(
  ggtour(gt_path, X, .2) +
    proto_point(list(color = Y, shape = Y)) +
    proto_basis(line_size = .6) +
    theme(legend.position = "off") +
    proto_origin(),
  width = 10, height = 5.625, units = "in",
  res = 128, ## resolution, pixels per dimension unit
  renderer = gganimate::av_renderer("./figures/ch2_fig4_penguin_grandtour.mp4")) ## Alternative render
beepr::beep(4)
## Doesn't seems to extend past 12 sec... don't want to push fps/angle may be too much.


## penguin_mt varying geom -----
bas      <- basis_olda(X, Y)
mt_path  <- manual_tour(bas, 1, data = X)

interp   <- spinifex:::interpolate_manual_tour(mt_path, angle = .3)
bas_full <- interp[,,2]
bas_half <- interp[,,8]
bas_zero <- interp[,,10]
attr(bas_full, "manip_var") <-
  attr(bas_half, "manip_var") <-
  attr(bas_zero, "manip_var") <- attributes(mt_path)$manip_var

## proto_point
full_pt <- ggtour(bas_full, X, .3) +
  proto_point(list(color = Y, shape = Y), list(size = .6)) +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off") + ylab("Points")
half_pt <- ggtour(bas_half, X, .3) +
  proto_point(list(color = Y, shape = Y), list(size = .6)) +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off")
zero_pt <- ggtour(bas_zero, X, .3) +
  proto_point(list(color = Y, shape = Y), list(size = .6)) +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off")

## proto_density2d (density contours)
full_cont <- ggtour(bas_full, X, .3) +
  proto_density2d(list(color = Y), list(bins = 5, size = .4)) +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off") + ylab("Density\ncontours")
half_cont <- ggtour(bas_half, X, .3) +
  proto_density2d(list(color = Y), list(bins = 5, size = .4)) +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off")
zero_cont <- ggtour(bas_zero, X, .3) +
  proto_density2d(list(color = Y), list(bins = 5, size = .4)) +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off")

## proto_hex
full_hex <- ggtour(bas_full, X, .3) +
  proto_hex() +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off") + ylab("Hexagonal\nheatmap")
half_hex <- ggtour(bas_half, X, .3) +
  proto_hex() +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off")
zero_hex <- ggtour(bas_zero, X, .3) +
  proto_hex() +
  proto_basis(line_size = .6) + proto_origin() + .t +
  theme(legend.position = "off")

(pw <- (full_pt + half_pt + zero_pt) /
    (full_cont + half_cont + zero_cont) /
    (full_hex + half_hex + zero_hex) + plot_layout(widths = rep(1, 9))
)
#6/ 1.7/1.05
## exp 3.4 inch height (ignoring whitespace)
ggsave("./figures/ch2_fig5_penguin_manualtour_geoms.pdf", pw, device = "pdf",
       width = 6, height = 3.6, units = "in")

