rep(print("THIS WAS INITIAL DOCUMENT AND MAY BE STALE. CHAPTER CODE SHOULD BE TRUSTED OVER THIS."),3)

library(spinifex)
library(ggplot2)
library(gridExtra)
set.seed(1) #don't think this matters, but can't hurt.

# ## FLEA HOLES TOUR
# flea_std <- tourr::rescale(tourr::flea[,1:6])
# hpath    <- tourr::save_history(flea_std, tourr::guided_tour(tourr::holes))
# h_bas    <- matrix(hpath[,,max(dim(hpath)[3])], ncol=2)
#
# #0_left
# step0_l <- view_basis(h_bas) # Maybe manual tour on v1?
#
# h_m_sp <- create_manip_space(h_bas, manip_var = 1)
# h_dat  <- cbind(data.frame(flea_std %*% h_m_sp[, 1:2]), flea$species)
# colnames(h_dat) <- c("x", "y", "species")
#
# #0_right
# step0_r <- ggplot() +
#   geom_point(h_dat, mapping = aes(x=x, y=y, color=species)) +
#   scale_color_brewer(palette = "Dark2") +
#   theme_void() +
#   theme(legend.position=c(0.8, 0.8)) +
#   theme(legend.background = element_rect(colour = 'black', fill = 'grey90', linetype='solid'))
#
#
# #=== Step 0 out
# step0 <- grid.arrange(step0_l, step0_r, ncol=2)
# ggsave("./output/step0_basis+proj.png", step0,
#        height = 4, width = 4*1.61, units = "in")

#=== Step 2 out
step2 <- view_manip_sp(h_bas, 5)
ggsave("./output/step2_manip_sp.png", step2,
       width = 4, height = 4, units = "in")


#===
mtour <- manual_tour(h_bas, manip_var = 5, n_slides = 15)
bases <- create_slides(tour = mtour, data = flea_std)
bases <- bases$basis_slides

mag <- 2.2
grid  <- data.frame(slide = 1:15, x = mag*rep(1:5, 3), y = mag*rep(3:1, each = 5))

# Initialize
## manip var asethetics
n_slides         <- max(bases$slide)
p                <- nrow(bases) / n_slides
manip_var        <- 5
col_v            <- rep("grey80", p)
col_v[manip_var] <- "blue"
col_v            <- rep(col_v, n_slides)
siz_v            <- rep(0.3, p)
siz_v[manip_var] <- 1
siz_v            <- rep(siz_v, n_slides)
## circle
angle <- seq(0, 2 * pi, length = 180)
circ  <- data.frame(c_x = cos(angle), c_y = sin(angle))
circ[nrow(circ)+1, ] <- NA

bases_grid <- merge(x = bases, y = grid, by = "slide", all = TRUE) # OUTER JOIN
circ_grid <- merge(x = circ, y = grid, by = NULL) # CROSS JOIN,

#=== Step 3 out
(step3 <- ggplot(data = bases_grid) +
  geom_segment(aes(x = V1+x, y = V2+y, xend = x, yend = y),
               color = col_v, size = siz_v) +
  geom_text(aes(x = V1+x, y = V2+y, label = lab_abbr),
            color = col_v, vjust = "outward", hjust = "outward") +
  geom_path(data = circ_grid, mapping = aes(x = x+c_x, y = y+c_y), color = "grey80") +
  theme_void())
ggsave("./output/step3_manual_tour.png", step3, width = 4*(5/3), height = 4)

