# Make figures
library(spinifex)
library(ggplot2)
set.seed(1) #don't think this matters, but can't hurt.

## STEP 1
flea_std <- tourr::rescale(tourr::flea[,1:6])
tpath    <- tourr::save_history(flea_std, tourr::guided_tour(tourr::holes))
h_bas  <- matrix(tpath[,,max(dim(tpath)[3])], ncol=2)
view_basis(h_bas) # Maybe manual tour on v1?
h_m_sp <- create_manip_space(h_bas, manip_var = 1)

h_dat <- cbind(data.frame(flea_std %*% h_m_sp[, 1:2]), flea$species)
colnames(h_dat) <- c("x", "y", "species")

#===
step1r <- ggplot() +
  geom_point(h_dat, mapping = aes(x=x, y=y, color=species)) +
  scale_color_brewer(palette = "Dark2") +
  theme_void()
#===

angle <- seq(0, 2 * pi, length = 360)
circ <- data.frame(x = cos(angle), y = sin(angle))
gg1 <- ggplot2::ggplot() +
  ggplot2::geom_path(data = circ, color = "grey80", size = 0.3, inherit.aes = F,
                     mapping = ggplot2::aes(x = circ$x, y = circ$y)) +
  ggplot2::scale_color_brewer(palette = "Dark2") +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position = "none")

# Plot refrence frame axes
gg2 <- gg1 +
  ggplot2::geom_segment(
    data = h_m_sp, size = siz_v, colour = col_v,
    mapping = ggplot2::aes(x = basis_slides$V1, y = basis_slides$V2,
                           xend = 0, yend = 0, frame = basis_slides$slide)
  )
)

# Reference frame text
gg3 <- gg2 # + suppressWarnings(ggplot2::geom_text( # for unused aes "frame".
# data = basis_slides, size = 4, hjust = 0, vjust = 0, colour = "black",
# mapping = ggplot2::aes(x = basis_slides$V1, y = basis_slides$V2,
#                        frame = basis_slides$slide, label = lab_abbr)
# ))
