library(spinifex)

# EXAMPLE:
# flea_std <- tourr::rescale(tourr::flea[, 1:6])
# rb <- basis_random(ncol(flea_std), 2)
#
# view_basis3d(rb, 4)

##TODO: Reconfigure for manip space, not basis.
# DevExamp:
basis<-basis_random(ncol(tourr::rescale(tourr::flea[, 1:6])), 2);manip_var<-4;manip_col = "blue";labels = paste0("V", 1:nrow(basis))
view_manip_sp <- funtion(basis,
                         manip_var,

                         manip_col = "blue",
                         labels = paste0("V", 1:nrow(basis)),
                         ...) {

  # Initialize
  m_sp  <- create_manip_space(basis, manip_var)
  angle <- seq(0, 2 * pi, length = 360)
  circ  <- data.frame(x = cos(angle), y = sin(angle))

  # circle and options
  gg1 <-
    ggplot2::ggplot() + ggplot2::geom_path(
      data = circ, color = "grey80", size = .3, inherit.aes = F,
      mapping = ggplot2::aes(x = circ$x, y = circ$y)
    ) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() # Do not use with plotly!

  # Initialize
  p <- nrow(basis)
  basis <- data.frame(basis)
  colnames(basis) <- c("x", "y")
  # Size and color of manip_var
  col_v            <- rep("black", p)
  col_v[manip_var] <- manip_col
  siz_v            <- rep(0.3, p)
  siz_v[manip_var] <- 1

  # Plot refrence frame axes
  gg2 <- gg1 +
     ggplot2::geom_segment(
      data = basis, size = siz_v, colour = col_v,
      mapping = ggplot2::aes(x = x, y = y, xend = 0, yend = 0)
    )


  # Reference frame text
  gg3 <- gg2 + ggplot2::geom_text(
    data = basis, size = 4, hjust = 0, vjust = 0, colour = "black",
    mapping = ggplot2::aes(x = x, y = y, label = labels)
  )

  gg3
}