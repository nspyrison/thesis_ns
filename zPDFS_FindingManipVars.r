library(spinifex)
library(ggplot2)
set.seed(1)

## FUNCTION FOR STATIC OUTPPUT, for chap3 use to clean up code chunks.
interpolate2static <- function(.m_tour, .data, .m_var, .cat, .mag =2.4, .angle)
{
  slides <- interpolate2df(array = .m_tour, data = .data, angle = .angle)
  basis_slides <- slides$basis_slides
  data_slides  <- slides$data_slides

  # Initialize
  ## manip var asethetics
  n_slides      <- max(basis_slides$slide)
  p             <- nrow(basis_slides) / n_slides
  col_v         <- rep("grey80", p)
  col_v[.m_var] <- "blue"
  col_v         <- rep(col_v, n_slides)
  siz_v         <- rep(0.3, p)
  siz_v[.m_var] <- 1
  siz_v         <- rep(siz_v, n_slides)
  ## circle
  angle <- seq(0, 2 * pi, length = 180)
  circ  <- data.frame(c_x = cos(angle), c_y = sin(angle))
  circ[nrow(circ)+1, ] <- NA
  ## data asethetics
  data_slides <- data.frame(data_slides, cat = rep(.cat, n_slides))

  grid_b <- grid_t <-
    data.frame(slide = 1:n_slides,
               x = .mag*rep(1:5, 3), y = .mag*rep(3:1, each = 5))
  grid_t$y <- grid_t$y + max(grid_t$y)
  # OUTER JOIN
  basis_grid <- merge(x = basis_slides, y = grid_t, by = "slide", all = TRUE)
  # CROSS JOIN
  circ_grid  <- merge(x = circ, y = grid_t, by = NULL)
  # OUTER JOIN
  data_grid  <- merge(x = data_slides, y = grid_b, by = "slide", all = TRUE)

  # Grpahics
  gg1 <-
    ggplot(data = basis_grid) +
    # AXES LINE SEGMETNS
    geom_segment(aes(x = V1 + x, y = V2 + y, xend = x, yend = y),
                 color = col_v, size = siz_v) +
    # AXES TEXT LABELS
    geom_text(aes(x = V1 + x, y = V2 + y, label = lab_abbr),
              color = col_v, vjust = "outward", hjust = "outward") +
    # AXES FRAME NUM
    geom_text(aes(x = x - .7, y = y + 1.1, label = paste0("frame: ",slide)),
              color = "grey50") +
    # AXES CIRCLE PATH
    geom_path(data = circ_grid, color = "grey80",
              mapping = aes(x = x+c_x, y = y+c_y))

  #===
  gg2 <- gg1 +
    # PROJ DATA POINTS
    geom_point(data = data_grid, size = .2,
               mapping = aes(x = V1 + x, y = V2 + y, color = cat, pch = cat)) +
    # PROJ DATA FRAME NUM
    geom_text(data = data_grid,    color = "grey50",
              mapping = aes(x = x - .7, y = y + 1.1,
                            label = paste0("frame: ",slide))) +
    theme_void() +
    scale_color_brewer(palette = "Dark2") +
    # + coord_fixed()
    theme(legend.position="none",
          panel.border = element_rect(colour = "black", fill = NA))

  # output
  gg2
}


# FIG 7 START ===========================
load("./data/jetsProj_sub.rda")
load("./data/PDFSense_fig7_basis.rda")

PDF7_bas <- fig7_basis
PDF7_dat <- tourr::rescale(jetsProj_sub[, 1:4])
PDF7_cat <- jetsProj_sub$exp
PDF7_m_var_good <- 3
PDF7_m_var_bad <- 4
PDF7_mtour <- manual_tour(basis = PDF7_bas, manip_var = PDF7_m_var)
# play_manual_tour(PDF7_dat, PDF7_bas, 1, angle = .28, axes="bottomleft")

# interpolate2static(.m_tour = PDF7_mtour, .data = PDF7_dat, .m_var = PDF7_m_var_good,
#                    .angle = .28, .cat = PDF7_cat, .mag = 2.4)
# interpolate2static(.m_tour = PDF7_mtour, .data = PDF7_dat, .m_var = PDF7_m_var_bad,
#                    .angle = .28, .cat = PDF7_cat, .mag = 2.4)

# all 4 axes
play_manual_tour(PDF7_dat, PDF7_bas, 1, angle = .28, axes="bottomleft", cat_var = PDF7_cat)
play_manual_tour(PDF7_dat, PDF7_bas, 2, angle = .28, axes="bottomleft", cat_var = PDF7_cat)
play_manual_tour(PDF7_dat, PDF7_bas, 3, angle = .28, axes="bottomleft", cat_var = PDF7_cat)
play_manual_tour(PDF7_dat, PDF7_bas, 4, angle = .28, axes="bottomleft", cat_var = PDF7_cat)

print("!!!!USE 3 for best example, and 4 for worst.!!!!")
##M_VAR, RESULT
# 1, good
# 2, poor
# 3, best
# 4, worst


# FIG 8 START =====================

load("./data/grDIScenter.rda")
load("./data/PDFSense_fig8l_basis.rda")

PDF8_bas <- fig8l_basis
PDF8_dat <- tourr::rescale(grDIScenter[, 1:6])
PDF8_cat <- factor(grDIScenter$disID)
PDF8_m_var <- 6
PDF8_mtour <- manual_tour(basis = PDF8_bas, manip_var = PDF8_m_var)

view_basis(PDF8_bas)
play_manual_tour(PDF8_dat, PDF8_bas, 1, angle = .28, axes="center", cat_var = PDF8_cat)
play_manual_tour(PDF8_dat, PDF8_bas, 2, angle = .28, axes="bottomleft", cat_var = PDF8_cat)
play_manual_tour(PDF8_dat, PDF8_bas, 3, angle = .28, axes="bottomleft", cat_var = PDF8_cat)
play_manual_tour(PDF8_dat, PDF8_bas, 4, angle = .28, axes="bottomleft", cat_var = PDF8_cat)
play_manual_tour(PDF8_dat, PDF8_bas, 5, angle = .28, axes="bottomleft", cat_var = PDF8_cat)
play_manual_tour(PDF8_dat, PDF8_bas, 6, angle = .28, axes="bottomleft", cat_var = PDF8_cat)

print("!!!!USE 6 for best example, and 2 for worst.!!!!")
##M_VAR, RESULT
# 1, green jet.
# 2, poor
# 3, black
# 4, plane
# 5, black and plane
# 6, green and plane