# Setup ------
if(F)
  remotes::install_github("nspyrison/spinifex")
require(spinifex) ## Current dev ver, likely a small diff from CRAN spinifex v0.3.1
require(tourr)
require(ggplot2)
require(GGally)
require(dplyr)
this_theme <- list(
  scale_color_brewer(palette = "Dark2"),
  theme_void(),
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "off"),
  coord_fixed(),
  labs(x = "", y = "")
)
.u = "in"
.w = 6.25
.h = 9



# fig1_pca_scatterplotmatrix -----
## TODO: This is already covered in the introduction/motivation
if(F){ ## Not run!!
  source("./figures_from_script/ch4_util_funcs.r")
  if(F)
    file.edit("./figures_from_script/ch4_util_funcs.r")

  tgt_fp <- paste0("./data/EEV_p6_0_1_rep3.rda")
  load(tgt_fp, envir = globalenv())
  dat <- EEV_p6_0_1_rep3
  clas <- as.factor(attr(dat, "cluster"))

  # str(dat)
  # str(clas)
  pca_obj <- #prcomp(dat)
    dat %>% scale_01() %>% prcomp()
  pca_proj1_3 <- as.data.frame(
    cbind(pca_obj$x[, 1:3],
          cluster = as.factor(clas)))

  gg_pca <- GGally::ggpairs(
    pca_proj1_3,
    mapping = aes(color = clas, fill = clas, shape = clas),
    columns = 1:3,
    #diag = "blank",
    upper = "blank",
    lower = list(continuous = GGally::wrap("points", alpha = 0.7, size=1)),
    columnLabels = paste0("PC", 1:3)) +
    theme_bw() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank())

  if(F)
    ggsave(
      "./figures_from_script/ch4_fig1_pca_splom.pdf", gg_pca, device = "pdf",
      width = .w / 2, height = .w / 2, units = .u)
}



# fig2_exp_factors -----
require("ggforce")
require("ggplot2")
require("ggExtra")
require("magrittr")
require("spinifex")
palette(RColorBrewer::brewer.pal(8, "Dark2"))
this_theme <- list(
  theme_bw(),
  scale_color_manual(values = palette()[1:8]),
  scale_fill_manual( values = palette()[1:8]),
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "off",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
)

load("./data/EEE_p4_0_1_rep1.rda") ## In global env, obj EEE_p4_0_1_rep1
clas <- attr(EEE_p4_0_1_rep1, "cluster")

## Factors -----
bas1 <- spinifex::basis_pca(EEE_p4_0_1_rep1)
gt <- tourr::save_history(EEE_p4_0_1_rep1, tour_path = grand_tour(), max_bases = 1)
bas2 <- matrix(gt[[1]], nrow=4, ncol=2)
bas3_st <- basis_half_circle(EEE_p4_0_1_rep1)
mt <- manual_tour(bas3_st, manip_var = 2)
bas3 <- spinifex:::interpolate_manual_tour(mt, .05)[,,17]

fct1 <- spinifex::ggtour(bas1, EEE_p4_0_1_rep1) +
  proto_basis() +
  this_theme +
  labs(x = expression(paste(PC_j)), y = expression(paste(PC_k)),
       subtitle = "PCA \n\n Discrete jump to \n selected pair")
fct2 <- spinifex::ggtour(bas2, EEE_p4_0_1_rep1) +
  proto_basis() +
  this_theme +
  labs(subtitle = "grand tour \n\n Animation through \n random bases")
attr(bas3, "manip_var") <- 2
fct3 <- spinifex::ggtour(bas3, EEE_p4_0_1_rep1) +
  proto_basis() +
  this_theme +
  labs(subtitle = "radial tour \n\n Animation changing \n contribution of \n selected variable")

## Locations ------
##     Cluster A         Cluster B
x <- c(rnorm(140, 0, 1), rnorm(140, 2, 1)) ## signal
y <- c(rnorm(140, 0, 1), rnorm(140, 0, 1)) ## noise
location_df <- data.frame( ## angles are 0, 30, 45 respectively
  name = factor(rep(c("0/100%", "33/66%", "50/50%"), each = 2 * 140),
                levels = c("0/100%", "33/66%", "50/50%")),
  cluster = as.factor(rep(rep(c("a", "b"), each = 140), times = 3)),
  signal = c(cos(0)*x + sin(0)*y, cos(pi/6)*x + sin(pi/6)*y, cos(pi/4)*x + sin(pi/4)*y),
  noise  = c(-sin(0)*x + cos(0)*y, -sin(pi/6)*x + cos(pi/6)*y, -sin(pi/4)*x + cos(pi/4)*y)
)
.rang <- range(location_df$signal)
lvls <- levels(location_df$name)
x_nms <- c("1*V1 + 0*V2 \n (signal & noise respectively)", ".866*V1 + .5*V2", ".7071*V1 + .7071*V2")
# y_nms <- c("V2 (noise)", "-sin(30)*V1 + cos(30)*V2", "-sin(45)*V1 + cos(45)*V2")
for(i in 1:length(lvls)){ ## Creates obj: loc1:loc3
  g <- location_df[location_df$name==lvls[i], ] %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype=1) +
    geom_vline(xintercept = 2, linetype=2) +
    geom_density(aes(signal, fill = cluster), alpha = .5) +
    this_theme +
    theme(axis.title =  element_text()) +
    ggplot2::labs(x = x_nms[i], y = "") +
    labs(subtitle = lvls[i]) +
    xlim(.rang)
  assign(paste0("loc", i), g, envir = globalenv())
}


## Shapes ------
## EEE, EEV, EVV*
shape_df <- data.frame(
  name = factor(c(rep(c("EEE", "EEV"), each = 3), rep("EVV, banana transformed", 7)),
                levels = c("EEE", "EEV", "EVV, banana transformed")),
  cluster = as.factor(c(rep(c("a", "b", "c"), 3), rep("b", 4))),
  x = c(rep(c(-1, 1, -1), 3),              .5,   0,  .5,  0),
  y = c(rep(c(-1, -1, 1), 3),              -1.5, -2, -.5, 0),
  a = c(rep(1, 6),  rep(c(.5, .4, 1), 1),  rep(.4, 4)),
  b = c(rep(.5, 6), rep(c(.5, .4, .5), 1), rep(.4, 4)),
  angle = c(rep(pi / 4, 3),           ## EEE
            rep(pi / 4, 2), -pi / 4,  ## EEV
            0, 0, -pi / 4, rep(0, 4)) ## EVV_banana
)
clust_d <- data.frame(
  name = c("EEI", "EEI", "EVI"),
  cluster = rep("(d)", 3),
  x = rep(-1, 3),
  y = rep(-1, 3),
  a = rep(.425, 3),
  b = rep(.425, 3),
  angle = rep(0, 3)
)
lvls <- levels(shape_df$name)
for(i in 1:length(lvls)){ ## Creates obj; shp1:shp3
  g <- shape_df[shape_df$name == lvls[i],] %>%
    ggplot() +
    ## Clusters a:c
    ggforce::geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                              angle = angle, color = cluster), size = 1) +
    ## Cluster d
    ggforce::geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                              angle = angle, color = cluster),
                          data = clust_d[i, ],
                          size = .6, linetype = 2, alpha = .5) +
    coord_fixed() +
    this_theme +
    labs(subtitle = lvls[i])
  ## Add text on first, but not the last two
  if(i == 1)
    g <- g +
      ## Cluster letters a-c
      geom_text(aes(x = x, y = y, label = cluster, color = cluster), size = 7) +
      ## Cluster letter d
      geom_text(aes(x = x - .5, y = y + .5, color = cluster),
                data = clust_d[i, ], size = 4, alpha =.7,
                label = "(d)")
  assign(paste0("shp", i), g, envir = globalenv())
}
shp2


## Dim ------
load("./data/EEE_p4_0_1_rep1.rda") ## load obj EEE_p4_0_1_rep1
load("./data/EEE_p6_0_1_rep1.rda") ## load obj EEE_p5_0_1_rep1
bas4 <- spinifex::basis_half_circle(EEE_p4_0_1_rep1)
bas6 <- spinifex::basis_half_circle(EEE_p6_0_1_rep1)
clas4 <- attr(EEE_p4_0_1_rep1, "cluster")
clas6 <- attr(EEE_p6_0_1_rep1, "cluster")
dim4 <- spinifex::ggtour(bas4, EEE_p4_0_1_rep1) +
  proto_basis() +
  this_theme +
  labs(subtitle = "3 cluster in 4 dim")
dim6 <- spinifex::ggtour(bas6, EEE_p6_0_1_rep1) +
  proto_basis() +
  this_theme +
  labs(subtitle = "4 cluster in 6 dim")
dim_txt <- ggplot() +
  geom_text(aes(0, 0), size = 3,
            label = "Cluster 'd', above, only exists \n when there are 6 dim, is \n spherical like the noise dim, \n but has cluster separation \n behind the plane of the \n other 3 isodensities.") +
  theme_void()+
  theme(text = element_text(hjust = .5, vjust = .5))

## Cowplot munging ------
require("cowplot")
.gg_empty <- ggplot() + theme_void()
fct_row <- plot_grid(fct1, fct2, fct3, nrow = 1)
loc_row <- plot_grid(loc1, loc2, loc3, nrow = 1)
shp_row <- plot_grid(shp1, shp2, shp3, nrow = 1)
dim_row <- plot_grid(dim4, dim6, dim_txt, nrow = 1,
                     rel_widths=rep(1,3), rel_heights=rep(1,3))
gc()
gg_matrix <- plot_grid(fct_row, loc_row, shp_row, dim_row, ncol = 1, rel_heights = c(1,.8,.8,1))

header_row <- ggplot() +
  labs(title = "Levels of the block") +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))
header_matrix <- plot_grid(header_row, gg_matrix, ncol = 1, rel_heights = c(0.03, 1))

t_fct <- ggplot() +
  labs(title = "factor") +
  theme_void() + labs(x="", y="") +
  theme(plot.title = element_text(angle = 90))
t_loc <- ggplot() +
  labs(title = "location") +
  theme_void() + labs(x="", y="") +
  theme(plot.title = element_text(angle = 90))
t_shp <- ggplot() +
  labs(title = "shape") +
  theme_void() + labs(x="", y="") +
  theme(plot.title = element_text(angle = 90))
t_dim <- ggplot() +
  labs(title = "dimension") +
  theme_void() + labs(x="", y="") +
  theme(plot.title = element_text(angle = 90))
tbl_col <- plot_grid(.gg_empty, t_fct, t_loc, t_shp, t_dim, ncol = 1, rel_heights = c(.8, 1.2,1.2,1,1))

final <- plot_grid(tbl_col, header_matrix, nrow = 1, rel_widths = c(0.05, 1))
.w = 6.25; .h = 9; .u = "in"; ## Save as previous

ggsave(
  "./figures_from_script/ch4_fig2_exp_factors.pdf", final, device = "pdf",
  width = .w, height = .h, units = .u)

## save for project 2
if(F)
  ggsave(
    "../spinifex_study/paper/figures/figExpFactors.pdf", final, device = "pdf",
    width = .w, height = .h, units = .u)


# fig3_accuracy_measure -----
tgt_fp <- paste0("./data/EEV_p6_33_66_rep2.rda")
## Make data plot
load(tgt_fp, envir = globalenv())
dat <- EEV_p6_33_66_rep2
clas <- attr(dat, "cluster")
source("./figures_from_script/ch4_util_funcs.r") ## Redundant
if(F)
  file.edit("./figures_from_script/ch4_util_funcs.r")

## Biplot -----
gg1 <- ggplot() + theme_bw()+
  ggproto_pca_biplot(dat, aes_clas = clas, x_pc_num = 1L, y_pc_num = 4L) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "factor=PCA, location=33/66%, \n shape=EEV, dimension=6&4 clusters",
       x = "PC1", y = "PC4")

## Accuracy measure -----
ans_tbl <- readRDS("./data/ans_tbl.rds") ## load obj ans_tbl
tgt_sim_nm <- "EEV_p6_33_66_rep2"
sub <- ans_tbl %>% dplyr::filter(sim_nm == tgt_sim_nm)
sub_longer <- pivot_longer_resp_ans_tbl(dat = sub)
sub_longer$weight[c(3,5)] <- sub_longer$weight[c(3,5)] / sum(sub_longer$weight[c(3,5)])
sub_longer$weight[c(-3,-5)] <- -sub_longer$weight[c(-3,-5)] / sum(sub_longer$weight[c(-3,-5)])

gg2 <- ggplot() + theme_bw() +
  ggproto_ans_plot(sub_longer) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "Cluster separation & weights") +
  theme(legend.position = "off")

(final <- cowplot::plot_grid(gg1, gg2 , scale = c(1, 1)))
.w = 6.25; .h = 9; .u = "in"; ## Save as previous
ggsave(
  "./figures_from_script/ch4_fig3_accuracy_measure.pdf", final, "pdf",
  width = .w, height = .w / 2, units = .u)

# ch4_fig4_randomization_MANUAL.png -----
# SEE: C:\Users\spyri\Documents\R\
### spinifex_study\paper\figures\figParmeterizationExample.png
# c+p to ./figures_from_script/
### ch4_fig4_randomization_MANUAL.png

# ch4_tab1_model_comparisons -----
## Handled inline, see `04-efficacy_radial_tour.Rmd`

# ch4_tab2_model_coefficients -----
## Handled inline, see `04-efficacy_radial_tour.Rmd`

# ch4_fig5_subjestive_measures -----
# ... not yet done? manually transfer?

# ch4_fig6_rand_effect_size ----
# include? I think it really supports the idea of using mixxed models.


