# Setup ------
{
  require(spinifex) ## Current dev ver, likely a small diff from CRAN spinifex v0.3.1
  require(tourr)
  require(ggplot2)
  require(GGally)
  require(dplyr)
  require(patchwork)
  my_theme <- list(
    scale_color_brewer(palette = "Dark2"),
    theme_void(),
    theme(axis.title      = element_text(),
          plot.title      = element_text(hjust = 0.5),
          plot.subtitle   = element_text(hjust = 0.5),
          legend.position = "off"),
    coord_fixed(),
    labs(x = "", y = "")
  )
  .u = "in"
  .w = 6.25
  .h = 9
}


# fig_Cl_Sep -----
{
  set.seed(2022)
  .n <- 100
  cl1  <- data.frame(
    V1 = rnorm(n = .n, mean = 0, sd = 1),
    V2 = rnorm(n = .n, mean = 0, sd = 1),
    V3 = rnorm(n = .n, mean = 0, sd = 1),
    v4 = rnorm(n = .n, mean = 0, sd = 1)
  )
  cl2  <- data.frame(
    V1 = rnorm(n = .n, mean = 0, sd = 1),
    V2 = rnorm(n = .n, mean = 4, sd = 1),
    V3 = rnorm(n = .n, mean = 0, sd = 3),
    v4 = rnorm(n = .n, mean = 0, sd = 1)
  )
  dat2  <- rbind(cl1, cl2)
  clas2 <- rep(c("A", "B"), each = .n)
  bas2  <- basis_olda(dat2, clas2, d = 4)

  set.seed(2022)
  rand <- tourr::basis_random(4, 2)
  rand[2,] <- rand[2, ] / 20
  rand <- tourr::orthonormalise(rand)
}

##4 plot and save
(ClSep1 <- ggtour(bas2[, c(3, 4)], dat2) +
    proto_basis() +
    proto_point(aes_args = list(color = clas2, shape = clas2)) +
    theme(plot.title    = element_text(face = "bold"),
          panel.border  = element_rect(size = .4, color = "grey20", fill = NA),
          panel.spacing = unit(0L, "points")) +
    labs(title = "a", color = "Cluster", shape = "Cluster"))
(ClSep2 <- ggtour(rand, dat2) +
    proto_basis() +
    proto_point(aes_args = list(color = clas2, shape = clas2)) +
    theme(legend.position = "off",
          plot.title    = element_text(face = "bold"),
          panel.border  = element_rect(size = .4, color = "grey20", fill = NA),
          panel.spacing = unit(0L, "points")) +
    labs(title = "b")
)
(ClSep <- ClSep1 + ClSep2)
ggsave("./figures/ch4_fig1_cl_sep.pdf", ClSep,
       device = "pdf", width = 6, height = 2.5, units = "in")


# fig3_exp_factors -----
## Setup 2 -----
{
  library(ggforce)
  library(ggplot2)
  library(ggExtra)
  library(magrittr)
  library(spinifex)
  library(cowplot)
  palette(RColorBrewer::brewer.pal(8, "Dark2"))
  my_theme <- list(
    theme_bw(),
    scale_color_brewer(palette = "Dark2"),
    scale_fill_brewer( palette = "Dark2"),
    labs(x="",y=""), ## clear titles
    #coord_fixed(),
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "off",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  )

  load("./data/EEE_p4_0_1_rep1.rda") ## In global env, obj EEE_p4_0_1_rep1
  clas <- attr(EEE_p4_0_1_rep1, "cluster")
  dat  <- EEE_p4_0_1_rep1
  clas <- attr(EEE_p4_0_1_rep1, "cluster")
  bas1 <- spinifex::basis_pca(EEE_p4_0_1_rep1)
  gt   <- tourr::save_history(EEE_p4_0_1_rep1, tour_path = grand_tour(), max_bases = 1)
  bas2 <- matrix(gt[[1]], nrow=4, ncol=2, dimnames = list(colnames(dat)))
  bas3_st <- basis_half_circle(EEE_p4_0_1_rep1)
  mt   <- manual_tour(bas3_st, manip_var = 2)
  bas3 <- spinifex:::interpolate_manual_tour(mt, .05)[,,17]
  attr(bas3, "manip_var") <- 2
}

## Visual ------
.t  <- c("PCA", "Grand tour", "Radial tour")
.st <- c("Discrete jump to \n selected PC pair",
         "Animation through \n random bases",
         "Animation changing \n the selected variable")
.x  <- c("PC i", "", "")
.y  <- c("PC j", "", "")
.m  <- sapply(1:3, function(i){
  .fct <- spinifex::ggtour(get(paste0("bas", i), envir = globalenv())) +
    proto_basis("center") +
    my_theme +
    labs(x = .x[i], y = .y[i], title = .t[i], subtitle = .st[i])
  assign(paste0("fct", i), .fct, envir = globalenv())
})

## Location ------
### 0/100 33/66, 50/50
# cl1 <- rnorm(140, 0, 1)
# cl2 <- rnorm(140, 2, 1)
##     Cluster A         Cluster B
set.seed(123)
x <- c(rnorm(140, 0, 1), rnorm(140, 2, 1)) ## signal
y <- c(rnorm(140, 0, 1), rnorm(140, 0, 1)) ## noise
location_df <- data.frame( ## angles are 0, 30, 45 respectively
  name    = factor(rep(c("0/100%", "33/66%", "50/50%"), each = 2 * 140),
                   levels = c("0/100%", "33/66%", "50/50%")),
  cluster = as.factor(rep(rep(c("a", "b"), each = 140), times = 3)),
  signal  = c(cos(0)*x + sin(0)*y, cos(pi/6)*x + sin(pi/6)*y, cos(pi/4)*x + sin(pi/4)*y),
  noise   = c(-sin(0)*x + cos(0)*y, -sin(pi/6)*x + cos(pi/6)*y, -sin(pi/4)*x + cos(pi/4)*y)
)
.rang <- range(location_df$signal)
lvls  <- levels(location_df$name)
x_nms <- c("1*V1 + 0*V2", ".866*V1 + .5*V2", ".7071*V1 + .7071*V2")
for(i in 1:length(lvls)){
  g <- location_df[location_df$name==lvls[i], ] %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = 1) +
    geom_vline(xintercept = 2, linetype = 2) +
    geom_density(aes(x=signal, y=..ndensity.., fill = cluster), alpha = .5) +
    my_theme +
    theme(axis.title =  element_text(), aspect.ratio = 1) +
    ggplot2::labs(x = x_nms[i], y = "") +
    labs(subtitle = lvls[i]) +
    xlim(.rang)
  assign(paste0("loc", i), g, envir = globalenv())
}
if(F)
  loc3

## Shape ------
## EEE, EEV, EVV*
shape_df <- data.frame(
  name = factor(c(rep(c("EEE", "EEV"), each = 3), rep("EVV, banana transformed", 7)),
                levels = c("EEE", "EEV", "EVV, banana transformed")),
  cluster = as.factor(c(rep(c("a", "b", "c"), 3), rep("b", 4))),
  x = c(rep(c(-1,  1, -1), 3),   .5,  0,  .5, 0),
  y = c(rep(c(-1, -1,  1), 3), -1.5, -2, -.5, 0),
  a = c(rep( 1, 6), rep(c(.5, .4,  1), 1), rep(.4, 4)),
  b = c(rep(.5, 6), rep(c(.5, .4, .5), 1), rep(.4, 4)),
  angle = c(rep(pi / 4, 3),           ## EEE
            rep(pi / 4, 2), -pi / 4,  ## EEV
            0, 0, -pi / 4, rep(0, 4)) ## EVV_banana
)
ellipse_df <- data.frame(
  name = c("EEI", "EEI", "EVI"),
  cluster = rep("(d)", 3),
  x = rep(-1,   3),
  y = rep(-1,   3),
  a = rep(.425, 3),
  b = rep(.425, 3),
  angle = rep(0, 3)
)
lvls <- levels(shape_df$name)
for(i in 1:length(lvls)){
  g <- shape_df[shape_df$name == lvls[i],] %>%
    ggplot() +
    ## Clusters a:c
    geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                     angle = angle, color = cluster), size = 1) +
    ## Cluster d
    geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                     angle = angle, color = cluster),
                 data = ellipse_df[i, ],
                 size = .6, linetype = 2, alpha = .5) +
    my_theme +
    coord_fixed() +
    xlim(-2.2,2.2) + ylim(-2.4,2) +
    labs(subtitle = lvls[i])
  if(i != length(lvls)) ## Add text on first 2, but not the last one.
    g <- g +
      ## Cluster letters a-c
      geom_text(aes(x = x, y = y, label = cluster, color = cluster), size = 7) +
      ## Cluster letter d
      geom_text(aes(x = x - .5, y = y + .5, color = cluster),
                data = ellipse_df[i, ], size = 4, alpha =.7,
                label = "(d)")
  assign(paste0("shp", i), g, envir = globalenv())
}

## Dim ------
load("./data/EEE_p4_0_1_rep1.rda") ## load obj EEE_p4_0_1_rep1
load("./data/EEE_p6_0_1_rep1.rda") ## load obj EEE_p5_0_1_rep1
str(EEE_p4_0_1_rep1)
bas4  <- spinifex::basis_half_circle(EEE_p4_0_1_rep1)
bas6  <- spinifex::basis_half_circle(EEE_p6_0_1_rep1)
clas4 <- attr(EEE_p4_0_1_rep1, "cluster")
clas6 <- attr(EEE_p6_0_1_rep1, "cluster")
dim4  <- ggtour(bas4) +
  proto_basis() +
  my_theme +
  ggplot2::labs(subtitle = "4 dimensions, 3 clusters")
dim6  <- ggtour(bas6) +
  proto_basis() +
  my_theme +
  ggplot2::labs(subtitle = "6 dimensions, 4 clusters")
## text block about cluster d
dim_txt <- ggplot() +
  geom_text(aes(0, 0), size = 3.3, hjust = .5, vjust = .3,
              label = "Cluster 'd', above, only exists \n when there are six dimensions, \n is spherical, and has cluster \n separation orthogonal to the \n plane of the other three \n isodensities.") +
  theme_void() +
  theme(text = element_text(hjust = .5, vjust = .5))

### Cowplot munging ------
.gg_empty <- ggplot() + theme_void()
fct_row   <- plot_grid(fct1, fct2, fct3,    nrow = 1)
loc_row   <- plot_grid(loc1, loc2, loc3,    nrow = 1)
shp_row   <- plot_grid(shp1, shp2, shp3,    nrow = 1)
dim_row   <- plot_grid(dim4, dim6, dim_txt, nrow = 1)
.m        <- gc()
gg_matrix <- plot_grid(fct_row, loc_row, shp_row,
                       dim_row, ncol = 1, rel_heights = c(1,.8,.8,1))

header_row <- ggplot() +
  labs(title = "Levels of the experimental factors") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 18))
header_matrix <- plot_grid(header_row, gg_matrix,
                           ncol = 1, rel_heights = c(0.05, 1))

t_fct   <- ggplot() +
  labs(title = "Visual") +
  theme_void()+
  theme(plot.title = element_text(angle = 90))
t_loc   <- ggplot() +
  labs(title = "Location") +
  theme_void()+
  theme(plot.title = element_text(angle = 90))
t_shp   <- ggplot() +
  labs(title = "Shape") +
  theme_void()+
  theme(plot.title = element_text(angle = 90))
t_dim   <- ggplot() +
  labs(title = "Dimension") +
  theme_void() +
  theme(plot.title = element_text(angle = 90))
tbl_col <- plot_grid(.gg_empty, t_fct, t_loc, t_shp, t_dim,
                     ncol = 1, rel_heights = c(.8,1.2,1.2,1,1))
final   <- plot_grid(tbl_col, header_matrix, nrow = 1, rel_widths = c(0.05, 1))

ggsave("./figures/ch4_fig3_exp_factors.pdf", final,
       device = "pdf", width = 6.25, height = 9, units = "in")


# fig4_accuracy_measure -----
tgt_sim_nm <- "EEV_p6_33_66_rep2"
tgt_fp <- paste0("./data/", tgt_sim_nm, ".rda")
## Make data plot
load(tgt_fp, envir = globalenv())
dat  <- EEV_p6_33_66_rep2
bas  <- basis_pca(dat, d = 4)[, c(1, 4)]
clas <- attr(dat, "cluster")
source("./figures/ch4_util_funcs.r") ## Ensure pivot_longer_resp_ans_tbl available
if(F)
  file.edit("./figures/ch4_util_funcs.r")

### Left pane ----
proj <- as.matrix(dat) %*% bas %>% as.data.frame()
(gg1 <- ggplot(proj, aes(PC1, PC4)) +
    geom_point(aes(shape = clas, color = clas)) +
    draw_basis(bas, proj) + coord_fixed() +
    theme_void() +
    theme(axis.title = element_text(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "off",
          axis.title.y = element_text(angle = 90)) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer( palette = "Dark2") +
    labs(subtitle = "Visual: PCA, location: 33/66%, \n Shape: EEV, dimension: 6 & 4 clusters",
         x = "PC1", y = "PC4", color = "color", shape = "shape"))


## right pane, w and acc bars ----
ans_tbl    <- readRDS("./data/ans_tbl.rds")
sub        <- ans_tbl %>% dplyr::filter(sim_nm == tgt_sim_nm)
sub_longer <- pivot_longer_resp_ans_tbl(dat = sub)

gg2 <- ggplot() + theme_bw() +
  ggproto_ans_plot(sub_longer) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(y = "Bars: observed cluster separation\nLines: accuracy weights if selected", x = "Variable") +
  theme(legend.position = "off")

(final <- cowplot::plot_grid(gg1, gg2 + theme(aspect.ratio = 8/6), rel_widths = c(1.2, 1)))
.w = 6.25
.h = 9
.u = "in"
ggsave("./figures/ch4_fig4_accuracy_measure.pdf", final, "pdf",
       width = .w, height = .w / 2, units = .u)

# ch4_fig5_randomization_MANUAL.png -----
# SEE: C:\Users\spyri\Documents\R\
### spinifex_study\paper\figures\figParmeterizationExample.png
# c+p to ./figures/
### ch4_fig5_randomization_MANUAL.png

# ch4_tab1_model_comparisons -----
## Handled inline, see `04-efficacy_radial_tour.Rmd`

# ch4_tab2_model_coefficients -----
## Handled inline, see `04-efficacy_radial_tour.Rmd`



# Setup 3 ----
{
  require(ggpubr)
  my_ggpubr <- function(
    df, x = "Visual", y = "Marks",
    title = waiver(), subtitle = waiver(), facet = NULL,
    y_pval_coef = .08, ## Subjective wants .032
    ylim_max_coef = .5 ## Subjective wants .6
  ){
    ## Find height of global significance test text.
    .x_lvls <- df %>% pull({{x}}) %>% levels()
    .y <- df %>% pull({{y}})
    if(is.factor(.y)) .y <- as.integer(.y)
    .y_range <- diff(range(.y))
    .no_x_lvls <- length(.x_lvls)
    if(is.null(facet) == FALSE){
      .facet_lvls <- df %>% pull({{facet}}) %>% levels() %>% length()
    } else .facet_lvls <- 1 ## Init
    .lab.y <- (y_pval_coef * .y_range) * (1 + .no_x_lvls) * .y_range + max(.y)
    my_comparisons <- NULL
    if(.no_x_lvls == 2)
      my_comparisons <- list(c(.x_lvls[1], .x_lvls[2]))
    if(.no_x_lvls == 3)
      my_comparisons <- list(c(.x_lvls[1], .x_lvls[2]),
                             c(.x_lvls[2], .x_lvls[3]),
                             c(.x_lvls[1], .x_lvls[3]))

    ## Plot
    ggviolin(df, x = x, y = y, fill = x, alpha = .6,
             palette = "Dark2", shape = x, trim = TRUE,
             add = c("mean"), ## Black circle, can change size, but not shape or alpha?
             draw_quantiles = c(.25, .5, .75)) +
      stat_compare_means(label.y = .lab.y,
                         aes(label = paste0("p = ", ..p.format..)), ## Global test
                         hide.ns = TRUE) + ## custom label
      stat_compare_means(method = "wilcox.test", ## pairwise test
                         comparisons = my_comparisons,
                         label = "p.signif", hide.ns = TRUE) +
      my_theme +
      coord_cartesian(ylim = c(min(.y), max(.y) + ylim_max_coef * .y_range)) +
      ggtitle(title, subtitle) +
      ggplot2::xlab(paste0(
        x, "\n(n=", nrow(df)/(.no_x_lvls * .facet_lvls), " each)"))
  }
  my_ggpubr_facet <- function(..., facet = "Location"){
    facet(my_ggpubr(..., facet = facet), facet.by = facet)
  }
}


# ch4_fig6_ABcd_violins -----
{
  dat_qual <- readRDS("./data/dat_qual.rds") %>%
    rename(Visual = Factor)
  .lvls <- c("0/100", "33/66", "50/50%")
  dat_qual_loc <- dat_qual %>%
    mutate(Location = factor(.lvls[as.integer(Location)], levels = .lvls))
  levels(dat_qual_loc$Location)

  .lp       <- list(
    theme(legend.position = "bottom", legend.direction = "vertical",
          legend.spacing = unit(0, "npc"),
          #legend.text = element_text(size=6),
          legend.margin = margin(-.05,0,-.01,0, "npc"),
          legend.box.margin = margin(0,0,0,0, "npc"),
          plot.margin = margin(0,0,0,0, "npc")),
    labs(color = "", fill = "")
  )
  .visual   <- my_ggpubr(dat_qual, x = "Visual", y = "Marks") + .lp + ylab("Accuracy")
  .location <- my_ggpubr(dat_qual_loc, x = "Location", y = "Marks") + .lp + ylab("")
  .shape    <- my_ggpubr(dat_qual,     x = "Shape",    y = "Marks") + .lp + ylab("")
  .dim      <- my_ggpubr(dat_qual,     x = "Dim",      y = "Marks") + .lp + ylab("")
  .VisualLocation <- my_ggpubr_facet(
    dat_qual, x = "Visual", y = "Marks", facet = "Location") + ylab("Accuracy") +
    theme(legend.position = "bottom", legend.direction = "horizontal",
          plot.margin = unit(x = c(-.11, 0, 0, 0), units = "npc"))

  title <- cowplot::ggdraw() +
    cowplot::draw_label(
      "Violin plots of the terms for accuracy: Y1^ = \u03b1 * \u03b2 + \u03b3 + \u03b4",
      x = .5, y = .75, hjust = .5, vjust = 1)
  top <- cowplot::plot_grid(.visual, .location, .shape, .dim, nrow = 1)
  require(patchwork)
  top_pw <- .visual + .location + .shape + .dim + plot_layout(nrow = 1)
  gc()
  (violin_ABcd <-
      cowplot::plot_grid(title, top_pw, .VisualLocation + ggtitle("", ""),
                         ncol = 1, rel_heights = c(.15, 1.4, 1.4)))
}
ggsave("./figures/ch4_fig6_ABcd_violins.pdf",
       violin_ABcd, device = cairo_pdf,
       width = .w, height = .w, unit = .u)
ggsave("./figures/ch4_fig6_ABcd_violins.png",
       violin_ABcd, device = "png",
       width = .w, height = .w, unit = .u)


# ch4_fig7_subjective_measures -----
## Follow the loose setup of _analysis.rmd:
.u <- "in"
.w <- 6.25
.h <- 9
.l_lvls      <- c("most disagree", "disagree", "neutral", "agree", "most agree")
survey_wider <- readRDS("./data/survey_wider.rds")
#str(survey_wider)

{
  ## pivot_longer within visual
  radial_longer <- survey_wider %>%
    dplyr::select(instance_id, radial_familar:radial_like) %>%
    tidyr::pivot_longer(radial_familar:radial_like,
                        names_to = "visual", values_to = "value")
  grand_longer <- survey_wider %>%
    dplyr::select(instance_id, grand_familar:grand_like) %>%
    tidyr::pivot_longer(grand_familar:grand_like,
                        names_to = "visual", values_to = "value")
  pca_longer <- survey_wider %>%
    dplyr::select(instance_id, pca_familar:pca_like) %>%
    tidyr::pivot_longer(pca_familar:pca_like,
                        names_to = "visual", values_to = "value")
  ## Combine and split measure from visual
  subjective_longer <- rbind(radial_longer, grand_longer, pca_longer) %>%
    tidyr::separate(visual, c("visual", "measure"), sep = "_")
}
## Technically not continuous numeric, will show side by side with Likert plot.
.lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")
subjective_longer <- subjective_longer %>%
  mutate(value = as.integer(plyr::mapvalues(value, from = .lvls, to = 1L:5L)),
         measure = factor(plyr::mapvalues(
           measure,
           from = c("like", "ease", "confidence", "familar"),
           to = c("preference", "ease of use", "confidence", "familiarity"))),
         visual = factor(visual, levels = c("pca", "grand", "radial"))
  )


{ ## Subjective violins
  .l_lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")
  col_idx <- 8:22
  col_nms <- colnames(survey_wider[, col_idx])
  survey_agg <- tibble()
  mute <- sapply(col_nms, function(col_nm){
    tmp <- survey_wider[col_nm] %>%
      group_by_all() %>%
      count() %>%
      as.data.frame()
    .this_agg <- data.frame(question = col_nm,
                            response = tmp[, 1],
                            n = tmp[, 2],
                            percent = 100 * tmp[, 2] / sum(tmp[, 2]))
    survey_agg <<- rbind(survey_agg, .this_agg)
  })
  #str(survey_agg)

  ## Format likert questions
  likert_q_nms <- colnames(survey_wider[, 11:22])
  likert <<- survey_agg %>% filter(question %in% likert_q_nms) %>%
    tidyr::separate(question, c("visual", "question"), sep = "_") %>%
    mutate(visual = factor(visual, levels = rev(c("pca", "grand", "radial"))),
           response = factor(response, levels = rev(.l_lvls)))
  likert$question <-
    plyr::mapvalues(likert$question,
                    from = c("like", "ease", "confidence", "familar"),
                    to = c("preference", "ease of use", "confidence", "familiarity")) %>%
    factor()
  #str(likert)
}

## Likert plots -----
(subjectiveMeasures <-
   ggplot(likert, aes(x = percent, y = visual, fill = response)) +
   geom_bar(position = "fill", stat = "identity", width = .6) + facet_grid(vars(question)) +
   ggtitle("Subjective measures", "Likert scale [1-5]") +
   theme_bw() +
   scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "PRGn"))) +
   theme(legend.position = "bottom",
         legend.direction = "horizontal") +
   # Reverse order that fill is displayed in legend.
   guides(fill = guide_legend(reverse = TRUE)) +
   # x as % rather than rate.
   scale_x_continuous(labels = scales::percent) +
   coord_flip() +
   theme(legend.direction = "vertical") +
   guides(fill = guide_legend(reverse = FALSE)) +
   labs(x = "Visual", y = "Response rate", fill = "Response")
)

(measure_violins <- my_ggpubr_facet(
  df = subjective_longer, x = "visual", y = "value", facet = "measure",
  y_pval_coef = .032,  ## Subjective wants .032
  ylim_max_coef = .6) +  ## Subjective wants .6
  labs(x = "Visual", y = "Response", fill = "Visual") +
    theme(legend.position = "bottom", legend.direction = "horizontal"))
figSubjectiveMeasures_w.violin_hori <-  cowplot::plot_grid(
  subjectiveMeasures, measure_violins, ncol = 2)

ggsave("./figures/ch4_fig7_subjective_measures.pdf",
       figSubjectiveMeasures_w.violin_hori, device = "pdf",
       width = .w, height = .w, units = "in")


