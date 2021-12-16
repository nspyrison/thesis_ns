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
  source("./figures/ch4_util_funcs.r")
  if(F)
    file.edit("./figures/ch4_util_funcs.r")

  tgt_fp <- paste0("./data/EEV_p6_0_1_rep3.rda")
  load(tgt_fp, envir = globalenv())
  dat <- EEV_p6_0_1_rep3
  clas <- as.factor(attr(dat, "cluster"))

  pca_obj <-
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

  ggsave(
    "./figures/ch4_fig1_pca_splom.pdf", gg_pca, device = "pdf",
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
  "./figures/ch4_fig2_exp_factors.pdf", final, device = "pdf",
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
source("./figures/ch4_util_funcs.r") ## Redundant
if(F)
  file.edit("./figures/ch4_util_funcs.r")

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
  "./figures/ch4_fig3_accuracy_measure.pdf", final, "pdf",
  width = .w, height = .w / 2, units = .u)

# ch4_fig4_randomization_MANUAL.png -----
# SEE: C:\Users\spyri\Documents\R\
### spinifex_study\paper\figures\figParmeterizationExample.png
# c+p to ./figures/
### ch4_fig4_randomization_MANUAL.png

# ch4_tab1_model_comparisons -----
## Handled inline, see `04-efficacy_radial_tour.Rmd`

# ch4_tab2_model_coefficients -----
## Handled inline, see `04-efficacy_radial_tour.Rmd`

# Setup2 ----

require("ggpubr")
my_theme <- list(
  theme_bw(),
  scale_color_brewer(palette = "Dark2"),
  scale_fill_brewer(palette = "Dark2"),
  geom_hline(yintercept = 0L),
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(-6))
)
my_ggpubr <- function(
  df, x = "factor", y = "value", title = waiver(), subtitle = waiver(),
  lab.y_coef = "auto"){ ## Alternatively 1 + (n_levels+1)*.1
  ## Find height of global significance test text.
  .x_lvls <- df %>% pull({{x}}) %>% levels()
  .y_range <- diff(range(df[y]))
  .n_lvls <- length(.x_lvls)
  if(lab.y_coef == "auto")
    lab.y_coef = 1 + (.n_lvls+2) *.1
  .lab.y_global <- min(df[y]) + (lab.y_coef) * .y_range
    #(.04 * .y_range) * (1 + .n_lvls) * .y_range + max(df[y])
  my_comparisons <- list(c("pca", "grand"), c("grand", "radial"), c("pca", "radial"))

  ## Plot
  ggviolin(df, x = x, y = y, fill = x, alpha = .6,
           palette = "Dark2", shape = x,
           add = c("mean"), ## Black circle, can change size, but not shape or alpha?
           draw_quantiles = c(.25, .5, .75)) +
    stat_compare_means(method = "wilcox.test",
                       comparisons = my_comparisons,
                       label = "p.signif",
                       hide.ns = TRUE) + ## pairwise test
    # stat_compare_means(label = "p.signif", label.y = .lab.y - .4,
    #                    method = "wilcox.test", ref.group = .x_lvls[1]) + ## Test each lvl w.r.t. first level.
    stat_compare_means( ## Global test
      label.y = .lab.y_global,
      aes(label = paste0("p=", ..p.format..))
    ) + ## custom label
    my_theme +
    ggtitle(title, subtitle)
}
my_ggpubr_facet <- function(..., facet = "measure"){
  facet(my_ggpubr(...), facet.by = facet)
}


# ch4_fig5_ABcd_violins TODO -----
{
  dat_qual <- readRDS("./data/dat_qual.rds")
  .lvls <- c("0/100%", "33/66%", "50/50%")
  dat_qual$location <-
    factor(.lvls[as.integer(dat_qual$location)], levels = .lvls)
  .no_legend <- theme(legend.position = "off")
  .factor <- my_ggpubr(dat_qual, x = "factor", y = "marks") +
    .no_legend
  .location <- my_ggpubr(dat_qual, x = "location", y = "marks") +
    .no_legend
  .shape <- my_ggpubr(dat_qual, x = "shape", y = "marks") +
    .no_legend
  .dim <- my_ggpubr(dat_qual, x = "dim", y = "marks") +
    .no_legend
  .FactorLocation <-
    my_ggpubr_facet(dat_qual, x = "factor", y = "marks",
                    facet = "location") + .no_legend

  title <- cowplot::ggdraw() +
    cowplot::draw_label("Violin plots of the terms in marks^ = \u03b1 * \u03b2 + \u03b3 + \u03b4",
                        x = .5, y = .75, hjust = .5, vjust = 1)
  top <- cowplot::plot_grid(
    .factor, .location, .shape, .dim, nrow = 1)
  gc()
  (ABcd_violins <-
      cowplot::plot_grid(title, top, .FactorLocation + ggtitle("", ""),
                         ncol = 1, rel_heights = c(.1, 1, 1.4)))
}
ggsave("./figures/ch4_fig5_ABcd_violins.pdf",
       ABcd_violins, device = cairo_pdf,
       width = .w, height = .w, unit = .u)
# ch4_figX_demographic_heatmap -----
require("tidyverse")
require("dplyr")
## Load aggregated data. filter to only surveys in the 108 instances in the analysis
survey_wider <- readRDS("./data/survey_wider.rds")
instance_id_whitelist <- readRDS("./data/instance_id_whitelist.rds")
## Only Prolific participants that were in the 108 instance_ids in the analysis
survey_wider <- survey_wider %>%
  dplyr::filter(nchar(as.character(prolific_id)) == 24,
         survey_wider$instance_id %in% instance_id_whitelist)
str(survey_wider)

## Change character to factor, include counts in the levels of sex?
(demographic_heatmap <- ggplot(survey_wider, aes(education, age)) +
    stat_bin2d(aes(fill = after_stat(count))) +
    geom_text(aes(label = after_stat(count)), stat = "bin2d") +
    facet_grid(cols = vars(pronoun), labeller = label_wrap_gen(width=18)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          # legend.position = "bottom",
          # legend.direction = "horizontal",
          legend.margin = margin(0, 0, 0, 0)) +
    scale_fill_gradient(low = "lightpink", high = "firebrick", na.value = NA) +
    ggtitle("Participant demographics"))
if(F)
  ggsave(filename = "./figures/ch4_figX_demographic_heatmap.pdf",
         plot = demographic_heatmap, device = "pdf", width = .w, height = .w/2)

# ch4_fig6_subjective_measures -----
## Follow the loose setup of _analysis.rmd:
.u = "in"
.w = 6.25
.h = 9
.l_lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")

survey_wider <- readRDS("./data/survey_wider.rds")
str(survey_wider)

{ ## Subjective violins munging
  ## pivot_longer within factor
  radial_longer <- survey_wider %>%
    dplyr::select(instance_id, radial_familar:radial_like) %>%
    tidyr::pivot_longer(radial_familar:radial_like,
                        names_to = "factor", values_to = "value")
  grand_longer <- survey_wider %>%
    dplyr::select(instance_id, grand_familar:grand_like) %>%
    tidyr::pivot_longer(grand_familar:grand_like,
                        names_to = "factor", values_to = "value")
  pca_longer <- survey_wider %>%
    dplyr::select(instance_id, pca_familar:pca_like) %>%
    tidyr::pivot_longer(pca_familar:pca_like,
                        names_to = "factor", values_to = "value")
  ## Combine and split measure from factor
  subjective_longer <- rbind(radial_longer, grand_longer, pca_longer) %>%
    tidyr::separate(factor, c("factor", "measure"), sep = "_")
}

{ ## Subjective violins
  .lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")
  subjective_longer <- subjective_longer %>%
    mutate(value = as.integer(plyr::mapvalues(value, from = .lvls, to = 1L:5L)),
           measure = factor(plyr::mapvalues(measure,
                                            from = c("like", "ease", "confidence", "familar"),
                                            to = c("preference", "ease of use", "confidence", "familiarity"))),
           factor = factor(factor, levels = c("pca", "grand", "radial"))
    )
  (subjective_violins <- my_ggpubr_facet(df = subjective_longer, x = "factor", y = "value"))
}

{ ## Subjective violins munging
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
  str(survey_agg)

  ## Format likert questions
  likert_q_nms <- colnames(survey_wider[, 11:22])
  likert <<- survey_agg %>% filter(question %in% likert_q_nms) %>%
    tidyr::separate(question, c("factor", "question"), sep = "_") %>%
    mutate(factor = factor(factor, levels = rev(c("pca", "grand", "radial"))),
           response = factor(response, levels = rev(.l_lvls)))
  likert$question <-
    plyr::mapvalues(likert$question,
                    from = c("like", "ease", "confidence", "familar"),
                    to = c("preference", "ease of use", "confidence", "familiarity")) %>%
    factor()
  str(likert)
}

## Likert plots -----
(subjective_likert <-
    ggplot(likert, aes(x = percent, y = factor, fill = response)) +
    geom_bar(position = "fill", stat = "identity", width = .6) + facet_grid(vars(question)) +
    ggtitle("Subjective measures",
            "Likert scale [1-5]") +
    theme_bw() +
    scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "PRGn"))) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    # Reverse order that fill is displayed in legend.
    guides(fill = guide_legend(reverse = TRUE)) +
    # x as % rather than rate.
    scale_x_continuous(labels = scales::percent)
)

## SAVING ----
## Cowplot and bringing it together
require("cowplot")
figSubjectiveMeasures <-
  cowplot::plot_grid(subjective_likert +
                       coord_flip() +
                       theme(legend.direction = "vertical") +
                       guides(fill = guide_legend(reverse = FALSE)),
                     subjective_violins,
                     ncol = 2)

ggsave("./figures/ch4_fig6_subjective_measures.pdf",
       figSubjectiveMeasures, device = "pdf",
       width = .w, height = .w, units = "in")


# ch4_fig7_rand_effect_size ----
# include? I think it really supports the idea of using mixed models.


