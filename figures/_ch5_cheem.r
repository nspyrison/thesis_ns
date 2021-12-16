# ch5_fig1_shap_distr_bd ----
require("DALEX")
require("dplyr")
require("ggplot2")

## Local func
my_parts_boxplot_df <- function(pred_parts, player_tag = "<tag unused>"){
  ## Remade from: iBreakDown:::print.break_down_uncertainty
  data.frame(
    player = player_tag,
    label  = tapply(pred_parts$label, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
    variable = tapply(pred_parts$variable_name, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
    value  = tapply(pred_parts$variable_value, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
    ## Of the distribution of local attributions:
    min    = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), min, na.rm = TRUE),
    q1     = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.25, na.rm = TRUE),
    median = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), median, na.rm = TRUE),
    q3     = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.75, na.rm = TRUE),
    max    = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), max, na.rm = TRUE))
}
my_parts_distribution <- function(pred_parts, player_tag = "<tag unused>"){
  df <- data.frame(
    player = player_tag,
    label = pred_parts$label,
    variable = pred_parts$variable_name,
    value = pred_parts$variable_value, ## Obs value of X
    contribution = pred_parts$contribution, ## SHAP contribution
    B_perm_num = pred_parts$B
  )
  rownames(df) <- paste(pred_parts$label, pred_parts$variable, pred_parts$B, sep = ": ")
  return(df)
}
.lvl_ord <- c("reaction", "offense", "movement", "defense", "power", "accuracy", "BMI", "age", "goalkeeping")
my_bd_df <- function(break_down, player_tag = "<tag unused>"){
  df <- data.frame(
    player = player_tag,
    label = break_down$label,
    variable = break_down$variable_name,
    contribution = break_down$contribution, ## SHAP contribution
    cumulative = break_down$cumulative, ## Cumulative SHAP contribution
    sign = break_down$sign
  )
  .n <- nrow(df)
  df$variable[is.na(df$variable)|df$variable==""] <- "prediction"
  df$variable <- factor(
    df$variable, rev(c("intercept", .lvl_ord, "prediction")))
  df$cumulative <- (df$cumulative - min(df$cumulative)) /
    (max(df$cumulative) - min(df$cumulative))
  df$last_cumulative <- c(NA, df$cumulative[-.n])
  df$variable_num <- 1:.n
  df$next_variable_num <- c(2:.n, NA)
  rownames(df) <- paste(break_down$label, break_down$variable, break_down$B, sep = ": ")
  return(df)
}


### Create FIFA x ------
.raw <- DALEX::fifa
.dat_less_ys <- .raw %>%
  dplyr::select(-c(`nationality`, ## useless class
                   `overall`, `potential`, `value_eur`, `wage_eur`)) %>% ## potential target vars.
  as.data.frame()

if(F) ## View corrplot?
  corrplot::corrplot(cor(.dat_less_ys),
                     method = "circle", ## geom
                     type = "upper", ## only upper triangle
                     diag = F, ## remove auto correlation
                     order = "FPC", ## First principal component
                     tl.col = "black", tl.srt = 90, ## Text label color and rotation
                     tl.pos = "td")

## Munging aspects
#### Agg some highly correlated vars.
dat <- .dat_less_ys %>%
  dplyr::mutate(
    .keep = "none",
    BMI = (weight_kg+(height_cm/100L)^2L),
    age = age,
    reaction = movement_reactions,
    offense = (attacking_finishing+skill_long_passing+attacking_volleys+
                 power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
                 attacking_short_passing+skill_dribbling+skill_ball_control)/10L,
    defense = (defending_sliding_tackle+mentality_interceptions+
                 defending_standing_tackle+defending_marking+mentality_aggression)/5L,
    accuracy = (attacking_heading_accuracy+power_shot_power)/2L,
    movement = (movement_sprint_speed+movement_balance+movement_acceleration+
                  mentality_vision+mentality_composure+movement_agility+
                  mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10L,
    power = (power_strength+power_jumping)/2L,
    goalkeeping = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
                     goalkeeping_handling+goalkeeping_kicking)/5L
  )
## Class for the position of the player, eiter "fielder" or "goalkeeper"
position <- clas <- dplyr::case_when(
  dat$gk <= 40L ~ "fielder",
  dat$gk >  40L ~ "goalkeeper") %>%
  factor(levels = c("fielder", "goalkeeper"))

## Starting with 42 variables, we remove `nationality`, and some potential Y vars,
#### and aggregate into 9 aggregate 'aspect' dimensions based on var correlation
X <- dat ## 9 aspects of the X's
Y <- .raw$wage_eur ## unscaled wages in Euros, assumed 2020 valuation.

## Create same RF used downstream -----
.is_y_disc <- FALSE ## regressing on continuous wages
.hp_ntrees <- sqrt(nrow(X))
.hp_mtry <- if(.is_y_disc == TRUE) sqrt(ncol(X)) else ncol(X) / 3L
.hp_node <- if(.is_y_disc == TRUE) 1L else 5L
.hp_node <- max(.hp_node, nrow(X) / 500L)


## NOTE: this DALEX::predict_parts("SHAP")
## NOT theeshap::treeshap()
rf_mod <- randomForest::randomForest(Y~., data = data.frame(Y, X),
                                     mtry = .hp_mtry,
                                     nodesize = .hp_node,
                                     ntrees = .hp_ntrees)
## DALEX parts
rf_expl <- DALEX::explain(model = rf_mod,
                          data  = X,
                          y     = Y,
                          label = "Random Forest")

## SHAP values & plot ----
## Messi SHAP
messi <- X[1, ]
shap_messi <- predict_parts(explainer       = rf_expl,
                            new_observation = messi,
                            type            = "shap",
                            B               = 25L)
shap_messi$contribution <- shap_messi$contribution %>%
  spinifex::scale_01()
box_df_messi <- my_parts_boxplot_df(shap_messi, "Messi (offense)")

## Virgil van Dijk SHAP
dijk <- X[8, ]
shap_dijk <- predict_parts(explainer       = rf_expl,
                           new_observation = dijk,
                           type            = "shap",
                           B               = 25L,
                           order = .lvl_ord)
shap_dijk$contribution <- shap_dijk$contribution %>%
  spinifex::scale_01()
box_df_dijk <- my_parts_boxplot_df(shap_dijk, "van Dijk (defense)")

## Bind shap aggs:
boxplot_df <- rbind(box_df_messi, box_df_dijk)
boxplot_df$variable <- factor(boxplot_df$variable, levels = rev(.lvl_ord))

## B Distributions of the SHAPS
dist_shap_messi <- my_parts_distribution(shap_messi, "Messi (offense)")
dist_shap_dijk <- my_parts_distribution(shap_dijk, "van Dijk (defense)")
dist_df <- rbind(dist_shap_messi, dist_shap_dijk)
dist_df$variable <- factor(dist_df$variable, levels = rev(.lvl_ord))

(g_shap <- ggplot(boxplot_df) +
    # Connecting grey line
    geom_segment(aes(x=`Messi (offense)`, xend=`van Dijk (defense)`, y=variable, yend=variable),
                 alpha =.7, size = 2L, color = "grey", fill = NA,
                 data = boxplot_df %>% select(player, variable, median) %>%
                   tidyr::pivot_wider(names_from = "player", values_from = "median") %>%
                   mutate(sum = `Messi (offense)` + `van Dijk (defense)`)) +
    geom_point(aes(x=median, y=variable, color=player, fill=player),
               alpha =.7, size = 5L) +
    ## Shap distributions
    geom_point(aes(x=contribution, y=variable, color=player, fill=player),
               dist_df, alpha =.8, size = 3, shape = 124,
               position = position_dodge(-.5)) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(title="SHAP distribution",
         y = "Variable", x = "Normalized SHAP values\n the median of 25 permutations of the explanatory variables") +
    theme(legend.position = "off"))

## Breakdowns & plot ----
## Messi Breakdown
bd_messi <- predict_parts(explainer       = rf_expl,
                          new_observation = messi,
                          type            = "break_down",
                          order= .lvl_ord)
bd_df_messi <- my_bd_df(bd_messi, "Messi (offense)")
## Dijk Breakdown
bd_dijk <- predict_parts(explainer       = rf_expl,
                         new_observation = dijk,
                         type            = "break_down",
                         order = .lvl_ord)
bd_df_dijk <- my_bd_df(bd_dijk, "van Dijk (defense)")
## Bind, by row
bd_df <- rbind(bd_df_messi, bd_df_dijk)
bd_df <- bd_df[is.na(bd_df$variable) == FALSE, ]
(g_bd <- ggplot() + #scale_y_continuous(limits = c(0, 1)) +
    # ## vertical "lines"
    # geom_segment(aes(x=variable_num, xend=variable_num, y=variable, yend=last_cumulative),
    #              data = bd_df, color = "black", size = .5) +
    ## horizontal "bars"
    geom_segment(aes(x=cumulative, xend=last_cumulative, y=variable, yend=variable, color=player),
                 data = bd_df, size=1.5, alpha=.8) +
    facet_grid(col=vars(player)) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    labs(title = "Breakdown plot", color = "Players",
         y = "Variable", x = "Normalized contribution to prediction | variable order") +
    theme(legend.margin = margin(0,0,0,0),
          legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
)

## Relative wages and patchwork
wages_df <- tibble::tibble(
  player = factor(c("Messi (offense)", "van Dijk (defense)")),
  wages = .raw$wage_eur[c(1L, 8L)])
(g_wage <- ggplot(wages_df, aes(wages, player, xend=0, yend=player, color = player)) +
    geom_segment(size=3L) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Player", x = "Wages [2020 Euros]") +
    theme(legend.position = "off"))
### Plot together
require("cowplot")
if(F) ## With differing player wages
  (cp <- cowplot::plot_grid(
    g_wage, g_shap, g_bd, ncol = 1,
    rel_heights = c(1, 2, 2), labels=c("a)", "b)", "c)")))
(cp <- cowplot::plot_grid(
  g_shap, g_bd, ncol = 1,
  rel_heights = c(2, 2), labels=c("a)", "b)")))

## SAVE -----
ggplot2::ggsave(
  "./figures/ch5_fig1_shap_distr_bd.png",
  cp, device = "png", width = 7, height = 7, units = "in")


# Figures 2:4 are screen captures -----


# CASE STUDIES FOR CHEEM ----
## Setup ----
{
  require("cheem")
  require("spinifex")
  require("dplyr")
  require("ggplot2")
  require("cowplot")

  setwd("~/R/cheem_paper")
  fp <- "../cheem/inst/shiny_apps/cheem_initial/data/"
  if(F)
    dir("../cheem/inst/shiny_apps/cheem_initial/data/")

  ## Load data:
  penguins_ls   <- readRDS(paste0(fp, "preprocess_penguins.rds"))
  fifa_ls       <- readRDS(paste0(fp, "preprocess_fifa.rds"))
  ames2018_ls   <- readRDS(paste0(fp, "preprocess_ames2018.rds"))
  chocolates_ls <- readRDS(paste0(fp, "preprocess_chocolates.rds"))


  .x_axis_title     <- "          x: PC1, y: PC2        x: PC1, y: PC2  x: predicted, y: observed"
  .x_axis_title_reg <- "          x: PC1, y: PC2             x: PC1, y: PC2     x: predicted, y: observed"
}

## Penguins classification ------
{
  ## Data setup, spinifex::penguins
  names(penguins_ls)
  prim_obs <- 124L
  comp_obs <- 86L

  ### Global view and cheem tour stills
  .glob_view <- global_view(
    penguins_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme(legend.position = "right") +
    labs(color = 'Predicted class', shape = 'Predicted class',
         x = .x_axis_title) +
    ggtitle("Global view")
  .bas <- basis_attr_df(penguins_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(penguins_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    penguins_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6) + theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 1) +
    ggtitle("Cheem tour, extrema contributions")# +
  #theme(plot.title = element_text(hjust = 0.05))
  .cp <- cowplot::plot_grid(
    .glob_view, .cheem_stills,
    labels = c("a)", "b)"),
    ncol = 1)#, rel_heights = c(1.5, 1))
}

### Save
ggplot2::ggsave(
  "./figures/ch5_fig5_case1_penguins.png",
  plot = .cp, device = "png",
  width = 6, height = 5, units = "in")
.m <- gc()



## Chocolates classification -----
{
  names(chocolates_ls)
  prim_obs <- 22L
  comp_obs <- 34L
  if(F)
    global_view(chocolates_ls,
                22, #"Dark Chocolate Bar, Lindt, Switzerland"
                49) #"Pure Dark Chocolate, Hershey's, US"

  ### Global view and cheem tour stills
  .glob_view <- global_view(
    chocolates_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme(legend.position = "right") +
    labs(color = 'Predicted class', shape = 'Predicted class',
         x = .x_axis_title) +
    ggtitle("Global view")
  .bas <- basis_attr_df(chocolates_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(chocolates_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    chocolates_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6) + theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 1) +
    ggtitle("Cheem tour, extrema contributions")
  .cp <- cowplot::plot_grid(
    .glob_view, .cheem_stills,
    labels = c("a)", "b)"),
    ncol = 1)#, rel_heights = c(1.5, 1))
}
### Save
ggplot2::ggsave(
  "./figures/ch5_fig6_case2_chocolates.png",
  plot = .cp, device = "png",
  width = 6, height = 5, units = "in")
.m <- gc()


## FIFA 2020 wage regression ------
{
  ## load saved cheem_ls
  names(fifa_ls)
  prim_obs <- 1L
  comp_obs <- 8L

  ### global view and tours
  .glob_view <- global_view(
    fifa_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme(legend.position = "off") +
    labs(x = .x_axis_title_reg) +
    ggtitle("Global view")
  ## Without reaction and movement
  .bas <- basis_attr_df(fifa_ls$attr_df, prim_obs)[-c(3,7),, drop=FALSE]
  .mv  <- 3 ## "offense, that is what is talked about in the paper.
  .ggt <- radial_cheem_tour(
    fifa_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6, inc_var_nms = colnames(fifa_ls$attr_df)[-c(3,7)]) +
    theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 3) +
    ggtitle("Cheem tour, extrema contributions")
  .cp <- cowplot::plot_grid(
    .glob_view, .cheem_stills,
    labels = c("a)", "b)"),
    ncol = 1, rel_heights = c(1, 2))
}
### Save
ggplot2::ggsave(
  "./figures/ch5_fig7_case3_fifa.png",
  plot = .cp, device = "png",
  width = 5.5, height = 8, units = "in")
.m <- gc()


## Ames Housing 2018 (North Ames) ----
{
  names(ames2018_ls)
  prim_obs <- 170L
  comp_obs <- 220L
  if(F)
    global_view(ames2018_ls, prim_obs, comp_obs,
                color = ames2018_ls$decode_df$residual,
                shape = factor(ames2018_ls$decode_df$class))

  ### global view and tours
  .glob_view <- global_view(
    ames2018_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    theme(legend.position = "off") +
    labs(x = .x_axis_title_reg) +
    ggtitle("Global view")
  .bas <- basis_attr_df(ames2018_ls$attr_df, prim_obs)
  .mv  <- manip_var_of_attr_df(ames2018_ls$attr_df, prim_obs, comp_obs)
  .ggt <- radial_cheem_tour(
    ames2018_ls, basis = .bas, manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 6) + theme(legend.position = "")
  .cheem_stills <- spinifex::filmstrip(.ggt, nrow = 3) +
    ggtitle("Cheem tour, select frames")
  .cp <- cowplot::plot_grid(
    .glob_view, .cheem_stills,
    labels = c("a)", "b)"),
    ncol = 1, rel_heights = c(1, 2))
}
### Save
ggplot2::ggsave(
  "./figures/ch5_fig8_case4_ames2018.png",
  plot = .cp, device = "png",
  width = 8, height = 8, units = "in")
.m <- gc()


### rejected -- Tidy Tuesday coffee -----
if(F){
  if(F) ## Regress score
    browseURL("https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-07/readme.md")
  coffee <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
  (skimr::skim(coffee))
  Y <- coffee$total_cup_points
  hist(Y) ## exactly 1 with score 0 will remove
  summary(Y)
  r_idx <- Y > 20
  Y <- Y[r_idx]
  clas <- coffee$species[r_idx]
  X <- coffee[r_idx, c(
    "aroma", "flavor", "aftertaste", "acidity", "body", "balance",
    "uniformity", "clean_cup", "sweetness", "cupper_points", "moisture")]

  ## would need modern copy of workflow.
}
