# ch5_fig1_shap_distr_bd ----
{
  require("DALEX")
  require("dplyr")
  require("ggplot2")
  set.seed(2022)

  ## Local func
  my_parts_boxplot_df <- function(pred_parts, player_tag = "<tag unused>"){
    ## Remade from: iBreakDown:::print.break_down_uncertainty
    data.frame(
      player   = player_tag,
      label    = tapply(pred_parts$label, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
      variable = tapply(pred_parts$variable_name, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
      value    = tapply(pred_parts$variable_value, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
      ## Of the distribution of local attributions:
      # min      = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), min, na.rm = TRUE),
      # q1       = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.25, na.rm = TRUE),
      mean     = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), mean, na.rm = TRUE),
      median   = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), median, na.rm = TRUE)#,
      # q3       = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.75, na.rm = TRUE),
      # max      = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), max, na.rm = TRUE)
    )
  }
  my_parts_distribution <- function(pred_parts, player_tag = "<tag unused>"){
    df <- data.frame(
      player       = player_tag,
      label        = pred_parts$label,
      variable     = pred_parts$variable_name,
      value        = pred_parts$variable_value, ## Obs value of X
      contribution = pred_parts$contribution,   ## SHAP contribution
      B_perm_num   = pred_parts$B
    )
    rownames(df) <- paste(pred_parts$label, pred_parts$variable, pred_parts$B, sep = ": ")
    return(df)
  }
  .lvl_ord <- c("reaction", "offense", "movement", "defense", "power", "accuracy", "BMI", "age", "goalkeeping")
  my_bd_df <- function(break_down, player_tag = "<tag unused>", seq_name = "<seq_name unused>", seq_ord = .lvl_ord){
    df <- data.frame(
      player       = player_tag,
      seq_name     = seq_name,
      label        = break_down$label,
      variable     = break_down$variable_name,
      contribution = break_down$contribution, ## SHAP contribution
      cumulative   = break_down$cumulative,   ## Cumulative SHAP contribution
      sign         = break_down$sign
    )
    .n <- nrow(df)
    df$variable[is.na(df$variable)|df$variable==""] <- "prediction"
    df$variable <- factor(
      df$variable, rev(c("intercept", seq_ord, "prediction")))
    df$cumulative <- (df$cumulative - min(df$cumulative)) /
      (max(df$cumulative) - min(df$cumulative))
    df$last_cumulative <- c(NA, df$cumulative[-.n])
    df$variable_num <- 1:.n
    df$next_variable_num <- c(2:.n, NA)
    rownames(df) <- paste(break_down$label, break_down$variable, break_down$B, sep = ": ")
    df <- df[!(df$variable %in% c("intercept", "prediction")), ]
    return(df)
  }
  my_bd_plot <- function(df){
    ggplot() +
      ## horizontal bars
      geom_segment(
        aes(x = cumulative, xend = last_cumulative,
            y = variable, yend = variable, color = player),
        data = df, size = 1.5, alpha = .8) +
      facet_grid(rows = vars(seq_name), col = vars(player)) +
      theme_bw() +
      scale_color_brewer(palette = "Dark2") +
      labs(title = "Breakdown plot", color = "Players",
           y = "", x = "Normalized contribution to prediction") +
      theme(legend.margin   = margin(0, 0, 0, 0),
            legend.position = "off",
            axis.text.x     = element_blank(),
            axis.ticks.x    = element_blank())
  }
}

### Create FIFA x ------
{
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
      .keep    = "none",
      BMI      = (weight_kg+(height_cm/100L)^2L),
      age      = age,
      reaction = movement_reactions,
      offense  = (attacking_finishing+skill_long_passing+attacking_volleys+
                    power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
                    attacking_short_passing+skill_dribbling+skill_ball_control)/10L,
      defense  = (defending_sliding_tackle+mentality_interceptions+
                    defending_standing_tackle+defending_marking+mentality_aggression)/5L,
      accuracy = (attacking_heading_accuracy+power_shot_power)/2L,
      movement = (movement_sprint_speed+movement_balance+movement_acceleration+
                    mentality_vision+mentality_composure+movement_agility+
                    mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10L,
      power    = (power_strength+power_jumping)/2L,
      goalkeeping = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
                       goalkeeping_handling+goalkeeping_kicking)/5L
    )
  ## Class for the position of the player, either "fielder" or "goalkeeper"
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
  rf_mod <- randomForest::randomForest(
    Y~., data = data.frame(Y, X),
    mtry = .hp_mtry, nodesize = .hp_node, ntrees = .hp_ntrees)
  ## DALEX parts
  rf_expl <- DALEX::explain(model = rf_mod,
                            data  = X,
                            y     = Y,
                            label = "Random Forest")
}

## SHAP values & plot -----
## Messi SHAP
messi <- X[1, ]
shap_messi <- predict_parts(explainer       = rf_expl,
                            new_observation = messi,
                            type            = "shap",
                            B               = 25L) %>%
  as.data.frame()
B_norms_messi <- shap_messi %>%
  group_by(B) %>%
  summarize(B_norm = norm(matrix(contribution))) %>%
  as.data.frame()
shap_messi <- left_join(shap_messi, B_norms_messi, by = "B") %>%
  mutate(contribution = contribution / B_norm) ## Normalize by the norm within B of each player.
box_df_messi <- my_parts_boxplot_df(shap_messi, "Messi (offense)")

## Virgil van Dijk SHAP
dijk      <- X[8, ]
shap_dijk <- predict_parts(explainer       = rf_expl,
                           new_observation = dijk,
                           type            = "shap",
                           B               = 25L,
                           order = .lvl_ord) %>%
  as.data.frame()
B_norms_dijk <- shap_dijk %>%
  group_by(B) %>%
  summarize(B_norm = norm(matrix(contribution))) %>%
  as.data.frame()
shap_dijk <- left_join(shap_dijk, B_norms_dijk, by = "B") %>%
  mutate(contribution = contribution / B_norm) ## Normalize by the norm within B of each player.
box_df_dijk <- my_parts_boxplot_df(shap_dijk, "van Dijk (defense)")

## Bind shap aggs:
boxplot_df          <- rbind(box_df_messi, box_df_dijk)
boxplot_df$variable <- factor(boxplot_df$variable, levels = rev(.lvl_ord))

## B Distributions of the SHAPS
dist_shap_messi  <- my_parts_distribution(shap_messi, "Messi (offense)")
dist_shap_dijk   <- my_parts_distribution(shap_dijk, "van Dijk (defense)")
dist_df          <- rbind(dist_shap_messi, dist_shap_dijk)
dist_df$variable <- factor(dist_df$variable, levels = rev(.lvl_ord))

(g_shap <- ggplot(boxplot_df) +
    # Connecting grey line
    geom_segment(aes(x=`Messi (offense)`, xend=`van Dijk (defense)`, y=variable, yend=variable),
                 alpha =.7, size = 2L, color = "grey", fill = NA,
                 data = boxplot_df %>% select(player, variable, mean) %>%
                   tidyr::pivot_wider(names_from = "player", values_from = "mean") %>%
                   mutate(sum = `Messi (offense)` + `van Dijk (defense)`)) +
    geom_point(aes(x=mean, y=variable, color=player, fill=player),
               alpha =.7, size = 5L) +
    ## Shap distributions
    geom_point(aes(x=contribution, y=variable, color=player, fill=player),
               dist_df, alpha =.8, size = 3, shape = 124,
               position = position_dodge(-.5)) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(title = "SHAP",
         y = "", x = "Normalized SHAP values") +
    theme(legend.margin   = margin(0, 0, 0, 0),
          legend.position = "bottom"))

## Breakdowns & plot ----
## add alternate sequences -----
shap_byplayer <- boxplot_df %>% select(player, variable, mean) %>%
  tidyr::pivot_wider(names_from = "player", values_from = "mean")
.seq_messi <-
  shap_byplayer$variable[order(abs(shap_byplayer$`Messi (offense)`), decreasing = TRUE)] %>%
  as.character()
.seq_dijk  <-
  shap_byplayer$variable[order(abs(shap_byplayer$`van Dijk (defense)`), decreasing = TRUE)] %>%
  as.character()

## seq_1, desc messi shap
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = messi,
                    type            = "break_down",
                    order           = .seq_messi)
bd_df_messi1 <- my_bd_df(bd, "Messi (offense)", "sequence 1", .seq_messi)
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = dijk,
                    type            = "break_down",
                    order           = .seq_messi)
bd_df_dijk1  <- my_bd_df(bd, "van Dijk (defense)", "sequence 1", .seq_messi)
## seq_2, desc dijk shap
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = messi,
                    type            = "break_down",
                    order           = .seq_dijk)
bd_df_messi2 <- my_bd_df(bd, "Messi (offense)", "sequence 2", .seq_dijk)
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = dijk,
                    type            = "break_down",
                    order           = .seq_dijk)
bd_df_dijk2  <- my_bd_df(bd, "van Dijk (defense)", "sequence 2", .seq_dijk)
## seq_3, desc sum shap
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = messi,
                    type            = "break_down",
                    order           = .lvl_ord)
bd_df_messi3 <- my_bd_df(bd, "Messi (offense)", "sequence 3", .lvl_ord)
bd <- predict_parts(explainer       = rf_expl,
                    new_observation = dijk,
                    type            = "break_down",
                    order           = .lvl_ord)
bd_df_dijk3 <- my_bd_df(bd, "van Dijk (defense)", "sequence 3", .lvl_ord)

bd_seq1 <- my_bd_plot(rbind(bd_df_messi1, bd_df_dijk1)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  labs(x = element_blank(), title = "Breakdown plots")
bd_seq2 <- my_bd_plot(rbind(bd_df_messi2, bd_df_dijk2)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        strip.text.x = element_blank()) +
  labs(title = element_blank(), x = element_blank())
bd_seq3 <- my_bd_plot(rbind(bd_df_messi3, bd_df_dijk3)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        strip.text.x = element_blank()) +
  labs(title = element_blank())
(g_bd <- cowplot::plot_grid(bd_seq1, bd_seq2, bd_seq3, ncol = 1, rel_heights = c(1.25, 1, 1.08)))

### Plot together
require("cowplot")
(cp <- cowplot::plot_grid(
  g_bd, g_shap, ncol = 1, rel_heights = c(3, 2), labels = c("a)", "b)")))

## SAVE -----
ggplot2::ggsave(
  "./figures/ch5_fig1_shap_distr_bd.png",
  cp, device = "png", width = 6.1, height = 8, units = "in")


# Figures 2:3 are screen captures -----


# CASE STUDIES FOR CHEEM ----
## Setup ----
{
  require(cheem)
  require(spinifex)
  require(dplyr)
  require(ggplot2)
  require(cowplot)
  require(patchwork)

  wd <- getwd()
  if(substr(wd, nchar(wd) - 8, nchar(wd)) != "thesis_ns")
    warning("Expected work directory to be thesis_ns")
  fp <- "../cheem/inst/shiny_apps/cheem_initial/data/"
  if(F)
    dir("../cheem/inst/shiny_apps/cheem_initial/data/")

  ## Load data:
  penguins_ls   <- readRDS(paste0(fp, "preprocess_penguins.rds"))
  fifa_ls       <- readRDS(paste0(fp, "preprocess_fifa.rds"))
  ames2018_ls   <- readRDS(paste0(fp, "preprocess_ames2018.rds"))
  chocolates_ls <- readRDS(paste0(fp, "preprocess_chocolates.rds"))
  .t <- theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
              axis.text   = element_blank(),
              axis.title  = element_blank(),
              axis.ticks  = element_blank(),
              legend.position = "off", aspect.ratio = 1)
}

## Penguins classification ------
{
  ## Data setup, spinifex::penguins
  names(penguins_ls)
  prim_obs <- 243
  comp_obs <- 169

  ### Global view and cheem tour stills
  .glob_view <- global_view(
    penguins_ls, prim_obs, comp_obs, as_ggplot = TRUE) + .t +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + theme(legend.position  = "bottom",
                                   legend.direction = "horizontal")
  .bas <- basis_attr_df(penguins_ls$attr_df, prim_obs)
  .mv  <- which(colnames(penguins_ls$attr_df) == "f_l")
  ## Cheem tour for stills
  mt_interp <- manual_tour(basis = .bas, manip_var = .mv) %>%
    spinifex:::interpolate_manual_tour(angle = .15) ## app is .15
  dim(mt_interp)
  .ggt1 <- radial_cheem_tour(
    penguins_ls, basis = mt_interp[,,1, drop=FALSE], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.22)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    penguins_ls, basis = mt_interp[,,8, drop=FALSE], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124, angle = 0) + .t
  ## Using patchwork:
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    ncol = 1, align = "v", axis = "l", rel_heights = c(1, 1.1))
}

### Save still shots
ggplot2::ggsave(
  "./figures/ch5_fig4_case_penguins.png",
  plot = .cp, device = "png",
  width = 6, height = 6, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4),
 Hosted on Vimeo and not stored in thesis repo.")

## Penguins orthogonal BlFl ----
(.g <- ggplot(penguins_na.rm, aes(x=bill_length_mm,
                                  y=flipper_length_mm,
                                  colour=species)) +
    geom_point(size=1) +
    ## Prim obs
    geom_point(data=penguins_na.rm[243,], shape=8, size=5, alpha=0.8) +
    ## Comparison obs
    #geom_point(data=penguins_na.rm[169,], shape=4, size=3, alpha=0.6) +
    theme_bw() +
    theme(aspect.ratio = 1) +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "Flipper length [mm]", x = "Bill length [mm]", color = "Observed species"))
## Save
ggplot2::ggsave(
  "./figures/ch5_fig5_case_penguins_BlFl.png",
  plot = .g, device = "png",
  height = 4, units = "in")


## Chocolates classification -----
{
  names(chocolates_ls)
  prim_obs <- 22L #"Dark Chocolate Bar, Lindt, Switzerland"
  comp_obs <- 7L  #"70% Cocoa, Columbia
  if(F)
    global_view(chocolates_ls, prim_obs, comp_obs)

  ### Global view
  .glob_view <- global_view(
    chocolates_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + .t + theme(
      legend.margin    = margin(0,0,0,0),
      legend.position  = "bottom",
      legend.direction = "horizontal")
  .inc_var_nms <- c("Calories", "SatFat", "Chol", "Na", "Fiber", "Sugars")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- basis_attr_df(chocolates_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "Sugars")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  ## Cheem tour stills for paper
  .ggt1 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.18)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,20], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    ncol = 1, align = "v", axis = "l", rel_heights = c(1, 1.2))
}

### Save Stills
ggplot2::ggsave(
  "./figures/ch5_fig6_case_chocolates.png",
  plot = .cp, device = "png",
  width = 6, height = 6, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4)")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_chocolates.mp4


## Chocolates inverse case -----
{
  names(chocolates_ls)
  prim_obs <- 84L #"Milk Chocolate Square, Ghiradelli, US"
  comp_obs <- 71L #"Classic Milk Chocolate Bar, Nestle, Switzerland"
  if(F)
    global_view(chocolates_ls, prim_obs, comp_obs)

  ### Global view
  .glob_view <- global_view(
    chocolates_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + .t + theme(
      plot.margin      = margin(0,0,0,0),
      legend.margin    = margin(0,0,0,0),
      legend.position  = "bottom",
      legend.direction = "horizontal")
  .inc_var_nms <- c("CalFat", "TotFat", "SatFat", "Na", "Fiber", "Sugars")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- basis_attr_df(chocolates_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "Na")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  ## Cheem tour stills for paper
  .ggt1 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.18)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    chocolates_ls, basis = mt_interp[,,19], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    rel_heights = c(1, 1.2),
    ncol = 1, align = "v", axis = "l")
}

### Save Stills
ggplot2::ggsave(
  "./figures/ch5_fig7_case_chocolates_inverse.png",
  plot = .cp, device = "png",
  width = 6, height = 6, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4),
 Hosted on Vimeo and not stored in thesis repo.")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_chocolates_inverse.mp4


## ONE OFF FUNC; ----
## regression case basis below scatterplot
THIS_REG_radial_cheem_tour  <- function(
  cheem_ls, basis, manip_var,
  primary_obs         = NULL,
  comparison_obs      = NULL,
  do_add_pcp_segments = TRUE,
  pcp_shape           = c(142, 124, 3), ## '|' plotly and gganimate, or '+' respectively
  angle               = .15,
  row_index           = NULL,
  inc_var_nms         = NULL,
  do_center_frame     = TRUE,
  do_add_residual     = FALSE
){
  if(is.null(row_index) == FALSE)
    if(sum(row_index) == 0L)
      stop("radial_cheem_tour: sum of row_index was 0.")

  ## Initialize
  x <- y <- NULL
  decode_df <- cheem_ls$decode_df
  .prim_obs <- primary_obs    ## Proto_basis1d_distribution EXPECTS NUMERIC INDEX;
  .comp_obs <- comparison_obs ## Don't coerce to logical index
  .n        <- nrow(decode_df)
  ## column & row indexes
  if(is.null(inc_var_nms)) inc_var_nms <- colnames(cheem_ls$attr_df)
  .col_idx <- colnames(decode_df) %in% inc_var_nms
  if(is.null(row_index) == FALSE){
    ## Change row_index from numeric to logical if needed and replicate
    row_index <- as_logical_index(row_index, .n)
    row_index[c(.prim_obs, .comp_obs)] <- TRUE
  }
  ## Subset columns and scale plot data
  .dat <- decode_df[, .col_idx] %>% spinifex::scale_sd() %>%
    spinifex::scale_01() %>% as.data.frame()
  ## Manual (radial) tour 1d
  .mt_path <- spinifex::manual_tour(basis, manip_var)

  ## Problem type & aesthetics
  .prob_type <- cheem_ls$type ## Either "classification" or "regression"
  .alpha <- logistic_tform(.n)

  ### Classification case ---
  if(.prob_type == "classification")
    stop("NS: DO NOT USE THIS ONE OFF FUNCTION FOR CLASSIFICATION,
         it was made to make the regression case basis below the scatterplot")

  ### Regression case ---
  ## Doubling data to facet on obs and residual.
  if(.prob_type == "regression"){
    ## Scale obs y, resid, df_hline
    .y        <- decode_df$y %>% spinifex::scale_sd() %>% spinifex::scale_01()
    .resid    <- decode_df$residual %>% spinifex::scale_sd() %>% spinifex::scale_01()
    .df_hline <- data.frame(x = FALSE, y = mean(.resid), facet_var = "residual")

    # Aesthetics setup
    .class    <- factor(FALSE) #decode_df$class|predicted_class
    .pts_prim_obs <- .pts_comp_obs <- NULL
    ## Condition handle adding residual facet or not
    if(do_add_residual){
      ## Double up data; observed y and residual
      if(is.null(.prim_obs) == FALSE)
        .pts_prim_obs <- c(.prim_obs, .n + .prim_obs)
      if(is.null(.comp_obs) == FALSE)
        .pts_comp_obs <- c(.comp_obs, .n + .comp_obs)
      if(length(.class) > 1L){.class_fore <- c(.class, .class)
      } else .class_fore <- .class ## could be dummy factor(FALSE)
      ## Foreground:
      .dat_fore   <- rbind(.dat, .dat)
      .idx_fore   <- c(row_index, row_index)
      .facet_fore <- factor(rep(c("observed y", "residual"), each = 2L * .n))
      .fixed_y    <- c(.y, .resid)
    } else {
      ## not doubled up data; just fixed_observed y
      if(is.null(.prim_obs) == FALSE)
        .pts_prim_obs <- .prim_obs
      if(is.null(.comp_obs) == FALSE)
        .pts_comp_obs <- .comp_obs
      ## Foreground:
      .dat_fore   <- .dat
      .idx_fore   <- row_index
      .facet_fore <- rep("observed y", each = .n)
      .class_fore <- .class
      .fixed_y    <- .y
    }

    ## ggtour
    ggt <- spinifex::ggtour(.mt_path, .dat_fore, angle = angle,
                            do_center_frame = do_center_frame) +
      ### FACET REMOVED HERE ------
    ## changing to vertical display results in basis on top, try to change map_relative
    #spinifex::facet_wrap_tour(facet_var = .facet_fore, nrow = 1L) +
    spinifex::append_fixed_y(fixed_y = .fixed_y) +
      ## Plotly doesn't rotate text in geom_text/annotate.
      ggplot2::theme(legend.position = "off",
                     axis.title.y = ggplot2::element_text(
                       angle = 90L, vjust = 0.5)) +
      ## Exasperates issues with plotly & geom presence issue.
      #spinifex::proto_frame_cor2(row_index = .idx_fore, position = c(.5, 1.1)) +
      ## Points; 1D proj & fixed y
      spinifex::proto_point(
        aes_args = list(color = .class_fore, shape = .class_fore),
        identity_args = list(alpha = .alpha), row_index = .idx_fore) +
      proto_basis1d_distribution(
        cheem_ls$attr_df,
        primary_obs = .prim_obs, comparison_obs = .comp_obs,
        position = "bottom1d", group_by = .class, pcp_shape = pcp_shape,
        do_add_pcp_segments = as.logical(do_add_pcp_segments),
        inc_var_nms = inc_var_nms, row_index = row_index) +
      spinifex::proto_basis1d(position = "bottom1d", manip_col = "black") +
      ## Highlight comparison obs
      spinifex::proto_highlight(
        row_index = .pts_comp_obs,
        identity_args = list(size = 3L, shape = 4L, alpha = 0.6, color = "black")) +
      ## Highlight primary obs
      spinifex::proto_highlight(
        row_index = .pts_prim_obs,
        identity_args = list(size = 5L, shape = 8L, alpha = .8, color = "black"))
    if(do_add_residual){
      ggt <- ggt +
        ## Use manual geom_hline as proto_hline0 is on all facets.
        ggplot2::geom_hline(ggplot2::aes(yintercept = y), .df_hline, color = "grey40")
    }
  }
  ## Return the static ggtour, animate in app
  ggt
}


## FIFA 2020 wage regression ------
{
  ## load saved cheem_ls
  names(fifa_ls)
  prim_obs <- 1L
  comp_obs <- 8L

  ### global view and tours
  .glob_view <- global_view(
    fifa_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + .t + theme(
      legend.margin    = margin(0,0,0,0),
      legend.position  = "off",
      legend.direction = "horizontal")
  .inc_var_nms <- c("BMI", "react", "off", "def", "mvm", "pwr")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- basis_attr_df(fifa_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "def")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  .ggt1 <- radial_cheem_tour(
    fifa_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.18)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    fifa_ls, basis = mt_interp[,,9], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    rel_heights = c(1, 1.2), ncol = 1)
}
### Save
ggplot2::ggsave(
  "./figures/ch5_fig8_case_fifa.png",
  plot = .cp, device = "png",
  width = 6, height = 7, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4),
 Hosted on Vimeo and not stored in thesis repo.")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_fifa.html



## Ames Housing 2018 (North Ames) ----
{
  names(ames2018_ls)
  prim_obs <- 74 ## Large lot area to living area ratio ## small house, big lot
  comp_obs <- 192 ## Small on that ratio ## large house, small lot
  if(F)
    global_view(ames2018_ls, prim_obs, comp_obs,
                color = ames2018_ls$decode_df$residual,
                shape = factor(ames2018_ls$decode_df$class))

  .glob_view <- global_view(
    ames2018_ls, prim_obs, comp_obs, as_ggplot = TRUE) +
    labs(color = "Predicted class", shape = "Predicted class", x = element_blank()) +
    ggtitle("Global view") + .t + theme(
      plot.margin      = margin(0,0,0,0),
      legend.margin    = margin(0,0,0,0),
      legend.position  = "off",
      legend.direction = "horizontal")
  .inc_var_nms <- c("LtA", "Qlt", "YrB", "LvA", "Rms", "GYB", "GrA")
  ## Removed 4 with lowest contribution for the prim_obs.
  .bas <- basis_attr_df(ames2018_ls$attr_df[, .inc_var_nms], prim_obs)
  .mv  <- which(.inc_var_nms == "LtA")
  mt_interp <- manual_tour(.bas, .mv) %>%
    spinifex:::interpolate_manual_tour(.15) ## App angle.
  dim(mt_interp)
  .ggt1 <- radial_cheem_tour(
    ames2018_ls, basis = mt_interp[,,1], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t +
    theme(plot.title = element_text(hjust = 0.18)) +
    ggtitle("Radial tour, select frames")
  .ggt2 <- radial_cheem_tour(
    ames2018_ls, basis = mt_interp[,,17], manip_var = .mv,
    primary_obs = prim_obs, comparison_obs = comp_obs,
    do_add_pcp_segments = FALSE, inc_var_nms = .inc_var_nms,
    pcp_shape = 124, angle = 0) + .t
  .pw <- .ggt1 + .ggt2
  .cp <- cowplot::plot_grid(
    .glob_view, .pw, labels = c("a", "b"),
    rel_heights = c(1, 1.2), ncol = 1)
}
### Save
ggplot2::ggsave(
  "./figures/ch5_fig9_case_ames2018.png",
  plot = .cp, device = "png",
  width = 6, height = 7, units = "in")
.m <- gc()

### Save .mp4, add GitHub urls to paper
message("NOTE: Manually capturing view from app with Screen to GIF (.mp4),
 Hosted on Vimeo and not stored in thesis repo.")
## https://github.com/nspyrison/cheem_paper/blob/main/figures/case_ames2018.mp4

