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
.lvl_ord <- c("react", "off", "mvm", "def", "pwr", "acc", "bmi", "age", "gk")
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
    bmi = (weight_kg+(height_cm/100L)^2L),
    age = age,
    react = movement_reactions,
    off = (attacking_finishing+skill_long_passing+attacking_volleys+
             power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
             attacking_short_passing+skill_dribbling+skill_ball_control)/10L,
    def = (defending_sliding_tackle+mentality_interceptions+
             defending_standing_tackle+defending_marking+mentality_aggression)/5L,
    acc = (attacking_heading_accuracy+power_shot_power)/2L,
    mvm = (movement_sprint_speed+movement_balance+movement_acceleration+
             mentality_vision+mentality_composure+movement_agility+
             mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10L,
    pwr = (power_strength+power_jumping)/2L,
    gk = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
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
box_df_messi <- my_parts_boxplot_df(shap_messi, "Messi")

## Virgil van Dijk SHAP
dijk <- X[8, ]
shap_dijk <- predict_parts(explainer       = rf_expl,
                           new_observation = dijk,
                           type            = "shap",
                           B               = 25L,
                           order = .lvl_ord)
shap_dijk$contribution <- shap_dijk$contribution %>%
  spinifex::scale_01()
box_df_dijk <- my_parts_boxplot_df(shap_dijk, "van Dijk")

## Bind shap aggs:
boxplot_df <- rbind(box_df_messi, box_df_dijk)
boxplot_df$variable <- factor(boxplot_df$variable, levels = rev(.lvl_ord))

## B Distributions of the SHAPS
dist_shap_messi <- my_parts_distribution(shap_messi, "Messi")
dist_shap_dijk <- my_parts_distribution(shap_dijk, "van Dijk")
dist_df <- rbind(dist_shap_messi, dist_shap_dijk)
dist_df$variable <- factor(dist_df$variable, levels = rev(.lvl_ord))

(g_shap <- ggplot(boxplot_df) +
    ## Connecting grey line
    geom_segment(aes(x=Messi, xend=`van Dijk`, y=variable, yend=variable),
                 alpha =.7, size = 2L, color = "grey", fill = NA,
                 data = boxplot_df %>% select(player, variable, median) %>%
                   tidyr::pivot_wider(names_from = "player", values_from = "median") %>%
                   mutate(sum = Messi + `van Dijk`)) +
    geom_point(aes(x=median, y=variable, color=player, fill=player),
               alpha =.7, size = 5L) +
    ## Shap distributions
    geom_point(aes(x=contribution, y=variable, color=player, fill=player),
               dist_df, alpha =.8, size = 3, shape = 124,
               position = position_dodge(-.5)) +
    # geom_boxplot(
    #   aes(ymin=min, lower=q1, middle=median, upper=q3, ymax=max),
    #   stat = "identity", alpha =.3) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(title="SHAP values",
         y = "variable", x = "SHAP values, normalized\n The median of the contributions while permuting X's") +
    theme(legend.position = "off")
)

## Breakdowns & plot ----
## Messi Breakdown
bd_messi <- predict_parts(explainer       = rf_expl,
                          new_observation = messi,
                          type            = "break_down",
                          order= .lvl_ord)
bd_df_messi <- my_bd_df(bd_messi, "Messi")
## Dijk Breakdown
bd_dijk <- predict_parts(explainer       = rf_expl,
                         new_observation = dijk,
                         type            = "break_down",
                         order = .lvl_ord)
bd_df_dijk <- my_bd_df(bd_dijk, "van Dijk")
## Bind, by row
bd_df <- rbind(bd_df_messi, bd_df_dijk)
bd_df <- bd_df[is.na(bd_df$variable) == FALSE, ]
(g_bd <- ggplot(bd_df) +
    geom_segment(aes(x=cumulative, xend=last_cumulative, y=variable, yend=variable, color=player),
                 size=1.5, alpha=.8) + facet_grid(col=vars(player))+
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    labs(title="Breakdown profile of predictions",
         y = "variable", x = "contribution to prediction | variable order") +
    theme(legend.margin = margin(0,0,0,0),
          legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()))

## Relative wages and patchwork
wages_df <- tibble::tibble(
  player = factor(c("Messi", "van Dijk")),
  wages = .raw$wage_eur[c(1L, 8L)])
(g_wage <- ggplot(wages_df, aes(wages, player, xend=0, yend=player, color = player)) +
    geom_segment(size=3L) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "player", x = "wages [2020 Euros]") +
    theme(legend.position = "off"))
### Plot together
require("patchwork")
(pw <- g_wage / g_shap / g_bd +
    plot_layout(heights = c(1, 3, 3)))

## SAVE -----
ggplot2::ggsave(
  "./figures_from_script/ch5_fig1_shap_distr_bd.pdf",
  pw, device = "pdf", width = 6, height = 7, units = "in")


# ch5_fig2_global_space -----
{
  require("plotly")
  require("spinifex")
  require("cheem")
  load("./data/2preprocess_simulation.RData")
  shap_obs <- 18L; comp_obs <- 111L;

  ## Local functions from cheem app
  THIS_linked_plotly_func <- function(
    layer_ls,
    shap_obs = NULL,
    comp_obs = NULL,
    height_px = 640L,
    width_px = 640L,
    do_include_maha_qq = FALSE
  ){
    .alpha <- ifelse(nrow(layer_ls$decode_df) > 999L, .1, .6)
    .xlab <- ifelse(do_include_maha_qq == FALSE, "PC1",
                    "PC1 | Quantile, chi-squared")
    .ylab <- ifelse(do_include_maha_qq == FALSE, "PC2",
                    "PC2 | Quantile, observed Mahalanobis distance")
    ## Remove QQ maha rows if needed
    plot_df <- layer_ls$plot_df ## Init
    if(do_include_maha_qq == FALSE){
      plot_df <- layer_ls$plot_df[
        layer_ls$plot_df$projection_nm != "QQ Mahalanobis distance", ]
      height_px <- height_px / 2L ## Half height display as qq maha is removed.
    }
    is_classification <- attr(layer_ls, "problem_type") == "classification"
    # ifelse("is_misclassified" %in% colnames(layer_ls$decode_df), TRUE, FALSE)
    pred_clas <- as.factor(FALSE) ## If regression; dummy pred_clas
    if(is_classification == TRUE) pred_clas <-
      layer_ls$decode_df$predicted_class %>%
      rep_len(nrow(plot_df)) %>%
      as.factor()

    gg <- plot_df %>%
      plotly::highlight_key(~rownum) %>%
      ggplot(aes(V1, V2))
    ## Red misclassified points, if present
    if(is_classification == TRUE){
      .rn_misclass <- which(layer_ls$decode_df$is_misclassified == TRUE)
      .idx_misclas <- plot_df$rownum %in% .rn_misclass
      if(sum(.idx_misclas) > 0L){
        .df <- plot_df[.idx_misclas, ] %>% plotly::highlight_key(~rownum)
        gg <- gg +
          geom_point(aes(V1, V2), .df,
                     color = "red", fill = NA,
                     shape = 21L, size = 3L, alpha = .alpha)
      }
    }
    ## Highlight comparison obs, if passed
    if(is.null(comp_obs) == FALSE){
      .idx_comp <- plot_df$rownum == comp_obs
      if(sum(.idx_comp) > 0L){
        .df <- plot_df[.idx_comp, ] %>% plotly::highlight_key(~rownum)
        gg <- gg +
          ## Highlight comparison obs
          geom_point(aes(V1, V2, color = pred_clas[.idx_comp]),
                     .df, size = 4L, shape = 4L)
      }
    }
    ## Highlight shap obs, if passed
    if(is.null(shap_obs) == FALSE){
      .idx_shap <- plot_df$rownum == shap_obs
      if(sum(.idx_shap) > 0L){
        .df <- plot_df[.idx_shap, ] %>% plotly::highlight_key(~rownum)
        gg <- gg +
          geom_point(aes(V1, V2, color = pred_clas[.idx_shap]),
                     .df, size = 5L, shape = 8L)
      }
    }
    ## Maha skew text,
    #### geom_text not working with plotly... & annotate() not working with facets...
    if(do_include_maha_qq == TRUE){
      gg <- gg +
        geom_text(aes(x = -Inf, y = Inf, label = ggtext),
                  hjust = 0L, vjust = 1L, size = 4L)
    }
    ## Normal points
    gg <- gg +
      suppressWarnings(geom_point(
        aes(V1, V2, color = pred_clas, shape = pred_clas, tooltip = tooltip),
        alpha = .alpha)) +
      facet_grid(rows = vars(projection_nm), cols = vars(layer_nm), scales = "free") +
      theme_bw() +
      labs(x = .xlab, y = .ylab) +
      scale_color_brewer(palette = "Dark2") +
      theme(axis.text  = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "off")
    return(gg)
  }

  THIS_manual_tour1d_func <- function(tour_array,
                                      layer_ls, shap_obs, comp_obs = NULL,
                                      do_add_basis_distri = FALSE,
                                      do_add_pcp_segments = FALSE,
                                      pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
                                      angle = .2
  ){
    ## Initialization
    .y <- layer_ls$decode_df$y %>% matrix(ncol = 1L)
    .col_idx <- which(!(
      colnames(layer_ls$decode_df) %in%
        c("rownum", "class", "y", "prediction", "residual",
          "predicted_class", "is_misclassified", "tooltip")
    ))
    .x <- layer_ls$decode_df[, .col_idx] ## Numeric X variables

    ## Problem type: classification or regression?
    problem_type <- function(y){
      if(length(unique(y)) < 5L) return("classification")
      if(is.numeric(y) == TRUE) return("regression")
      stop("y was expected as a with less than 5 unique values, or numeric indicating a classification or regression problem respectivly.")
    }
    .prob_type <- problem_type(.y) ## Either "classification" or "regression"
    .pred_clas <- as.factor(FALSE) ## Initialize dummy predicted class
    if(.prob_type == "classification")
      .pred_clas <- layer_ls$decode_df$predicted_class
    .alpha <- ifelse(nrow(layer_ls$decode_df) > 999L, .1, 1L)

    tour_array
    ### Classification problem -----
    if(.prob_type == "classification"){
      .dat <- .x %>% scale_01
      ggt <- ggtour(tour_array, .dat, angle = angle) +
        proto_density(aes_args = list(color = .pred_clas, fill = .pred_clas)) +
        proto_origin1d() +
        proto_basis1d(manip_col = "black")## leaves space
      if(do_add_basis_distri == TRUE){
        ggt  <- ggt + cheem::proto_basis1d_distribution(
          layer_ls,
          group_by = .pred_clas,
          position = "top1d",
          shape = pcp_shape, ## '|' for gganimate/ggplot
          do_add_pcp_segments = as.logical(do_add_pcp_segments),
          primary_obs = shap_obs,
          comparison_obs = comp_obs)
      }
      ggt <- ggt +
        ## Highlight comparison obs, if passed
        proto_highlight1d(comp_obs,
                          list(color = .pred_clas),
                          list(linetype = 3L, alpha = 0.8)) +
        ## Highlight shap obs
        proto_highlight1d(shap_obs,
                          list(color = .pred_clas),
                          list(linetype = 2L, alpha = .6, size = .8))
    }

    ## Return the static ggtour, animate in app
    return(ggt)
  }
}

## Create ggplot
p <- THIS_linked_plotly_func(layer_ls, shap_obs, comp_obs,
                             do_include_maha_qq = FALSE)
p <- p +
  labs(title = "PC1:2 of data and SHAP spaces")

## Save -----
ggplot2::ggsave(
  "./figures_from_script/ch5_fig2_global_space.pdf",
  plot = p + theme(aspect.ratio=1), device = "pdf",
  width = 8, height = 4.8, units = "in")

## Save interactive html widget
ggp <- ggplotly(p, tooltip = "tooltip") %>%
  config(displayModeBar = FALSE) %>% ## Remove html buttons
  layout(dragmode = "select", showlegend = FALSE,
         width = 640, height = 320) %>% ## Set drag left mouse
  event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
  highlight(on = "plotly_selected", off = "plotly_deselect")
htmlwidgets::saveWidget(ggp, "./figures_from_script/ch5_fig2_global_space.html",
                        selfcontained = TRUE)

# ch5_fig3_cheem_initial_bas -----
.df <- layer_ls$shap_df
bas <- .df[shap_obs, -ncol(.df)] %>%
  as.matrix(nrow = 1L) %>% t() %>%
  tourr::normalise()
mv <- 2L #spinifex::manip_var_of(bas)
.opts <- rownames(bas)
mv_nm <- .opts[mv]

?cheem::radial_cheem_ggtour()
ggt142 <- radial_cheem_ggtour(
  layer_ls, bas, mv_nm,
  shap_obs, comp_obs,
  #do_add_pcp_segements = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .2)
(anim <-
    animate_plotly(ggt142) %>%
    layout(dragmode = FALSE, showlegend = FALSE,
           ## Animation doesn't seem to like width/height.
           ) %>%
    event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect"))

## Save .html cheem tour, and with prim/comp obs swapped -----
htmlwidgets::saveWidget(
  anim, "./figures_from_script/ch5_fig3_cheem_tour.html",
  selfcontained = TRUE)
### SWAPPING shap and comp obs
bas_swap <- .df[comp_obs, -ncol(.df)] %>%
  as.matrix(nrow = 1L) %>% t() %>%
  tourr::normalise()
ggt142_swapped <- radial_cheem_ggtour(
  layer_ls, bas_swap, mv_nm,
  primary_obs = comp_obs, comparison_obs = shap_obs,
  #do_add_pcp_segements = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .2) + ggtitle("Comparison point's perspective \n(swap primary & comparison obs)")
(anim_swapped <-
    animate_plotly(ggt142_swapped) %>% layout(dragmode = FALSE, showlegend = FALSE) %>% ## Set drag left mouse
    event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect"))
htmlwidgets::saveWidget(
  anim_swapped,
  "./figures_from_script/ch5_fig3_cheem_tour_SWAPPED.html",
  selfcontained = TRUE)

## Manually make static frames -----
{
  array_dummy <- bas
  attr(array_dummy, "manip_var") <- mv
  manual_tour(array_dummy, mv)

  ## Initial basis, with distribution
  fr1_with <- THIS_manual_tour1d_func(
    tour_array = array_dummy,
    layer_ls,
    shap_obs, comp_obs,
    do_add_basis_distri = TRUE,
    do_add_pcp_segments = TRUE,
    pcp_shape = 124L, ## '|' plotly and gganimate respectively
    angle = .2) +
    theme(legend.position = "off") +
    ggtitle(waiver(), subtitle = "Initial contribution")
  ## Initial, with/out distribution
  fr1_wo <- THIS_manual_tour1d_func(
    tour_array = array_dummy,
    layer_ls,
    shap_obs, comp_obs,
    do_add_basis_distri = FALSE,
    do_add_pcp_segments = FALSE,
    pcp_shape = 124L, ## '|' plotly and gganimate respectively
    angle = .2) +
    theme(legend.position = "off") +
    ggtitle(waiver(), subtitle = "Initial contribution")
  msp <- create_manip_space(bas, mv)
  ## Full contribution
  bas2 <- rotate_manip_space(msp, 0, -1.6)
  attr(bas2, "manip_var") <- mv
  (fr2 <- THIS_manual_tour1d_func(
    tour_array = bas2,
    layer_ls,
    shap_obs, comp_obs,
    FALSE, FALSE,
    pcp_shape = 124L, ## '|' plotly and gganimate respectively
    angle = .2) +
      theme(legend.position = "off") +
      ggtitle(waiver(), subtitle = "Full contribution"))
  ## Zero contribution
  bas3 <- rotate_manip_space(msp, 0, -.029)
  attr(bas3, "manip_var") <- mv
  (fr3 <- THIS_manual_tour1d_func(
    tour_array = bas3,
    layer_ls,
    shap_obs, comp_obs,
    FALSE, FALSE,
    pcp_shape = 124L, ## '|' plotly and gganimate respectively
    angle = .2) +
      theme(legend.position = "off") +
      ggtitle(waiver(), subtitle = "Zero contribution"))

  require("patchwork")
  pw <- (fr1_wo + fr2 + fr3)
}

## Save static frames -----
## Frame 1 with distribution
ggplot2::ggsave(
  "./figures_from_script/ch5_fig3_cheem_initial_bas.pdf",
  fr1_with, device = "pdf", width = 8, height = 4, units = "in")

# ch5_fig4_cheem_endpts technically
## tour end points without distribution
ggplot2::ggsave(
  "./figures_from_script/ch5_fig4_cheem_endpts.pdf",
  pw, device = "pdf", width = 8, height = 4, units = "in")
