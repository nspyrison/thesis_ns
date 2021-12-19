library("tidyverse")
library("dplyr")

## Following the loose setup of _analysis.rmd:
if(F)
  file.edit("./apps_supplementary/v4_prolifico_100/_analysis.rmd")

.u = "in"
.w = 6.25
.h = 9
.l_lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")


#### Load and plot as demographic heatmap ----
## Load aggregated data. filter to only surveys in the 108 instances in the analysis
survey_wider <- readRDS("./data/survey_wider.rds")
instance_id_whitelist <- readRDS("./data/instance_id_whitelist.rds")
## Only Prolific participants that were in the 108 instance_ids in the analysis
survey_wider <- survey_wider %>%
  dplyr::filter(nchar(as.character(prolific_id)) == 24,
         instance_id %in% instance_id_whitelist)
str(survey_wider)

## Change character to factor, include counts in the levels of sex?
(demographic_heatmaps <- ggplot(survey_wider, aes(education, age)) +
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
  ggsave(filename = "./figures/ch4_zapp_fig1_survey_demograpics.pdf",
         plot = demographic_heatmaps, device = "pdf", width = .w, height = .w/2)


## Subjective measures, Boxplots -----
survey_wider <- readRDS("./data/survey_wider.rds")
str(survey_wider)

{
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
## Technically not continuous numeric, will show side by side with Likert plot.
.lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")
subjective_longer <- subjective_longer %>%
  mutate(value = as.integer(plyr::mapvalues(value, from = .lvls, to = 1L:5L)),
         measure = factor(plyr::mapvalues(measure,
                                          from = c("like", "ease", "confidence", "familar"),
                                          to = c("preference", "ease of use", "confidence", "familiarity"))),
         factor = factor(factor, levels = c("pca", "grand", "radial"))
  )

## 4 panes with {ggpubr}, ggviolin or ggboxplot with tests
require("ggpubr")
my_theme <- list(
  theme_bw(),
  scale_color_brewer(palette = "Dark2"),
  scale_fill_brewer(palette = "Dark2"),
  geom_hline(yintercept = 0L),
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(-6)))
my_ggpubr <- function(df, x = "factor", y = "value", title = waiver(), subtitle = waiver()){
  ## Find height of global significance test text.
  .x_lvls <- df %>% dplyr::pull({{x}}) %>% levels()
  .y_range <- diff(range(df[y]))
  .n_lvls <- length(.x_lvls)
  .lab.y <- (.04 * .y_range) * (1 + .n_lvls) * .y_range + max(df[y])
  my_comparisons <- list(c("pca", "grand"), c("grand", "radial"), c("pca", "radial"))

  ## Plot
  ggviolin(df, x = x, y = y, fill = x, alpha = .6,
           palette = "Dark2", shape = x, trim = TRUE,
           add = c("mean"), ## Black circle, can change size, but not shape or alpha?
           draw_quantiles = c(.25, .5, .75)) +
    stat_compare_means(method = "wilcox.test",
                       comparisons = my_comparisons,
                       label = "p.signif", hide.ns = TRUE) + ## pairwise test
    # stat_compare_means(label = "p.signif", label.y = .lab.y - .4,
    #                    method = "wilcox.test", ref.group = .x_lvls[1]) + ## Test each lvl w.r.t. first level.
    stat_compare_means( ## Global test
      label.y = .lab.y,
      aes(label = paste0("p=", ..p.format..))
    ) + ## custom label
    my_theme +
    ggtitle(title, subtitle)
}
my_ggpubr_facet <- function(..., facet = "measure"){
  facet(my_ggpubr(...), facet.by = facet)
}
(measure_violins <- my_ggpubr_facet(df = subjective_longer, x = "factor", y = "value")
  + labs(x = "Factor", y = "Response", fill = "Factor"))


#### Subjective measures, Likert plots -----
### Try to create my own likert barplots and signif tables: likert and this seem to want preaggregated format
length(unique(survey_wider$instance_id))

## assumes df obj survey_wider, already filtered to whitelisted 108.
## creates df obj survey_agg and likert, a subset
# script_survey_wider_to_survey_agg <- function(col_idx = 8:22){
{
  .l_lvls <- c("most disagree", "disagree", "neutral", "agree", "most agree")
  col_idx <- 8:22
  col_nms <- colnames(survey_wider[, col_idx])
  survey_agg <- tibble()
  mute <- sapply(col_nms, function(col_nm){
    tmp <- survey_wider[col_nm] %>%
      dplyr::group_by_all() %>%
      dplyr::count() %>%
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
  likert <<- survey_agg %>% dplyr::filter(question %in% likert_q_nms) %>%
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
# script_survey_wider_to_survey_agg()

# Stacked + percent
(subjectiveMeasures <-
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
    scale_x_continuous(labels = scales::percent) +
    coord_flip() +
    theme(legend.direction = "vertical") +
    guides(fill = guide_legend(reverse = FALSE)) +
    labs(x = "Factor", y = "Response rate", fill = "Response")
)



### SAVING ------
## Cowplot and bringing it together
require("cowplot")

if(F){
  figSubjectiveMeasures_w.violin_hori <-  cowplot::plot_grid(
    subjectiveMeasures, measure_violins, ncol = 2)
  ggsave("./figures/ch4_fig6_subjective_measures.pdf",
         figSubjectiveMeasures_w.violin_hori, device = "pdf",
         width = .w, height = .w * 1, units = "in")
}
