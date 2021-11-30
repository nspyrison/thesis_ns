require("ggplot2")
require("GGally")
require("spinifex")
require("cheem")
h <- w <- 4.2

dat <- spinifex::penguins[, 1:5]
colnames(dat) <- c("bill length", "bill depth", "flipper length", "body mass", "species")
#knitr::kable(head(dat), format = 'html')

X <- dat[, 1:4]
Y <- dat$species
gg <- GGally::ggpairs(X, upper = "blank",
                mapping = ggplot2::aes(color = Y, shape = Y)) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

ggsave("./figures_from_script/ch2_fig1_penguin_splom.pdf", gg, device = "pdf",
       width = w, height = h, units = "in")


gg2 <- GGally::ggparcoord(
  dat, columns = c(1:4), groupColumn = 5) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x="", y="") +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggsave("./figures_from_script/ch2_fig2_penguin_pcp.pdf", gg2, device = "pdf",
       width = w, height = h, units = "in")
