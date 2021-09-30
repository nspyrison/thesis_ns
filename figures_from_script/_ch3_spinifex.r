# Setup ------
if(F)
  remotes::install_github("nspyrison/spinifex")
require(spinifex) ## Current dev ver, likely a small diff from CRAN spinifex v0.3.1
require(tourr)
require(ggplot2)
require(gridExtra)
require(dplyr)

# fig1_biplot -----

## Flea holes tour
set.seed(20190425) ## Doesn't change tourr output.
f_dat <- tourr::rescale(flea[,1:6])
f_clas <- factor(flea$species)
## Hard code a basis. tourr doesn't fit results with set.seed().
f_bas <- c(.693, -.022, .082, -.119, .706, .023,
           -.070, .438, .405, .515, .103, .604) %>%
  matrix( ncol=2) %>%
  tourr::orthonormalise()
rownames(f_bas) <- colnames(f_dat)

biplot <- ggtour(f_bas, f_dat) +
  proto_default(list(color = f_clas, shape = f_clas))

ggplot2::ggsave(
  "./figures_from_script/ch3_fig1_biplot.pdf", biplot, "pdf",
  height=3, scale=1, units="in")

# fig2_manip_sp -----
f_mvar <- 5
manip_sp <- spinifex::view_manip_space(basis = f_bas, manip_var = f_mvar)

ggplot2::ggsave(
  "./figures_from_script/ch3_fig2_manip_sp.pdf", manip_sp, "pdf",
  height=3, scale=1, units="in")


# fig3_filmstrip -----
## All arguments
mt <- manual_tour(basis = f_bas, manip_var = f_mvar)
ggt <- ggtour(mt, f_dat, angle = 3) +
  proto_default(list(color = f_clas, shape = f_clas))
film <- filmstrip(ggt)

ggplot2::ggsave(
  "./figures_from_script/ch3_fig3_filmstrip.pdf", film, "pdf",
  width=8, scale=1, units="in")





#fig4_jet_better_pc4 -----
load("./data/JetCluster_sub.rda")
load("./data/JetCluster_basis.rda")
jet_dat   <- tourr::rescale(JetCluster_sub[, 1:4])
jet_bas   <- JetCluster_basis
rownames(jet_bas) <- colnames(jet_dat)
jet_clas  <- factor(JetCluster_sub$exp)
jet_mvar  <- 4
jet_ang   <- .315 ## gives 12 frames
jet_mtour <- manual_tour(basis = jet_bas, manip_var = jet_mvar)
ggt <- ggtour(jet_mtour, jet_dat, angle = pi*3) +
  proto_default(list(color = jet_clas, shape = jet_clas),
                list(alpha = .5))
fig4_jet_better_pc4 <- filmstrip(ggt)

ggplot2::ggsave(
  "./figures_from_script/ch3_fig4_jet_better_pc4.pdf",
  fig4_jet_better_pc4, "pdf",
  width=8, scale=1, units="in")

#fig5_jet_worse_pc3 -----
jet_mvar  <- 3
jet_mtour <- manual_tour(basis = jet_bas, manip_var = jet_mvar)
ggt <- ggtour(jet_mtour, jet_dat, angle = pi*3) +
  proto_default(list(color = jet_clas, shape = jet_clas),
                list(alpha = .5))
fig5_jet_worse_pc3 <- filmstrip(ggt)

ggplot2::ggsave(
  "./figures_from_script/ch3_fig5_jet_worse_pc3.pdf",
  fig5_jet_worse_pc3, "pdf",
  width=8, scale=1, units="in")

# fig6_DIS_better_pc6 -----
load("./data/grDIScenter.rda")
load("./data/DIScluster_centered_basis.rda")
DIS_dat   <- tourr::rescale(grDIScenter[, 1:6])
DIS_bas   <- DIScluster_centered_basis
rownames(DIS_bas) <- colnames(DIS_dat)
DIS_clas  <- factor(grDIScenter$disID,
                    labels = c("DIS HERA1+2", "dimuon SIDIS", "charm SIDIS"))
DIS_mvar  <- 6
DIS_ang   <- .32
DIS_mtour <- manual_tour(basis = DIS_bas, manip_var = DIS_mvar)

ggt <- ggtour(DIS_mtour, DIS_dat, angle = pi*3) +
  proto_default(list(color = DIS_clas, shape = DIS_clas),
                list(alpha = .5))
fig6_DIS_better_pc6 <- filmstrip(ggt)
ggplot2::ggsave(
  "./figures_from_script/ch3_fig6_DIS_better_pc6.pdf",
  fig6_DIS_better_pc6, "pdf",
  width=8, scale=1, units="in")

# fig7_DIS_worse_pc2 -----
DIS_mvar <- 2
DIS_mtour <- manual_tour(basis = DIS_bas, manip_var = DIS_mvar)

ggt <- ggtour(DIS_mtour, DIS_dat, angle = pi*3) +
  proto_default(list(color = DIS_clas, shape = DIS_clas),
                list(alpha = .5))
fig7_DIS_worse_pc2 <- filmstrip(ggt)
ggplot2::ggsave(
  "./figures_from_script/ch3_fig7_DIS_worse_pc2.pdf",
  fig7_DIS_worse_pc2, "pdf",
  width=8, scale=1, units="in")
