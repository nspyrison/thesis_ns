library(spinifex)
load("./data/jetsProj_sub.rda") # fig 7, subset of ATLAS7old and ATLAS7new
load("./data/PDFSense_fig7_basis.rda") # fi 7
load("./data/grDIScenter.rda") # fig 8, left
load("./data/grDISsphere.rda") # fig 8, right
load("./data/PDFSense_fig8l_basis.rda") # fig 8, left
# load("./data/PDFSense_fig8r_basis.rda") # fig 8, right

# Fig 7, subset of ATLAS7old and ATLAS7new
view_basis(fig7_basis)
play_manual_tour(data = jetsProj_sub[, 1:4], basis = fig7_basis, manip_var = 3,
                 cat_var = jetsProj_sub$exp)
play_manual_tour(data = jetsProj_sub[, 1:4], basis = fig7_basis, manip_var = 4,
                 cat_var = jetsProj_sub$exp)

# Fig 8,
view_basis(fig8l_basis)
play_manual_tour(data = grDIScenter[, 1:6], basis = fig8l_basis, manip_var = 6,
                 cat_var = grDIScenter$disID)
play_manual_tour(data = grDIScenter[, 1:6], basis = fig8l_basis, manip_var = 5,
                 cat_var = grDIScenter$disID)
