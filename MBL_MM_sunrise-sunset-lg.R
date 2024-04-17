#Linear regression analysis (Dependent Var = #of interactions; Independent Var = time difference to sunrise/sunset)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("rhdf5")
install.packages("bioRad")

library(bioRad)
?sunrise

?sunset
