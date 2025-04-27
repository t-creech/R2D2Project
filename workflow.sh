# Reproducible workflow for the project
Rscript code/install_packages.R
Rscript code/00_eda.R data/gambia_data.csv data/breast_tumors_mixOmics.csv figures/EDA
Rscript code/01_prior_construction.R
Rscript code/04_figures.R ph1 ph2 figures/PaperFigures
