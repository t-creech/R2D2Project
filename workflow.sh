# Reproducible workflow for the project
N_SEED=1234
Rscript code/install_packages.R
Rscript code/00_eda.R data/gambia_data.csv data/breast_tumors_mixOmics.csv figures/EDA
Rscript code/01_prior_construction.R
Rscript code/02_gambia.R data/gambia_data.csv $N_SEED results
Rscript code/03_breast_cancer.R data/breast_tumors_mixOmics.csv $N_SEED results
Rscript code/04_figures.R ph1 ph2 $N_SEED figures/PaperFigures
