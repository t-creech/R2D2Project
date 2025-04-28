# Reproducible workflow for the project
N_SEED=1234
Rscript code/install_packages.R
Rscript code/00_eda.R data/gambia_data.csv data/breast_tumors_mixOmics.csv figures/EDA
Rscript code/01_prior_construction.R
Rscript code/02_gambia.R data/gambia_data.csv $N_SEED results/gambia
Rscript code/03_breast_cancer.R data/breast_tumors_mixOmics.csv $N_SEED results/breast_cancer
Rscript code/05_gambia2_supp.R data/gambia_data.csv $N_SEED results/gambia2
Rscript code/06_figs_supp.R results/gambia2 results/breast_cancer $N_SEED figures/SuppFigures
Rscript code/07_breast_cancer_supp.R data/breast_tumors_mixOmics.csv $N_SEED results/breast_cancer2
Rscript code/04_figures.R results/gambia results/breast_cancer $N_SEED figures/PaperFigures