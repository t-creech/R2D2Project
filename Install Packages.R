packages <- c("geoR", "devtools")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}
lapply(packages, install_if_missing)
lapply(packages, library, character.only = TRUE)

# write.csv(gambia, file = "gambia_data.csv", row.names = FALSE)

install_github("eyanchenko/r2d2glmm")