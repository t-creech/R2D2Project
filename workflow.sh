# Reproducible workflow for the March‑Madness project

N_REPS=500

# 0. Start fresh: delete old simulation output
if [ -d results ]; then
  echo "Removing old folders"
  rm -rf results
  rm -rf figures
  rm -rf data_temp
fi
mkdir -p results
mkdir -p figures
mkdir -p data_temp

# 1. Exploratory data analysis
Rscript code/01_eda.R data/gambia_data.csv  figures/EDA/

# 3. Simulation study replications
for seed in $(seq 1 "$N_REPS"); do
  echo "Running replication $seed / $N_REPS"
  Rscript code/run_file.R "$seed"
done

# 4. Aggregate results
Rscript code/out_file.R  results/  figures/Simulation/

# 5. Fit model on real data
Rscript code/03_fit_real.R data_clean/games.csv results/ figures/Model/
