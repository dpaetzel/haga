#!/usr/bin/env bash
#SBATCH --time=06:30:00
#SBATCH --partition=cpu
#SBATCH --array=0-30
#SBATCH --output=./output/output_run_%j.txt
#SBATCH --error=./output/error_run_%j.txt
#SBATCH --nodelist=oc-compute02
#SBATCH --mem=3G
srun nix develop --command stack --no-nix --system-ghc --no-install-ghc run haga
