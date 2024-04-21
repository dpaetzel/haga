#!/usr/bin/env bash
#SBATCH --time=01:30:00
#SBATCH --partition=cpu
#SBATCH --array=0-1
#SBATCH --output=./output/output_run_%j.txt
#SBATCH --error=./output/error_run_%j.txt
#SBATCH --nodelist=oc-compute02
#SBATCH --mem=8G
srun nix develop --command stack --no-nix --system-ghc --no-install-ghc run haga
