#!/usr/bin/env bash
#SBATCH --time=00:10:00
#SBATCH --partition=cpu
#SBATCH --output=./output/output_build.txt
#SBATCH --error=./output/error_build.txt
#SBATCH --nodelist=oc-compute02
#SBATCH --mem=4G
#SBATCH -c16
srun nix develop --command stack --no-nix --system-ghc --no-install-ghc build
