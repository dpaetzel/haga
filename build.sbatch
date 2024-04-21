#!/usr/bin/env bash
#SBATCH --time=00:10:00
#SBATCH --partition=cpu
#SBATCH --output=./run/output_build.txt
#SBATCH --error=./run/error_build.txt
nix develop --command "stack --nix build"
