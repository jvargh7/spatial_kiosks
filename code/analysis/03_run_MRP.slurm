#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=500M
#SBATCH --time=1:00:00
#SBATCH --array=1-16
#SBATCH --partition=chang,preemptable,short-cpu
#SBATCH --output=/scratch/thsiao3/output/MRP_%A_%a.out
#SBATCH --error=/scratch/thsiao3/error/MRP_%A_%a.err

# Load necessary modules or set any other environment variables if needed
module load singularity
# navigate to directory
cd /projects/waller/spatial_kiosks/code/analysis

# Path to the Singularity image
SINGULARITY_IMAGE=~/singularity-images/geospatial_4.4.1.sif

# Path to the R script
R_SCRIPT=03_run_MRP.R

# Launch Singularity Job
srun singularity exec --bind /projects:/projects --bind /scratch:/scratch $SINGULARITY_IMAGE Rscript $R_SCRIPT $1
