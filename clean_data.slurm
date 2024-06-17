#!/bin/bash

#SBATCH --job-name=cleandata
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=50G
#SBATCH --time=00:30:00
#SBATCH --partition=chang,preemptable,short-cpu,day-long-cpu
#SBATCH --output=/scratch/thsiao3/output/cleandata_%A_%a.out
#SBATCH --error=/scratch/thsiao3/error/cleandata_%A_%a.err

# Load necessary modules or set any other environment variables if needed
module load singularity
# navigate to directory
cd /projects/waller/spatial_kiosks

# Path to the Singularity image
SINGULARITY_IMAGE=~/singularity-images/geospatial_4.2.2.sif

# Path to the R script
R_SCRIPT=clean_data.R

# Launch Singularity Job
srun singularity exec --bind /projects:/projects --bind /scratch:/scratch $SINGULARITY_IMAGE Rscript $R_SCRIPT
