#!/bin/bash
#SBATCH -J rcsim100      # Job name
#SBATCH -e rcsimu.%j       # Name of stderr error file
#SBATCH -p normal          # Queue (partition) name
#SBATCH -N 1               # Total # of nodes (must be 1 for serial)
#SBATCH -n 1               # Total # of mpi tasks (should be 1 for serial)
#SBATCH -t 12:00:00        # Run time (hh:mm:ss)
#SBATCH --mail-user=bwj9135@uncw.edu
#SBATCH --mail-type=all    # Send email at begin and end of job

# Other commands must follow all #SBATCH directives...
Rscript --verbose ./rcsim100.R> ./output.Rout

