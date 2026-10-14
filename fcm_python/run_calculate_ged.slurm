#!/usr/bin/bash -l
#SBATCH -p batch
#SBATCH -J calculate_ged
#SBATCH -w compute06
#SBATCH -n 4
#SBATCH --output=ged_output.txt
#SBATCH --error=ged_error.txt

# load the Python 3.10 module
module load python/3.10

# Set the working directory
cd /home/sthomas/fcm

# activate the virtual environment
source venv/bin/activate

# run your Python script
python calculate_ged.py