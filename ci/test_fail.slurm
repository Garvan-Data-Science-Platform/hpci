#! /bin/bash
#SBATCH --job-name=testjob
#SBATCH --partition=NON_EXISTENT_PARTITION

set -eu -o pipefail

echo "Sbatch version: $(sbatch --version)" > test_job.log 2>&1
