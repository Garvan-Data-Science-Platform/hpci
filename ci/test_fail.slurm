#! /bin/bash
#SBATCH --job-name=testjob
#SBATCH --account=NON_EXISTENT_ACCOUNT

set -eu -o pipefail

echo "Sbatch version: $(sbatch --version)" > test_job.log 2>&1
