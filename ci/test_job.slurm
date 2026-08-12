#! /bin/bash
#SBATCH --job-name=testjob
#SBATCH --output=test_job.log

set -eu -o pipefail

echo "Sbatch version: $(sbatch --version)"
