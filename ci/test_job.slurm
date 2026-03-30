#! /bin/bash
#SBATCH --job-name=testjob

set -eu -o pipefail

echo "Sbatch version: $(sbatch --version)" > test_job.log 2>&1
