#!/bin/bash
#SBATCH --job-name=hpci-example-job
#SBATCH --output=hpci-example-job.log

set -eu -o pipefail

# Define the target directory name
TARGET_DIR="${REPO_NAME}-${COMMIT_SHA}"
git clone "$REPO_URL" "$TARGET_DIR"
cd "$TARGET_DIR"
git checkout "$COMMIT_SHA"

# Here is where you would run the analysis you want to test

echo "Working Dir: $(pwd); Sbatch version: $(sbatch --version)"
# Optional clean up
cd ..
rm -rf "${TARGET_DIR}"

echo "SUCCESS"
