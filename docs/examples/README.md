# Examples

This directory contains example Continuous Integration (CI) workflow scripts for GitHub Actions and GitLab CI, as well as example job submission scripts for Slurm and PBS schedulers.

These are just examples.
Feel free to be creative with how you use `hpci`!

## Example job submission scripts

The typical use of `hpci` requires that there is a job submission script within the repository of your analysis code.
This directory has two example job submissions scripts: `hpci-example-job.pbs` and `hpci-example-job.slurm`.

The example job submission scripts use variables from the CI context (e.g. git commit hash, repository url and repository name), passed via `hpci` CI workflow scripts (see above).
These variables are used to create a specifically-named directory on the HPC (`TARGET_DIR="${REPO_NAME}-${COMMIT_SHA}"`).
The script then clones a copy of the analysis code to the target directory on the HPC node; navigates to this directory; and performs a `git checkout` of the target commit.

The script then has a placeholder where it prints the path of the working directory and the version of the HPC scheduler to the output job log file.
This is where you would run the analysis you were wanting to test.

There is then an optional clean up step where the new `TARGET_DIR` is cleaned up.

## Example CI workflow scripts

Example workflow scripts for GitHub Actions and GitLab CI perform similar function, but are tailored to the respective CI systems.
The examples use the hosted CI systems, however they can be easily adjusted to run on self-hosted runners.
Both example workflow scripts allow the CI workflow to be triggered manually via the browser as well as triggering a run on each `git push`.

### CI Secrets

The CI workflows require four variables to be stored in the CI platforms' secret management systems.
See the ['GitLab and Slurm example'](#gitlab-and-slurm-example) and ['GitHub and PBS example'](#github-and-pbs-example) sections below for more details.

### Runner images

The GitLab example runs on the `busybox:latest` docker image as it is lightweight for fast start-up times, and provides required `wget` and `base64` utilities.
The GitHub example runs on the `ubuntu-latest` docker image as this makes it easy to use common actions from the GitHub marketplace actions (such as ['actions/checkout'](https://github.com/marketplace/actions/checkout)).

### Example workflow explanation

Both example CI workflow scripts download the 'latest' `hpci` release from github releases.
You may want to pin this to a particular release version (see code snippet below).

```
# Download the most recent release of hpci
wget https://github.com/Garvan-Data-Science-Platform/hpci/releases/latest/download/hpci-exe

# Or download a specific release version
wget https://github.com/Garvan-Data-Science-Platform/hpci/releases/download/v0.1.4/hpci-exe
```

The workflow scripts then run `chmod` to make the downloaded binary executable.
They then run `./hpci-exe --help` to verify the download and `chmod` steps were successful.

The private and public ssh key secrets are piped from the secrets management system into files on the CI runner.
Another `chmod` command is run to constrain the permissions of the key files.

The workflows then call `hpci-exe` using secret variables for `HPC_USER` and `HPC_HOST`; the newly created key files; and passing three variables of CI context to the job submission scripts (git commit sha, repository url, and repository name).

Both workflow scripts have a clean up step that runs regardless of outcome of the `hpci-exe` command.
This removes the `hpci-exe` binary, and the two key files.

## GitLab and Slurm example

See `gitlab-ci.yml` and `hpci-example-job.slurm`.

To use this example, copy the `gitlab-ci.yml` file to the root of your analysis repository, and rename it to `.gitlab-ci.yml`.

Move `hpci-example-job.slurm` to the root of your analysis repository.

Create an ssh key on your local computer.
Add the public key to the `~/.ssh/authorized_keys` file on your HPC cluster.

Base64 encode your public and private ssh keys (using `base64`) and save the output to the GitLab CI secret manager for your analysis repository as PUBLIC_KEY
and PRIVATE_KEY, respectively.
Encoding your secret variables enables GitLab CI to mask your variable in CI logs.
See [this guidance](https://dev.to/shamdnayeem/securely-store-sensitive-information-in-gitlab-environment-variable-lc0) for more information.

Create HPC_USER and HPC_HOST secret variables (these do not need to be base64 encoded).

Commit and push your changes, and click on 'Build' then 'Pipelines' in the GitLab browser UI to see the progress of your CI run ([link for more information](https://docs.gitlab.com/ci/quick_start/)).

If your HPC scheduler is different to this example, update the `--scheduler` and `--script` arguments in your CI workflow; and ensure you copy the appropriate job submission script to your repository.

## GitHub and PBS example

See `github-actions.yml` and `hpci-example-job.pbs`.

To use this example, create a `.github/workflows` directory in your analysis repository, copy the `github-actions.yml` to this new directory, and rename it to any name you like (e.g. `.github/workflows/hpci-workflow.yml`).

Move `hpci-example-job.pbs` to the root of your analysis repository.

Create an ssh key on your local computer.
Add the public key to the `~/.ssh/authorized_keys` file on your HPC cluster.

Create HPC_USER, HPC_HOST, PRIVATE_KEY, and PUBLIC_KEY secret variables (GitHub secrets masking does not require base64 encryption for keys).
See [this guidance](https://docs.github.com/en/actions/how-tos/write-workflows/choose-what-workflows-do/use-secrets) for more information.

Commit and push your changes, and click on the 'Actions' tab in the GitHub browser UI to see the progress of your CI run.

If your HPC scheduler is different to this example, update the `--scheduler` and `--script` arguments in your CI workflow; and ensure you copy the appropriate job submission script to your repository.
