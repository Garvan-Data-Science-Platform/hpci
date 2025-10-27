# hpci

> *You can't spell reprodu***ci***bility without CI*

`hpci` is a tool to integrate Continuous Integration (CI) with High Performance Compute (HPC).

Install `hpci` on your CI runner, and `hpci` can schedule and monitor jobs on HPC.
When you job has finished, the job's exit status code is recorded.

`hpci` will copy log files from HPC to the CI runner, print these to the CI logs.
Then `hpci` will exit will the same exit status code as the job on HPC, therefore CI will crash if the job on HPC fails.

`hpci` is designed to be easily used with different CI platforms and different HPC clusters.

## How to use `hpci`

1. Download `hpci` to your CI runner.
You can either add `hpci` to your deployed runner vm, or include a download step as part of your CI workflow.
You can also download it locally for testing, but it is intended to be used on CI runner.

```
  wget https://github.com/Garvan-Data-Science-Platform/hpci/releases/download/v0.0.5/hpci-exe
```

Either add the binary to your path, or download to a specific location by using wget with `--directory-prefix=`.
For this README, I'll use `/usr/local/bin/` as example location

2. Ensure binary is executable

```
chmod +x /usr/local/bin/hpci-exe
```

3. Run `hpci`

`/usr/local/bin/hpci-exe` followed by the following required arguments:
  - --user       *username on remote system*
  - --host       *IP address or domain name or remote host*
  - --port       *port declaration - typically 22*
  - --publickey  *local filepath for ssh public key*
  - --privatekey *local filepath for ssh private key*
  schedule       *to schedule a job on HPC* 
  - --script     *local filepath of job scheduler script*
  - --logFile    *remote filepath of logfile produced by job scheduler script to copy back to local system*
  - -c           ***Optional*** *Configuration in the form of `KEY1=VALUE1,KEY2=VALUE2` that is passed to job scheduler when submitting the job*

## Security

I recommend creating an ssh key specifically for use with `hpci` so you can easily revoke access, and cycle keys.

Other steps you can take to increase security:
  - use self-hosted, ephemeral runners
  - use CI secrets for as many variables as possible (e.g. username, host)
  - use a cloud secret manager to store ssh-keys 

## Acknowlegements

This repo was initialised with [template-haskell](https://github.com/jonascarpay/template-haskell)
