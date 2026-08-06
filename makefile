# Makefile for hpci

.PHONY: help
help: ## Display available commands
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk \
		'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

EXEC_COMMAND:=pwd

SCHEDULE_PBS_ARGS=--user pbsuser \
			  --host 127.0.0.1 \
			  --port 2222 \
			  --publicKey test_key.pub \
			  --privateKey test_key \
			  schedule \
			  --script ci/test_job.pbs \
			  --logFile test_job.log \
              --scheduler-arg "-q workq" \
              --scheduler-arg "-l 'walltime=01:30:00'" \
			  -c TEST_VAR1=success,TEST_VAR2=double_success
              # --scheduler-arg "-N testjob" \

SCHEDULE_SLURM_ARGS=--user root \
			  --host 127.0.0.1 \
			  --port 2223 \
			  --publicKey test_key.pub \
			  --privateKey test_key \
			  schedule \
			  --scheduler slurm \
			  --script ci/test_job.slurm \
			  --logFile test_job.log \
              --scheduler-arg "--partition=all" \
              --scheduler-arg "--job-name='Build&Test'" \
			  -c TEST_VAR1=success,TEST_VAR2=double_success

EXEC_ARGS=--user pbsuser \
			--host 127.0.0.1 \
			--port 2222 \
			--publicKey test_key.pub \
			--privateKey test_key \
			exec $(EXEC_COMMAND)

.PHONY: up-d
up-d: ## Run dockerised OpenPBS and Slurm containers (This requires creating an ssh key called `test_key` in the root of the `hpci` directory). Includes `--detach` flag so containers run in the background
	docker compose -f ci/docker-compose.yml up -d

.PHONY: up
up: ## Run dockerised OpenPBS and Slurm containers (This requires creating an ssh key called `test_key` in the root of the `hpci` directory). Does not include `--detach` flag, so you can see container logs.
	docker compose -f ci/docker-compose.yml up

.PHONY: interact-pbs
interact-pbs: ## Start interactive terminal access to running pbs docker container
	docker exec -it --user pbsuser pbs bash

.PHONY: interact-slurm
interact-slurm: ## Start interactive terminal access to running slurm docker container
	docker exec -it slurm bash

.PHONY: down
down: ## Stop the running docker container
	docker compose -f ci/docker-compose.yml down

.PHONY: test
test: test-schedule test-exec ## Run tests for `schedule` (for both OpenPBS and Slurm) and `exec`. Requires `make up` first.

.PHONY: test-schedule
test-schedule: test-pbs-schedule test-slurm-schedule ## Run tests for `schedule` (for both OpenPBS and Slurm). Requires `make up` first.

.PHONY: test-pbs-schedule
test-pbs-schedule: ## Compile `hpci` and test the schedule command with dockerised OpenPBS (requires `make up` first)
	cabal run exes -- $(SCHEDULE_PBS_ARGS)

.PHONY: test-slurm-schedule
test-slurm-schedule: ## Compile `hpci` and test the schedule command with dockerised Slurm (requires `make up` first)
	cabal run exes -- $(SCHEDULE_SLURM_ARGS)

.PHONY: test-exec
test-exec: ## Compile `hpci` and test the exec command with dockerised OpenPBS (requires `make up` first)
	cabal run exes -- $(EXEC_ARGS)

.PHONY: test-bin
test-bin: test-bin-schedule test-bin-exec # Run tests on binary

.PHONY: test-bin-schedule
test-bin-schedule: test-bin-pbs-schedule test-bin-slurm-schedule

.PHONY: test-bin-pbs-schedule
test-bin-pbs-schedule: ## Test `hpci` binary and test schedule command with dockerised OpenPBS (requires `make up`, and `make build-linux` first)
	result/bin/hpci-exe $(SCHEDULE_PBS_ARGS)

.PHONY: test-bin-slurm-schedule
test-bin-slurm-schedule: ## Test `hpci` binary and test schedule command with dockerised Slurm (requires `make up`, and `make build-linux` first)
	result/bin/hpci-exe $(SCHEDULE_SLURM_ARGS)

.PHONY: test-bin-exec
test-bin-exec: ## Test `hpci` binary and test exec command with dockerised OpenPBS (requires `make up`, and `make build-linux` first)
	result/bin/hpci-exe $(EXEC_ARGS)

.PHONY: build-linux
build-linux: ## Build fully-static binary on linux x86_64
	nix build .#packages.x86_64-linux.hpci

.PHONY: build-darwin
build-darwin: ## Build dynamically-linked binary on aarch64-darwin
	cabal build

###### Not for general use ######
# The following commands do not appear in `make help` as they are not for general use
# They are used to develop, build and push docker images used in CI for testing `hpci`
# Also for pushing a binary to a cloud artifact registry

REGISTRY:=ghcr.io/garvan-data-science-platform/
IMAGE:=pbs
DOCKER_TAG:=$(REGISTRY)$(IMAGE):latest

.PHONY: docker
## Build a docker image. Only works on x86_64-linux.
docker:
	docker buildx build \
		--platform linux/amd64,linux/arm64 \
		-t $(DOCKER_TAG) -f ci/Dockerfile ci

.PHONY: pull
## Pull a docker image from artifact registry (useful on non-x86_64 machines).
pull:
	docker pull --platform linux/amd64 $(DOCKER_TAG)
