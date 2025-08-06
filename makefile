# Makefile for hpci
REGION:=australia-southeast1
REGISTRY:=$(REGION)-docker.pkg.dev/$(PROJECT)/docker/
IMAGE:=pbs
DOCKER_TAG:=$(REGISTRY)$(IMAGE):latest
EXEC_COMMAND:=pwd

SCHEDULE_ARGS=--user pbsuser \
			  --host 127.0.0.1 \
			  --port 2222 \
			  --publicKey test_key.pub \
			  --privateKey test_key \
			  schedule \
			  --script ci/test_job.pbs \
			  --logFile test_job.log \
			  -c TEST_VAR1=success,TEST_VAR2=double_success

EXEC_ARGS=--user pbsuser \
			--host 127.0.0.1 \
			--port 2222 \
			--publicKey test_key.pub \
			--privateKey test_key \
			exec $(EXEC_COMMAND)

.PHONY: docker
docker: ## Build a docker image. Only works on x86_64-linux. Provide PROJECT argument on commandline (e.g. `make PROJECT=blah docker`).
	docker build -t $(DOCKER_TAG) ci

.PHONY: pull
pull: ## Pull a docker image from artifact registry (useful on non-x86_64 machines. Provide PROJECT argument on commandline (e.g. `make PROJECT=blah pull`).
	docker pull --platform linux/amd64 $(DOCKER_TAG)

.PHONY: run
run: ## Start a OpenPBS server and ssh server inside docker container (This requires creating an ssh key called `test_key` in the root of the `hpci` directory). Provide PROJECT argument on commandline (e.g. `make PROJECT=blah run`).
	docker run \
	--platform linux/amd64 \
	-d \
	--rm \
	-p 2222:22 \
	--name $(IMAGE) \
	-h pbs_container \
	-v ./test_key.pub:/tmp/authorized_keys:ro \
	$(DOCKER_TAG)

.PHONY: interact
interact: ## Start interactive terminal access to running docker container
	docker exec -it --user pbsuser $(IMAGE) bash

.PHONY: test
test: test-schedule test-exec # Run cabal tests

.PHONY: test
test-schedule: ## Compile HPCI and test the schedule command with dockerised OpenPBS (requires `make run` first)
	cabal run exes -- $(SCHEDULE_ARGS)

.PHONY: test-exec
test-exec: ## Compile HPCI and test the exec command with dockerised OpenPBS (requires `make run` first)
	cabal run exes -- $(EXEC_ARGS)

.PHONY: test-bin
test-bin: test-bin-schedule test-bin-exec # Run tests on binary

.PHONY: test-bin-schedule
test-bin-schedule: ## Test HPCI binary and test schedule command with dockerised OpenPBS (requires `make run`, and `nix build .#packages.x86_64-linux.hpci` first)
	result/bin/hpci-exe $(SCHEDULE_ARGS)

.PHONY: test-bin-exec
test-bin-exec: ## Test HPCI binary and test exec command with dockerised OpenPBS (requires `make run`, and `nix build .#packages.x86_64-linux.hpci` first)
	result/bin/hpci-exe $(EXEC_ARGS)

.PHONY: build
build: ## Build fully-static binary on linux x86_64
	nix build .#packages.x86_64-linux.hpci

.PHONY: push-bin
push-bin: ## Push binary to gcp artifact registry - requires VERSION
	gcloud artifacts generic upload \
		--location=australia-southeast1 \
		--source=result/bin/hpci-exe \
		--package=hpci \
		--version=$(VERSION) \
		--repository=generic

.PHONY: delete-bin
delete-bin: ## Delete binary from gcp artifact registry - requires VERSION
	gcloud artifacts versions delete $(VERSION)\
		--location=australia-southeast1 \
		--package=hpci \
		--repository=generic

.PHONY: stop
stop: ## Stop the running docker container
	docker stop $(IMAGE)

.PHONY: help
help: ## Display available commands
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk \
		'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
