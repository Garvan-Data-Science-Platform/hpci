# Makefile for hpci
REGION:=australia-southeast1
REGISTRY:=$(REGION)-docker.pkg.dev/$(PROJECT)/docker/
IMAGE:=pbs
DOCKER_TAG:=$(REGISTRY)$(IMAGE):latest

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
test: ## Compile HPCI and test with dockerised OpenPBS (requires `make run` first)
	cabal run exes -- \
		--user pbsuser \
		--host localhost \
		--port 2222 \
		--publicKey test_key.pub \
		--privateKey test_key \
		--script ci/test_job.pbs \
		--logFile test_job.log

.PHONY: test-bin
test-bin: ## Test HPCI binary and test with dockerised OpenPBS (requires `make run`, and `nix build .#packages.x86_64-linux.hpci` first)
	result/bin/hpci-exe \
		--user pbsuser \
		--host localhost \
		--port 2222 \
		--publicKey test_key.pub \
		--privateKey test_key \
		--script ci/test_job.pbs \
		--logFile test_job.log

.PHONY: stop
stop: ## Stop the running docker container
	docker stop $(IMAGE)

.PHONY: help
help: ## Display available commands
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk \
		'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
