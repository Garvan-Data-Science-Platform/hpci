DOCKER_TAG=pbs-hpci

docker:
	docker build --platform linux/amd64 -t $(DOCKER_TAG) test/integration
