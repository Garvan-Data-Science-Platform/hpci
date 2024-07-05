IMAGE=pbs
DOCKER_TAG=$(IMAGE)-hpci

docker:
	docker build -t $(DOCKER_TAG) ci

# requires creating an ssh key in the `ci` directory
run:
	docker run \
	-d --rm \
	-p 2222:22 \
	--name $(IMAGE) \
	-h pbs_container \
	-v ./ci/test_key.pub:/tmp/.ssh/authorized_keys:ro \
	$(DOCKER_TAG) /bin/bash -l /run.sh
	
interact:
	docker exec -it --user pbsuser $(IMAGE) /bin/bash -l

stop:
	docker stop $(IMAGE)
