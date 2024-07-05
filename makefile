IMAGE=pbs
DOCKER_TAG=$(IMAGE)-hpci

docker:
	docker build -t $(DOCKER_TAG) test/integration

run:
	docker run \
	-d --rm \
	-p 2222:22 \
	--name $(IMAGE) \
	-h pbs_container \
	-v ./test/integration/test_key.pub:/tmp/.ssh/authorized_keys:ro \
	$(DOCKER_TAG) /bin/bash -l /run.sh
	
interact:
	docker exec -it --user pbsuser $(IMAGE) /bin/bash -l

stop:
	docker stop $(IMAGE)
