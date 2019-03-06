PROG_NAME = ambi
OBJ_DIR = obj

IMAGE_NAME = ambi_image
CONTAINER_NAME = ambi_container

.PHONY : clean build build-server-image start-server-image stop-server-image remove-server-image

build:
	mkdir -p $(OBJ_DIR)
	gprbuild $(PROG_NAME).gpr

clean:
	rm -f $(PROG_NAME)
	rm -rf $(OBJ_DIR)

build-server-image:
	docker build -t $(IMAGE_NAME) .

start-server-image:
	docker run -p 80:80 -dit -v $(CURDIR):/home/ubuntu/ambi --rm --name $(CONTAINER_NAME) $(IMAGE_NAME)
	docker exec -dit -w /home/ubuntu/ambi $(CONTAINER_NAME) /bin/bash -c "make build && ./ambi &> server_log.txt"

stop-server-image:
	docker stop $(CONTAINER_NAME)

remove-server-image:
	docker rmi $(IMAGE_NAME)
