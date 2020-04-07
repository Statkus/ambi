PROG_NAME = ambi
OBJ_DIR = obj

IMAGE_NAME = ambi_image
CONTAINER_NAME = ambi_container
FORMAT_CONTAINER_NAME = ambi_format_container

FILE =

.PHONY : build build-test run-test test clean build-server-image start-server-image stop-server-image remove-server-image format-file

build:
	mkdir -p $(OBJ_DIR)
	gprbuild $(PROG_NAME).gpr

build-test:
	mkdir -p $(OBJ_DIR)
	gprbuild $(PROG_NAME)_test.gpr

run-test:
	$(OBJ_DIR)/$(PROG_NAME)_test

test: build-test run-test

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

format-file:
	@docker run -v $(CURDIR):/home/ubuntu/ambi --rm --name $(FORMAT_CONTAINER_NAME) $(IMAGE_NAME) /bin/bash -c "gnatpp -pipe -P $(PROG_NAME)_test.gpr $(FILE)"
