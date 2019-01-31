EXEC_NAME = ambi
OBJ_DIR = obj

.PHONY : clean build

clean:
	rm -f $(EXEC_NAME)
	rm -rf $(OBJ_DIR)

build:
	mkdir -p $(OBJ_DIR)
	gprbuild ambi.gpr
