EXEC_NAME = main
OBJ_DIR = obj

.PHONY : clean build

clean:
	rm $(EXEC_NAME)
	rm -rf $(OBJ_DIR)

build:
	mkdir $(OBJ_DIR)
	gprbuild ambi.gpr
