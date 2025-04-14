PWD := $(shell pwd)
SCALA_SRC := $(shell find src -name "*.scala")
all: build

build: $(SCALA_SRC)
	./mill -s -j0 _.runMain Main

run:
	# @IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir sbt 'testOnly EmuMain' --batch
	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir ./mill -s -j0 _.test.testOnly EmuMain
	


