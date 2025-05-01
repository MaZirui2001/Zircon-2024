PWD := $(shell pwd)
SCALA_SRC := $(shell find src -name "*.scala")
all: verilog

verilog: $(SCALA_SRC)
	@BUILD_MODE=SYNC ./mill -s -j0 _.runMain Main


run:
	# @IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir BUILD_MODE=sim sbt 'testOnly EmuMain' --batch
	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir BUILD_MODE=sim ./mill -s -j0 _.test.testOnly EmuMain
	
