PWD := $(shell pwd)
SCALA_SRC := $(shell find src -name "*.scala")
all: verilog

USE_SBT := 0

verilog: $(SCALA_SRC)
ifeq ($(USE_SBT), 1)
	@BUILD_MODE=SYNC sbt 'runMain Main' --batch
else
	@BUILD_MODE=SYNC ./mill -s -j0 _.runMain Main
endif


run:
ifeq ($(USE_SBT), 1)
	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir BUILD_MODE=sim sbt 'testOnly EmuMain' --batch
else
	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir BUILD_MODE=sim ./mill -s -j0 _.test.testOnly EmuMain
endif
	
