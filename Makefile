PWD := $(shell pwd)
run:
	# @IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir sbt 'testOnly EmuMain' --batch
	@IMG=$(IMG) TEST_DIR=$(PWD)/test_run_dir ./mill -s -j0 _.test.testOnly EmuMain
	


