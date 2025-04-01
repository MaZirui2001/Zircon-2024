run:
	@IMG=$(IMG) sbt 'testOnly EmuMain' --batch
	# @IMG=$(IMG) mill -s _.test.testOnly EmuMain
	


