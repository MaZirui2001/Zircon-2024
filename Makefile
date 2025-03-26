run:
	# @IMG=$(IMG) sbt 'testOnly SimMain'
	@IMG=$(IMG) mill -s _.test.testOnly SimMain
	


