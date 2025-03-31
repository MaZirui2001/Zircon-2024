run:
	@IMG=$(IMG) sbt 'testOnly EmuMain' --batch
	# @export CC="ccache clang" && export CXX="ccache clang++" && IMG=$(IMG) mill -s _.test.testOnly EmuMain
	


