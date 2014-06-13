## This file is distributed under the BSD 3-clause license.  See file LICENSE

F = fsc -feature -deprecation -Xfatal-warnings
S = scalac -feature -deprecation -Xfatal-warnings
X = scala -feature -deprecation -Xfatal-warnings

default: Kse.jar
KSE_JAR_PATH = kse/flow

Kse.jar : \
  kse/flow/Ok.class
	jar cf Kse.jar ${KSE_JAR_PATH}

kse/flow/Ok.class : \
  makefile \
  flow/Ok.scala
	${F} flow/Ok.scala

clean :
	${F} -shutdown; rm -r ${KSE_JAR_PATH}
