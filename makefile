## This file is distributed under the BSD 3-clause license.  See file LICENSE.
## Copyright (c) 2014 Rex Kerr and UCSF

F = fsc -feature -deprecation -Xfatal-warnings
S = scalac -feature -deprecation -Xfatal-warnings
X = scala -feature -deprecation -Xfatal-warnings
D = scaladoc -d api

default: Kse.jar
KSE_JAR_PATH = kse/flow

Kse.jar : \
  kse/flow/Ok.class \
  kse/flow/Flow.class
	jar cf Kse.jar ${KSE_JAR_PATH}

kse/flow/Ok.class : \
  makefile \
  flow/Ok.scala
	${F} flow/Ok.scala

kse/flow/Flow.class : \
  makefile \
  kse/flow/Ok.class \
  flow/Flow.scala
	${F} flow/Flow.scala

clean :
	${F} -shutdown
	rm -r ${KSE_JAR_PATH} api

docs :
	mkdir -p api
	${D} flow/Ok.scala
	tar czf kse-api.tar.gz api
