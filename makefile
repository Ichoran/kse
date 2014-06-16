## This file is distributed under the BSD 3-clause license.  See file LICENSE.
## Copyright (c) 2014 Rex Kerr and UCSF

F = fsc -feature -deprecation -Xfatal-warnings
S = scalac -feature -deprecation -Xfatal-warnings
X = scala -feature -deprecation -Xfatal-warnings
D = scaladoc -d api

default: Kse.jar
KSE_JAR_PATH = kse/flow kse/eio kse/coll

Kse.jar : \
  kse/flow/Ok.class \
  kse/flow/Flow.class \
  kse/coll/packed/package.class \
  kse/eio/Base64.class \
  kse/eio/Grok.class
	jar cf Kse.jar ${KSE_JAR_PATH}

kse/flow/Ok.class : \
  makefile \
  flow/Ok.scala
	${F} flow/Ok.scala

kse/flow/ControlFlowMacroImpl.class : \
  makefile \
  flow/ControlFlowMacroImpl.scala
	${F} flow/ControlFlowMacroImpl.scala

kse/flow/Flow.class : \
  makefile \
  kse/flow/Ok.class \
  kse/flow/ControlFlowMacroImpl.class \
  flow/Flow.scala
	${F} flow/Flow.scala

kse/coll/packed/package.class : \
  makefile \
  coll/Packed.scala
	${F} coll/Packed.scala

kse/eio/Base64.class : \
  makefile \
  eio/Base64.scala
	${F} eio/Base64.scala

kse/eio/Grok.class : \
  makefile \
  kse/flow/Ok.class \
  kse/flow/Flow.class \
  eio/Grok.scala
	${F} eio/Grok.scala

clean :
	${F} -shutdown
	rm -r ${KSE_JAR_PATH} api

docs :
	mkdir -p api
	${D} flow/Ok.scala flow/Flow.scala coll/Packed.scala eio/Base64.scala eio/Grok.scala
	tar czf kse-api.tar.gz api
