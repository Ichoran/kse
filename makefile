## This file is distributed under the BSD 3-clause license.  See file LICENSE.
## Copyright (c) 2014 Rex Kerr and UCSF

F = fsc -feature -deprecation -Xfatal-warnings
S = scalac -feature -deprecation -Xfatal-warnings
X = scala -feature -deprecation -Xfatal-warnings
D = scaladoc -d api

default: Kse.jar
KSE_JAR_PATH = kse/tpck kse/flow kse/eio kse/coll

Kse.jar : \
  kse/tpck/package.class \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  kse/flow/package.class \
  kse/coll/packed/package.class \
  kse/coll/package.class \
  kse/eio/base64/Base64.class \
  kse/eio/Grok.class
	jar cf Kse.jar ${KSE_JAR_PATH}

kse/tpck/package.class : \
  makefile \
  tpck/Typecheck.scala
	${F} tpck/Typecheck.scala

kse/flow/Ok.class : \
  makefile \
  flow/Ok.scala
	${F} flow/Ok.scala

kse/flow/ControlFlowMacroImpl.class : \
  makefile \
  flow/ControlFlowMacroImpl.scala
	${F} flow/ControlFlowMacroImpl.scala

kse/flow/Hop.class : \
  makefile \
  kse/flow/Ok.class \
  kse/flow/ControlFlowMacroImpl.class \
  flow/Hop.scala
	${F} flow/Hop.scala

kse/flow/package.class : \
  makefile \
  kse/flow/Ok.class \
  kse/flow/ControlFlowMacroImpl.class \
  kse/flow/Hop.class \
  flow/Flow.scala
	${F} flow/Flow.scala

kse/coll/packed/package.class : \
  makefile \
  coll/Packed.scala
	${F} coll/Packed.scala

kse/coll/package.class : \
  makefile \
  kse/tpck/package.class \
  coll/Coll.scala
	${F} coll/Coll.scala

kse/eio/base64/Base64.class : \
  makefile \
  eio/Base64.scala
	${F} eio/Base64.scala

kse/eio/Grok.class : \
  makefile \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  eio/Grok.scala
	${F} eio/Grok.scala

clean :
	${F} -shutdown
	rm -r ${KSE_JAR_PATH} api

docs :
	mkdir -p api
	${D} tpck/Typecheck.scala flow/Ok.scala flow/Hop.scala flow/Flow.scala coll/Packed.scala eio/Base64.scala eio/Grok.scala
	tar czf kse-api.tar.gz api
