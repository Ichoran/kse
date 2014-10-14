## This file is distributed under the BSD 3-clause license.  See file LICENSE.
## Copyright (c) 2014 Rex Kerr and UCSF

F = fsc -feature -deprecation -Xfatal-warnings
S = scalac -feature -deprecation -Xfatal-warnings
X = scala -feature -deprecation -Xfatal-warnings
D = scaladoc -d api

default: Kse.jar
KSE_JAR_PATH = kse/typecheck kse/flow kse/eio kse/coll

Kse.jar : \
  makefile \
  kse/typecheck/package.class \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  kse/flow/package.class \
  kse/coll/packed/package.class \
  kse/coll/package.class \
  kse/eio/base64/Base64.class \
  kse/eio/Grok.class
	jar cf Kse.jar ${KSE_JAR_PATH}

kse/typecheck/package.class : \
  tpck/Typecheck.scala
	${F} tpck/Typecheck.scala

kse/flow/Ok.class : \
  flow/Ok.scala
	${F} flow/Ok.scala

kse/flow/ControlFlowMacroImpl.class : \
  flow/ControlFlowMacroImpl.scala
	${F} flow/ControlFlowMacroImpl.scala

kse/flow/Hop.class : \
  kse/flow/Ok.class \
  kse/flow/ControlFlowMacroImpl.class \
  flow/Hop.scala
	${F} flow/Hop.scala

kse/flow/package.class : \
  kse/flow/Ok.class \
  kse/flow/ControlFlowMacroImpl.class \
  kse/flow/Hop.class \
  flow/Flow.scala
	${F} flow/Flow.scala

kse/coll/packed/package.class : \
  coll/Packed.scala
	${F} coll/Packed.scala

kse/coll/package.class : \
  kse/typecheck/package.class \
  coll/Coll.scala
	${F} coll/Coll.scala

kse/eio/base64/Base64.class : \
  eio/Base64.scala
	${F} eio/Base64.scala

kse/eio/Grok.class : \
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

test : \
  makefile \
  kse/tests/Test_Typecheck.class \
  kse/tests/Test_Ok.class \
  kse/tests/Test_Hop.class \
  kse/tests/Test_Flow.class \
  kse/tests/Test_Flow_Macros.class \
  kse/tests/Test_Lazy.class
	scala kse.tests.Test_Typecheck
	scala kse.tests.Test_Ok
	scala kse.tests.Test_Hop
	scala kse.tests.Test_Flow
	scala kse.tests.Test_Flow_Macros
	scala kse.tests.Test_Lazy

kse/tests/Test_Typecheck.class : \
  kse/tests/Test_Kse.class \
  kse/typecheck/package.class \
  tests/Test_Typecheck.scala
	${F} tests/Test_Typecheck.scala

kse/tests/Test_Ok.class : \
  kse/tests/Test_Kse.class \
  kse/flow/Ok.class \
  kse/flow/package.class \
  tests/Test_Ok.scala
	${F} tests/Test_Ok.scala

kse/tests/Test_Hop.class : \
  kse/tests/Test_Kse.class \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  kse/flow/package.class \
  tests/Test_Hop.scala
	${F} tests/Test_Hop.scala

kse/tests/Test_Flow.class : \
  kse/tests/Test_Kse.class \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  kse/flow/package.class \
  tests/Test_Flow.scala
	${F} tests/Test_Flow.scala

kse/tests/Test_Flow_Macros.class : \
  kse/tests/Test_Kse.class \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  kse/flow/package.class \
  kse/flow/ControlFlowMacroImpl.class \
  tests/Test_Flow_Macros.scala
	${F} tests/Test_Flow_Macros.scala

kse/tests/Test_Lazy.class : \
  kse/tests/Test_Kse.class \
  kse/coll/Lazy.class \
  tests/Test_Lazy.scala
	${F} tests/Test_Lazy.scala

kse/tests/Test_Kse.class : \
  kse/typecheck/package.class \
  kse/flow/Ok.class \
  kse/flow/package.class \
  tests/Test_Kse.scala
	${F} tests/Test_Kse.scala
