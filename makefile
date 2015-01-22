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
  kse/coll/Lazy.class \
  kse/coll/Soft.class \
  kse/coll/Mopt.class \
  kse/eio/base64/Base64.class \
  kse/eio/base64/package.class \
  kse/eio/Grok.class \
  kse/eio/package.class
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

kse/coll/package.class kse/coll/Lazy.class kse/coll/Soft.class kse/coll/Mopt.class : \
  kse/typecheck/package.class \
  coll/Coll.scala
	${F} coll/Coll.scala

kse/eio/base64/Base64.class : \
  kse/flow/Hop.class \
  eio/Base64.scala
	${F} eio/Base64.scala

kse/eio/Grok.class : \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  eio/Grok.scala
	${F} eio/Grok.scala

kse/eio/package.class : \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  kse/flow/package.class \
  eio/Eio.scala
	${F} eio/Eio.scala

clean :
	${F} -shutdown
	rm -r ${KSE_JAR_PATH} api

docs :
	mkdir -p api
	${D} tpck/Typecheck.scala flow/Ok.scala flow/Hop.scala flow/Flow.scala coll/Packed.scala coll/Coll.scala eio/Base64.scala eio/Grok.scala eio/Eio.scala
	tar czf kse-api.tar.gz api

test : \
  makefile \
  kse/tests/Test_Typecheck.class \
  kse/tests/Test_Ok.class \
  kse/tests/Test_Hop.class \
  kse/tests/Test_Flow.class \
  kse/tests/Test_Flow_Macros.class \
  kse/tests/Test_Lazy.class \
  kse/tests/Test_Mopt.class \
  kse/tests/Test_TupleImplicits.class \
  kse/tests/Test_Packed.class \
  kse/tests/Test_Base64.class \
  kse/tests/Test_Grok.class
	scala kse.tests.Test_Typecheck
	scala kse.tests.Test_Ok
	scala kse.tests.Test_Hop
	scala kse.tests.Test_Flow
	scala kse.tests.Test_Flow_Macros
	scala kse.tests.Test_Lazy
	scala kse.tests.Test_Mopt
	scala kse.tests.Test_TupleImplicits
	scala kse.tests.Test_Packed
	scala kse.tests.Test_Base64
	scala kse.tests.Test_Grok

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

kse/tests/Test_Mopt.class : \
  kse/tests/Test_Kse.class \
  kse/flow/Ok.class \
  kse/flow/Hop.class \
  kse/flow/package.class \
  kse/coll/Mopt.class \
  tests/Test_Mopt.scala
	${F} tests/Test_Mopt.scala

kse/tests/Test_TupleImplicits.class : \
  kse/tests/Test_Kse.class \
  kse/coll/package.class \
  tests/Test_TupleImplicits.scala
	${F} tests/Test_TupleImplicits.scala

kse/tests/Test_Packed.class : \
  kse/tests/Test_Kse.class \
  kse/coll/packed/package.class \
  tests/Test_Packed.scala
	${F} tests/Test_Packed.scala

kse/tests/Test_Base64.class : \
  kse/tests/Test_Kse.class \
  kse/flow/package.class \
  kse/eio/base64/package.class \
  kse/eio/base64/Base64.class \
  tests/Test_Base64.scala
	${F} tests/Test_Base64.scala

kse/tests/Test_Grok.class : \
  kse/tests/Test_Kse.class \
  kse/flow/Ok.class \
  kse/flow/package.class \
  kse/eio/base64/package.class \
  kse/eio/base64/Base64.class \
  eio/Grok.scala \
  tests/Test_Grok.scala
	${F} tests/Test_Grok.scala

kse/tests/Test_Kse.class : \
  kse/typecheck/package.class \
  kse/flow/Ok.class \
  kse/flow/package.class \
  tests/Test_Kse.scala
	${F} tests/Test_Kse.scala
