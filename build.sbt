name := "aether"

version := "0.1"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  // "-Xfatal-warnings",
  "-Xlint:_",
  "-Ywarn-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-deprecation",
  "-feature",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  // TODO we don't need all these libraries
  "colt" % "colt" % "1.2.0",
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.randombits.math" % "math-eval" % "1.0.1",
  "gov.nist.math" % "jama" % "1.0.3",
  "org.typelevel" %% "cats" % "0.8.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
