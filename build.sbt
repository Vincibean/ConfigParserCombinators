name := "Parsing Combinators"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.specs2" %% "specs2-core" % "4.0.3" % Test,
  "org.specs2" %% "specs2-scalacheck" % "4.0.3" % Test
)

scalafmtOnCompile in ThisBuild := true

lazy val compileScalastyle = taskKey[Unit]("compileScalastyle")

compileScalastyle := scalastyle.in(Compile).toTask("").value

(compile in Compile) := ((compile in Compile) dependsOn compileScalastyle).value
