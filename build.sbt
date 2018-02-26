name := "Parser Combinators"

version := "0.1"

scalaVersion := "2.12.4"

scalafmtOnCompile in ThisBuild := true // all projects

wartremoverErrors ++= Warts.all

lazy val compileScalastyle = taskKey[Unit]("compileScalastyle")

compileScalastyle := scalastyle.in(Compile).toTask("").value

(compile in Compile) := ((compile in Compile) dependsOn compileScalastyle).value
