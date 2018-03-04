package org.vincibean.parser.combinators

object Main extends ParserService {

  def main(args: Array[String]) {
    // TODO libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"
    println(
      loadConfig(
        "/Users/andrea/IdeaProjects/mine/Parser Combinators/src/main/resources/settings.conf",
        Nil))
  }

}
