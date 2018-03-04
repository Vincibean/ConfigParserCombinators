package org.vincibean.parser.combinators

import org.vincibean.parser.combinators.service.ParserService

object Main extends ParserService {

  def main(args: Array[String]) {
    // TODO libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.3"
    val config = loadConfig(
      "/Users/andrea/IdeaProjects/mine/Parser Combinators/src/main/resources/settings.conf",
      List("ubuntu" -> "production"))
    println(config)
    println(config("ftp.enabled"))
    println(config("ftp[‘path’]"))
    println(config.ftp.path)

  }

}
