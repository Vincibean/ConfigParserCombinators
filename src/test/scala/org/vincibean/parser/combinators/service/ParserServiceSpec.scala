package org.vincibean.parser.combinators.service

import org.specs2.Specification
import org.specs2.specification.core.SpecStructure

import scala.util.Try

class ParserServiceSpec extends Specification with ParserService {

  private val config = loadConfig(
    this.getClass.getClassLoader.getResource("settings.conf").getFile,
    List("ubuntu" -> "production"))

  private val invalidConfig = loadConfig(
    this.getClass.getClassLoader.getResource("invalid-settings.conf").getFile,
    List("ubuntu" -> "production"))

  override def is: SpecStructure =
    s2"""
        ParserService can
          successfully parse a complex settings file ${config.groupSettings must not beEmpty}
          throw an exception when a non-existing file is given ${Try(
      loadConfig("i-should-not-exist.conf", Nil)) must beFailedTry}
          return an empty config when an invalid settings file is given ${invalidConfig.groupSettings must beEmpty}
      """
}
