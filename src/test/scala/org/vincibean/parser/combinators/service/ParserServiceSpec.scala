package org.vincibean.parser.combinators.service

import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.vincibean.parser.combinators.lexical.Ast

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
          return the result expected by the 1st specification of the exercise ${config(
      "common.paid_users_size_limit") must beEqualTo(2147483648D)}
          return the result expected by the 2nd specification of the exercise ${config(
      "ftp.name") must beEqualTo("hello there, ftp uploading")}
          return the result expected by the 3rd specification of the exercise ${config(
      "http.params")
      .asInstanceOf[Seq[Ast.Val[_]]]
      .map(_.wrapped) must beEqualTo(Vector("array", "of", "values"))}
          return the result expected by the 4th specification of the exercise ${config(
      "ftp.lastname").asInstanceOf[Option[_]] must beNone}
          return the result expected by the 5th specification of the exercise ${config(
      "ftp.enabled").asInstanceOf[Boolean] must beFalse}
          return the result expected by the 6th specification of the exercise ${config(
      "ftp[‘path’]").toString must beEqualTo("/etc/var/uploads")}
          return the result expected by the 7th specification of the exercise ${config(
      "ftp")
      .asInstanceOf[Map[String, Any]]
      .mapValues(_.toString) must beEqualTo(
      Map("name" -> "hello there, ftp uploading",
          "path" ->
            "/etc/var/uploads",
          "enabled" -> "false"))}
      """
}
