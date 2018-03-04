package org.vincibean.parser.combinators.service

import java.nio.file.{Path, Paths}

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical.Ast

trait PathParserService extends CommonParserService {

  val pathParser: core.Parser[Ast.Path[Path], Char, String] = {
    val slash = P("/")
    (slash ~/ string).rep(1).map { x =>
      val fs = x.filter(_.nonEmpty)
      Ast.Path(asPath(fs))
    }
  }

  private def asPath(fs: Seq[String]) = Paths.get("/", fs: _*)

}
