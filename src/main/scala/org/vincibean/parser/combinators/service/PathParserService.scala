package org.vincibean.parser.combinators.service

import java.nio.file.Path

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical.Ast

trait PathParserService extends CommonParserService {

  val pathParser: core.Parser[Ast.Path[Path], Char, String] = {
    val slash = P("/")
    (slash ~/ string).rep(1).map { x =>
      val fs = x.filter(_.nonEmpty)
      val path =
        if (fs.isEmpty)
          java.nio.file.Paths.get("/")
        else
          java.nio.file.Paths.get("/", fs: _*)
      Ast.Path(path)
    }
  }

}
