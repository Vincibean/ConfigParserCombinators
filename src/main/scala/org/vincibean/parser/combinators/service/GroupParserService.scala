package org.vincibean.parser.combinators.service

import fastparse.all._
import fastparse.core
import org.vincibean.parser.combinators.lexical
import org.vincibean.parser.combinators.lexical.{Ast, Group, Header, Key}

trait GroupParserService extends LineParserService {

  val groupParser: core.Parser[Group[Ast.Val[_]], Char, String] = {
    val multiline = lineParser.rep.map(_.flatten)
    val header = P("[" ~/ string ~/ "]" ~/ newline.rep(1)).map(h => Header(h))
    P(header ~/ multiline ~/ newline.rep).map {
      case (h, kvs) => lexical.Group(h, kvs)
    }
  }

}
