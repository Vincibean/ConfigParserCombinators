package org.vincibean.parser.combinators.lexical

import scala.collection.immutable.Seq
import scala.language.dynamics

object Config {
  case class InnerSetting(settings: Map[String, Any])
      extends AnyVal
      with Dynamic {
    def selectDynamic(name: String): Any = {
      settings(name)
    }
  }
}

class Config[V <: Ast.Val[_]](groups: Seq[Group[V]],
                              overrides: List[(String, String)])
    extends Dynamic {
  private val withSquaresRegex = """(\w+)\[‘(\w+)’\]"""

  private val linearizedOverrides = {
    val prefOvr = overrides.map(_._1)
    val defOvr = overrides.map(_._2)
    prefOvr ++ defOvr
  }

  val groupSettings: Map[String, Config.InnerSetting] = groups.map { g =>
    val k = g.header.name
    val v = Config.InnerSetting(preferredSettings(g.settings.toVector))
    k -> v
  }.toMap

  override val toString: String = groupSettings.toString()

  def selectDynamic(name: String): Config.InnerSetting = {
    groupSettings(name)
  }

  // We do exactly like the exercise asks: return a different type based on the input (!)
  def apply(s: String): Any = s match {
    case xs if xs.split('.').length == 2 =>
      val as = xs.split('.')
      val group = as.head
      val key = as(1)
      groupSettings(group).settings(key)
    case xs if xs.matches(withSquaresRegex) =>
      val r = withSquaresRegex.r
      val r(group, key) = xs
      groupSettings(group).settings(key)
    case ss =>
      groupSettings(ss).settings
  }

  private def preferredSettings(kvs: Seq[(Key, V)]): Map[String, Any] = {
    val ovrSet = linearizedOverrides.reverse.toSet
    val xs: Set[(String, Any)] = for {
      ovr <- ovrSet
      (k, v) <- kvs
      ovr2 <- k.ovride
      if ovr == ovr2
    } yield (k.key, v.wrapped)
    val xs2 = kvs.map { case (k, v) => k.key -> v.wrapped }.toMap
    xs2 ++ xs.toMap
  }

}
