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

case class Config[V <: Ast.Val[_]](
    groups: Seq[Group[V]],
    private val overrides: List[(String, String)])
    extends Dynamic {
  private val withSquaresRegex = """(\w+)\[‘(\w+)’\]"""

  private val linearizedOverrides = {
    val prefOvr = overrides.map(_._1)
    val defOvr = overrides.map(_._2)
    prefOvr ++ defOvr
  }

  val group2setting: Map[String, Config.InnerSetting] = groups.map { g =>
    val k = g.name.name
    val v = Config.InnerSetting(preferredSettings(g.settings.toVector))
    k -> v
  }.toMap

  def selectDynamic(name: String): Config.InnerSetting = {
    group2setting(name)
  }

  // We do exactly like the exercise asks: return a different type based on the input (!)
  def apply(s: String): Any = s match {
    case xs if xs.split('.').length == 2 =>
      val as = xs.split('.')
      val group = as.head
      val key = as(1)
      group2setting(group).settings(key)
    case xs if xs.matches(withSquaresRegex) =>
      val r = withSquaresRegex.r
      val r(group, key) = xs
      group2setting(group).settings(key)
    case ss =>
      group2setting(ss).settings
  }

  private def preferredSettings(kvs: Seq[(Key, V)]): Map[String, Any] = {
    val ovrSet = linearizedOverrides.reverse.toSet
    val xs: Set[(String, Any)] = for {
      ovr <- ovrSet
      (k, v) <- kvs
      ovr2 <- k.ovride
      if ovr == ovr2
    } yield (k.key, v.value)
    val xs2 = kvs.map { case (k, v) => k.key -> v.value }.toMap
    xs2 ++ xs.toMap
  }

}
