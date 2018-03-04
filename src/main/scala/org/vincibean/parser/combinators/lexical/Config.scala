package org.vincibean.parser.combinators.lexical

import scala.collection.immutable.Seq

case class Config(groups: Seq[Group[_]], private val overrides: List[(String, String)]) {
  private val linearizedOverrides = {
    val prefOvr = overrides.map(_._1)
    val defOvr = overrides.map(_._2)
    prefOvr ++ defOvr
  }

  // We do exactly like the exercise asks: return a different type based on the input (!)
  def apply(s: String): Any = s match {
    case xs if xs.split('.').length == 2 =>
      val as = xs.split('.')
      val group = as.head
      val key = as(1)
      fetchSetting(group, key)
    case xs if "(w+)[‘(w+)’]".r.findAllIn(xs).length == 2 =>
      // TODO fix pattern mathcing in this regex
      val (group, key) = "(w+)[‘(w+)’]".r(xs)
      fetchSetting(group, key)
    case ss =>
      groups.find(_.name == ss).flatMap(g => preferredSetting(g.settings.toVector))
  }

  private def fetchSetting(group: String, key: String) = {
    val kvs = overridableSettings(group, key)
    preferredSetting(kvs.toVector)
  }

  private def overridableSettings(group: String, key: String): scala.Seq[(Key, Any)] =
    groups.find(_.name == group).toSeq.flatMap { group =>
      group.settings.filter(_._1.key == key)
    }

  private def preferredSetting(kvs: Seq[(Key, Any)]): Option[Any] = {
    val y: Seq[Any] = for {
      s <- linearizedOverrides
      (key, value) <- kvs.reverse
      if key.ovride.contains(s)
    } yield value
    y.headOption
  }

}
