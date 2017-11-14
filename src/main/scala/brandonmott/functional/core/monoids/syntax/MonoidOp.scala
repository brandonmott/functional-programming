package brandonmott.functional.core
package monoids
package syntax

import brandonmott.functional.core.monoids.{FoldLeft, Monoid}

/**
  * Enrich my library. inject `|+|` to both Int and String with just one definition.
  */
trait MonoidOp[A] {
  val F: Monoid[A]
  val value: A

  //the method being injected
  def |+|(a2: A): A = F.mappend(value, a2)
}