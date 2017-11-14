package brandonmott.functional.core
package monoids
package syntax

import brandonmott.functional.core.monoids.{FoldLeft, Monoid}

/**
  * Enrich my library. inject `plus` to both Int and String with just one definition.
  */
trait Identity[A] {
  val value: A

  def plus(a2: A)(implicit m: Monoid[A]): A = m.mappend(value, a2)
}




