// Creates a new random number generator 
// seeded with the current system time
val rn = new scala.util.Random

val randomDouble = rn.nextDouble
//Gets a random integer between 0 and 9
val randomInt = rn.nextInt(10)

import brandonmott.functional.chapters.state.RNG._
import brandonmott.functional.chapters.state._
import brandonmott.functional.chapters.state.{RNG, Simple}
val rng = Simple(42)

val (int1, rng1) = rng.nextInt

val (int2, rng2) = rng1.nextInt

val ((pair1, pair2), rng3) = RNG.randomPair(rng)

val (pos, rng4) = RNG.nonNegativeInt(rng)

val (doub, rng5) = RNG.double1(rng4)

val (intDouble, rng6) = RNG.intDouble(rng5)
val (doubleInt, rng7) = RNG.doubleInt(rng6)
val (double3, rng8) = RNG.double3(rng7)

val (list, rng9) = RNG.ints(5)(rng8)

//Using the RNG `state transition`, the unit action
val unit3 = RNG.unit(3)

val even = RNG.nonNegativeEven

val d = RNG.double
val _d = RNG._double

val x = 102
val n = 40962

n % x