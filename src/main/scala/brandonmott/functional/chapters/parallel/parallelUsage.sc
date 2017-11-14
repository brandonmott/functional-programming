def sumFold(ints: Seq[Int]): Int =
  ints.foldLeft(0)((a, b) => a + b)

/**
  * Using the Divide and Conquer algorithm:
  * divide the sequence in half using the splitAt function, 
  * `recursively` sum both halves, and then combine their results.
  * Note: this implementation can be parallelized
  */
def sumDivideAndConquer(ints: IndexedSeq[Int]): Int =
  if (ints.size <= 1)
    ints.headOption getOrElse 0
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    sumDivideAndConquer(l) + sumDivideAndConquer(r)
  }