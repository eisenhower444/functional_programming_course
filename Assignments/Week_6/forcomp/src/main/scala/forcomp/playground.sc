// Streams

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  println(lo + " ")
  if (lo > hi) Stream.Empty
  else Stream.cons(lo, streamRange(lo+1, hi))
}

//streamRange(1, 1000).filterNot(_%2 == 0).take(3).toList
// Lazy evaluation
lazy val x = 4

// Infinite Streams
def from(n: Int): Stream[Int] = n #:: from(n+1)

val nats = from(0)

nats.map(_ * 4).take(4).toList

val y = 4

// Sieve of Eratosthenes
def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail.filterNot(_ % s.head == 0))

val primes = sieve(from(2))

primes.take(5).toList

// Back to Square Roots!

def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double): Double = (guess + x / guess) / 2

  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)

  guesses
}

def isGoodEnough(guess: Double, x: Double): Boolean =
  math.abs((guess * guess - x) / x) < 0.0001

sqrtStream(4) filter (isGoodEnough(_, 4))

