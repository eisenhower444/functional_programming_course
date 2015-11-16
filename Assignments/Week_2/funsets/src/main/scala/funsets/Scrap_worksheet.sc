import funsets.FunSets._

def twice(x: Int) = 2*x
twice(-49)


def map(s: Set, f: Int => Int): Set = (elem: Int) => exists(s, (x: Int) => f(x) == elem)

val s1 = singletonSet(-49)
val mapSet = map(s1, (x: Int) => 2*x)

contains(mapSet, -98)

!contains(mapSet, -49)
!contains(mapSet, 0)
