package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 0))
      !contains(s1, 2)
      assert(!contains(s1, 100000))
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains the right subset of elements") {
    new TestSets {
      val i1 = intersect(s1, s1)
      assert(contains(i1, 1))
      assert(!contains(i1, 2))

      val i2 = intersect(s1, s2)
      assert(!contains(i2, 1))
      assert(!contains(i2, 2))
    }
  }

  test("diff contains the right elements") {
    new TestSets {
      val s12 = union(s1, s2)

      val d1 = diff(s12, s2)
      assert(contains(d1, 1))
      assert(!contains(d1, 2))
    }
  }

  test("filter contains the right elements") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      def p(x: Int): Boolean = !(x % 2 == 0)

      val f = filter(s, p)
      assert(contains(f, 1))
      assert(!contains(f, 2))
      assert(contains(f, 3))
    }
  }

  trait QueriesSets {
    val bound = 1000
    val s0 = singletonSet(-bound)
    val s1 = singletonSet(-49)
    val s2 = singletonSet(0)
    val s3 = singletonSet(100)
    val s4 = singletonSet(bound)

    val u = union(union(union(union(s0, s1), s2), s3), s4)
  }

  test("forall returns the right value") {
    new QueriesSets {
      assert(forall(u, (x: Int) => true))
      assert(!forall(u, (x: Int) => (x%2) == 0))
    }
  }

  test("exist returns the right value") {
    new QueriesSets {
      assert(exists(u, (x: Int) => x == 0))
      assert(exists(u, (x: Int) => (x%2) == 0))
    }
  }

  test("map returns the right set") {
    new QueriesSets {
      val mapSet = map(s1, (x: Int) => 2*x)
      assert(contains(mapSet, -98))
      assert(!contains(mapSet, -49))
      assert(!contains(mapSet, 0))
    }
  }
}
