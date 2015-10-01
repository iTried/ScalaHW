package funsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Этот класс - набор методов для объектов в классе FunSets. Для запуска тестов вы можете:
  *  - набрать команду "test" в sbt консоли (sbt test)
 *   - В IDE правой кнопкой на файле "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Ссылка на scaladoc - очень понятный и детализированный туториал по FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Операторы
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Тесты написаны с использованием оператора "test" и метода "assert".
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }


  
  import FunSets._

  trait TestSets {
    val s1 = (x: Int) => x > 0
    val s2 = (x: Int) => x != x
    val s3 = (x: Int) => x % 2 == 0
  }

  test("contains tests"){
    new TestSets {
      assert(contains(s1, 100), "contains 1")
      assert(!contains(s1, 0), "contains 2")
      assert(!contains(s1, -2), "contains 3")

      assert(!contains(s2, 0), "contains 4")
      assert(!contains(s2, 100), "contains 5")
      assert(!contains(s2, -500), "contains 6")

      assert(contains(s3, 100), "contains 7")
      assert(contains(s3, 0), "contains 8")
      assert(!contains(s3, 201), "contains 9")
    }
  }


  test("union tests"){
    new TestSets {
      val s1Ors2 = union(s1,s2)
      val s2Ors3 = union(s2,s3)
      val s1Ors3 = union(s3,s1)

      assert(contains(s1Ors2, 1), "union 1")
      assert(!contains(s1Ors2, -2), "union 2")

      assert(!contains(s2Ors3, 1), "union 3")
      assert(contains(s2Ors3, 100), "union 4")
      assert(contains(s2Ors3, -500), "union 5")

      assert(contains(s1Ors3, 101), "union 6")
      assert(contains(s1Ors3, 12), "union 7")
      assert(contains(s1Ors3, -10), "union 8")
      assert(!contains(s1Ors3, -15), "union 9")
    }
  }

  test("intersect tests"){
    new TestSets {
      val s1Ands2 = intersect(s1,s2)
      val s2Ands3 = intersect(s2,s3)
      val s1Ands3 = intersect(s3,s1)

      assert(!contains(s1Ands2, 1), "intersect 1")
      assert(!contains(s1Ands2, -2), "intersect 2")

      assert(!contains(s2Ands3, 1), "intersect 3")
      assert(!contains(s2Ands3, -500), "intersect 4")

      assert(!contains(s1Ands3, 101), "intersect 5")
      assert(contains(s1Ands3, 12), "intersect 6")
      assert(!contains(s1Ands3, -10), "intersect 7")
      assert(!contains(s1Ands3, -15), "intersect 8")
    }
  }

  test("diff tests"){
    new TestSets {
      val s3diffs1 = diff(s3,s1)
      val s1diffs3 = diff(s1,s3)

      assert(contains(s3diffs1, 0), "diff 1")
      assert(contains(s3diffs1, -2), "diff 2")
      assert(!contains(s3diffs1, 2), "diff 3")

      assert(!contains(s1diffs3, 2), "diff 4")
      assert(contains(s1diffs3, 101), "diff 5")
      assert(!contains(s1diffs3, -6), "diff 6")
    }
  }

  test("filter tests"){
    new TestSets {
      val s1Subset = filter(s1, (x : Int) => x % 2 == 0)
      val s3Subset = filter(s3, (x : Int) => x < 0)

      assert(!contains(s1Subset, 0), "filter 1")
      assert(!contains(s1Subset, -2), "filter 2")
      assert(contains(s1Subset, 2), "filter 3")
      assert(!contains(s1Subset, 7), "filter 4")

      assert(!contains(s3Subset, 2), "filter 5")
      assert(!contains(s3Subset, 101), "filter 6")
      assert(contains(s3Subset, -6), "filter 7")
      assert(!contains(s3Subset, -57), "filter 8")
    }
  }

  test("forall tests"){
    new TestSets {
      assert(forall(s1, (x : Int) => contains(s1, x)), "forall 1")
      assert(!forall(s1, (x : Int) => x == 5), "forall 2")
      assert(!forall(s3, (x : Int) => x % 4 == 0), "forall 3")
      assert(!forall(s1, (x : Int) => x > 0 && x < 0), "forall 4")
    }
  }

  test("exist tests"){
    new TestSets {

      val s1Squared= map(s1, (x : Int) => x * x)
      val s3PlusOne = map(s3, (x : Int) => x + 1)

      assert(contains(s3PlusOne, 5), "map 1")
      assert(!contains(s3PlusOne, 6), "map 2")

      assert(contains(s1Squared, 25), "map 3")
      assert(!contains(s1Squared, 24), "map 4")
      assert(!contains(s1Squared, 3), "map 5")
      assert(contains(s1Squared, 10000), "map 6")

    }
  }

  test("map tests"){
    new TestSets {
      assert(exists(s1, (x : Int) => contains(s1, x)), "exist 1")
      assert(exists(s1, (x : Int) => x == 5), "exist 2")
      assert(exists(s3, (x : Int) => x % 4 == 0), "exist 3")
      assert(!exists(s1, (x : Int) => x > 0 && x < 0), "exist 4")
      assert(!exists(s2, (x : Int) => true), "exist 5")
    }
  }

  trait TestSingltons {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(-3)
  }

  test("singleton tests") {
    new TestSingltons {
      assert(contains(s1, 1), "Singleton 1")
      assert(contains(s2, 2), "Singleton 2")
      assert(contains(s3, -3), "Singleton 3")
    }
  }
}
