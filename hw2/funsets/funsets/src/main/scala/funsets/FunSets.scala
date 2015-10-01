package funsets

import common._

/**
 * 2. Чисто функциональные множества.
 */
object FunSets {
  /**
   * Мы представляем множество их характеристической функцией. Например, его предикатом
   * `contains`.
   */
  type Set = Int => Boolean

  /**
   * Проверяет, содержит ли множество данный элемент.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Возвращает множество из одного переданного элемента.
   */
  def singletonSet(elem: Int): Set = (x: Int) => elem == x

  /**
   * Возвращает объединение двух данных множеств,
   * множества всех элементов , входящих либо в `s`, либо в `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  /**
   * Возвращает пересечение двух заданных множеств,
   * множество всех элементов, которые входят и в `s` и в `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  /**
   * Возвращает разницу между двумя данными множествами,
   * множество всех элементов, входящих в `s` и не входящих в `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  /**
   * Возвращает подмножество `s` для которых `p` выполняется.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  /**
   * Ограничения для `forall` и `exists` равны +/- 1000.
   */
  val bound = 1000

  /**
   * Проверяет, все ли числа множества `s` из заданного интервалла удовлетворяют `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a) && !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
   * Проверяет, существует ли число в интервалле из множества `s`
   * которое удовлетворяет `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (x : Int)=> !p(x))

  /**
   * Возвращает множество преобразованное применением `f` к каждому элементу `s`.
   */
  def map(s: Set, f: Int => Int): Set =  (x: Int)=> exists(s, (n : Int) => f(n) == x )

  /**
   * Отображает содержимое множества
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Выводит на печать содержимое множества на консоль.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
