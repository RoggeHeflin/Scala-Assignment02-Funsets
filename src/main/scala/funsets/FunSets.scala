package funsets


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
// this operates just like a function, but is initialized with 'type'
//  type Set = Int => Boolean

  type Set = (Int => Boolean)

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
//  def singletonSet(elem: Int): Set = (x: Int) => List(elem) contains x
//  refactored to simpler function
def singletonSet(elem: Int): Set = (x: Int) => elem == x

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
//  def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)

// refactored to match 's(elem)' notation
def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
//  def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)

// refactored to match 's(elem)' notation
def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
//  def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)

// refactored to match 's(elem)' notation
def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
//  NOTE: 'p: Int => Boolean' has the same definition as 'Set'; therefore, it operates like a set
//  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && contains(p, x)

// refactored to match 's(elem)' notation
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
//      else if (contains(s, a) && contains(p, a)) true
//    need recursive function for if true: is "a" in the set "s"; does "a" confrom to "p"?  and then continue
      else if (s(a)) p(a) && iter(a + 1)
      else iter(a + 1)
    }
    //  starting at -bound
    iter(-bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = (x: Int) => exists(s, y => f(y) == x)

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
