object FunSets {

  type Set = (Int => Boolean)
  val bound = 1000

  def singletonSet(elem: Int): Set = (x: Int) => elem == x

  def union(s: Set, t: Set): Set = (x: Int) => s(x) || t(x)

  def intersect(s: Set, t: Set): Set = (x: Int) => s(x) && t(x)

  def diff(s: Set, t: Set): Set = (x: Int) => s(x) && !t(x)

  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  def map(s: Set, f: Int => Int): Set = (x: Int) => exists(s, y => f(y) == x)

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a)) p(a) && iter(a + 1)
      else iter(a + 1)
    }

    iter(-bound)
  }

  def printSet(s: Set) {
    println(toString(s))
  }

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def contains(s: Set, elem: Int): Boolean = s(elem)
}

val s1 = FunSets.singletonSet(1)
val s2 = FunSets.singletonSet(2)
val s3 = FunSets.singletonSet(3)
val s4 = FunSets.singletonSet(4)
val s5 = FunSets.singletonSet(5)
val u0 = FunSets.union(FunSets.union(FunSets.union(s1, s2), s3), s4)
//FunSets.toString(u)

FunSets.exists(u0, s2)
FunSets.exists(u0, s5)
FunSets.exists(s2, (x: Int) => x.==(2))

//submit rogge+coursera@roggeheflin.com oYtcCsJvZXZzpxIu