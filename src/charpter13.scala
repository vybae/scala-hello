
import scala.collection.mutable.ArrayBuffer

object charpter13 extends App {
  def question1(str: String): collection.mutable.Map[Char, Int] = {
    val map = collection.mutable.Map[Char, Int]()
    str foreach { x => map(x) = map.getOrElse(x, 0) + 1 }
    map
  }

  def question2(str: String): Map[Char, Int] = {
    (Map[Char, Int]() /: str) {
      (m, c) => m + (c -> (m.getOrElse(c, 0) + 1))
    }
  }

  def question3(link: List[Int]): List[Int] = {
    link filter { _ != 0 }
    // 或者 link.foldLeft(List[Int]()) { (l, n) => if (n == 0) l else l :+ n }
  }

  def question4(arr: Array[String], map: Map[String, Int]): Array[Int] = {
    arr flatMap { map.get _ }
  }
  question4(Array("a", "b"), Map("a" -> 100, "b" -> 2)) foreach println

  /*
   * question5
   */
  trait IMakeString {
    this: Iterable[String] =>
    def makeString(): String = this reduceLeft { _ + _ }
  }
  val a = new ArrayBuffer[String]() with IMakeString
  a += "dd"
  println(a.makeString)

  /*
   * (lst :\ List[Int]()){_::_}
   * (List[Int]() /: lst){_:+_}
   * 以上2个表达式结果为得到队列自身
   * ----------------------------
   * question6 逆序排列
   */
  def question6(lst: List[Int]): List[Int] = {
    (List[Int]() /: lst) { (l, n) => n :: l }
    // 或者 (lst :\ List[Int]()) { (n, l) => l :+ n }
  }

  def question7(arr1: Iterable[Int], arr2: Iterable[Int]): Iterable[Int] = {
    arr1 zip arr2 map { Function.tupled { (a, b) => a * b } }
  }

  def question8[A](arr: Iterable[A], n: Int) = {
    arr.grouped(n)
  }
  question8(Array(1, 2, 3, 4), 2) foreach { println }

  def question10(str: String): Map[Char, Int] = {
    str.par.aggregate(Map[Char, Int]())(
      (m, c) => m + (c -> (m.getOrElse(c, 0) + 1)),
      (a, b) => (Map[Char, Int]() /: (a.keys ++ b.keys)) {
        (m, c) => m + (c -> (a.getOrElse(c, 0) + b.getOrElse(c, 0)))
      }
    )
  }
  question10("a" * 22 + "b" * 34) foreach println
}