
object charpter12 extends App {
  def question1(fun: Int => Int, low: Int, high: Int) = {
    for (i <- low to high) yield (i, fun(i))
  }

  question1(_ * 2, -1, 2) foreach { println _ }

  def question2(arr: Iterable[Int]): Int = {
    arr.reduceLeft((x, y) => if (x > y) x else y)
  }

  Predef println question2(Seq(4, 2, 6, 3))

  def question3(low: Int, high: Int) = {
    low to high reduceLeft { _ * _ }
  }

  Predef println question3(1, 4)

  def question4(low: Int, high: Int) = {
    (low to high).foldLeft(1)(_ * _)
  }

  Predef println question4(1, 4)

  def question5(fun: Int => Int, input: Iterable[Int]) = {
    fun(input reduceLeft { (x, y) => if (fun(x) > fun(y)) x else y })
  }

  Predef println question5(x => (10 - x) * x, 1 to 10)

  def question6(fun: Int => Int, input: Iterable[Int]) = {
    input reduceLeft { (x, y) => if (fun(x) > fun(y)) x else y }
  }

  /*
   * question7
   */
  def adjust2Pair(fun: (Int, Int) => Int)(pair: (Int, Int)) = fun(pair._1, pair._2)

  1 to 10 zip (11 to 20) map { adjust2Pair(_ * _)(_) } foreach { println _ }

  def question8(strArr: Array[String], intArr: Array[Int]): Boolean = {
    strArr.corresponds(intArr)(_.length == _)
  }

  Predef println question8(Array("d", "dd"), Array(1, 2))

  def question9() {
    val arr = Iterator("asdf", "dd")
    def corresponds[B](that: Iterator[B], p: (String, B) => Boolean): Boolean = {
      while (arr.hasNext && that.hasNext) {
        if (!p(arr.next(), that.next())) {
          return false
        }
      }
      !arr.hasNext && !that.hasNext
    }
    Predef println corresponds(Iterator(3, 2), (x: String, y: Int) => x.length() == y)
  }

  question9()

  /*
   * question10
   */
  def unless(condition: => Boolean)(block: => Unit) {
    if (!condition) {
      block
    }
  }
  unless(0 < 10) {
    println("hahaha")
  }
}