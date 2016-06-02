
object charpter12 extends App {
  
  /*
   * 12.1
   * 编写函数values(fun:(Int)=>Int,low:Int,high:Int),
   * 该函数输出一个集合，对应给定区间内给定函数的输入 和输出。
   * 比如，values(x=>x*x,-5,5)应该产出一个对偶的集合(-5,25),(-4,16),(-3,9),…,(5,25) 
   */
  def question1(fun: Int => Int, low: Int, high: Int) = {
    for (i <- low to high) yield (i, fun(i))
  }
  question1(_ * 2, -1, 2) foreach { println _ }

  /*
   * 12.2
   * 如何用reduceLeft得到数组中的最大元素?
   */
  def question2(arr: Iterable[Int]): Int = {
    arr.reduceLeft((x, y) => if (x > y) x else y)
  }
  Predef println question2(Seq(4, 2, 6, 3))

  /*
   * 12.3
   * 用to和reduceLeft实现阶乘函数,不得使用循环或递归
   */
  def question3(low: Int, high: Int) = {
    low to high reduceLeft { _ * _ }
  }
  Predef println question3(1, 4)

  /*
   * 12.4
   * 前一个实现需要处理一个特殊情况，即n<1的情况。
   * 展示如何用foldLeft来避免这个需要。
   */
  def question4(low: Int, high: Int) = {
    (low to high).foldLeft(1)(_ * _)
  }
  Predef println question4(1, 4)

  /*
   * 12.5
   * 编写函数largest(fun:(Int)=>Int,inputs:Seq[Int]),
   * 输出在给定输入序列中给定函数的最大值。
   * 举例来说，largest(x=>10*x-x*x,1 to 10)应该返回25,不得使用循环或递归
   */
  def question5(fun: Int => Int, input: Iterable[Int]) = {
    fun(input reduceLeft { (x, y) => if (fun(x) > fun(y)) x else y })
  }
  Predef println question5(x => (10 - x) * x, 1 to 10)

  /*
   * 12.6
   * 修改前一个函数，返回最大的输出对应的输入。
   * 举例来说,largestAt(fun:(Int)=>Int,inputs:Seq[Int])应该返回5。
   * 不得使用循环或递归
   */
  def question6(fun: Int => Int, input: Iterable[Int]) = {
    input reduceLeft { (x, y) => if (fun(x) > fun(y)) x else y }
  }

  /*
   * 12.7
   * 要得到一个序列的对偶很容易，比如: 
   * val pairs = (1 to 10) zip (11 to 20)
   * 假定你想要对这个序列做某中操作—比如，给对偶中的值求和，但是你不能直接使用:
   * pairs.map(_ + _)
   * 函数_ + _ 接受两个Int作为参数，而不是(Int,Int)对偶。
   * 编写函数adjustToPair,该函数接受一个类型为(Int,Int)=>Int的 函数作为参数，
   * 并返回一个等效的, 可以以对偶作为参数的函数。
   * 举例来说就是:adjustToPair(_ * _)((6,7))应得到42。
   * 然后用这个函数通过map计算出各个对偶的元素之和
   * 
   */
  def adjust2Pair(fun: (Int, Int) => Int)(pair: (Int, Int)) = fun(pair._1, pair._2)
  
  1 to 10 zip (11 to 20) map { adjust2Pair(_ * _)(_) } foreach { println _ }

  /*
   * 12.8
   * 在12.8节中，你看到了用于两组字符串数组的corresponds方法。
   * 做出一个对该方法的调用，让它帮我们判断某个字符串数组里的所有元素的长度是否和某个给定的整数数组相对应
   */
  def question8(strArr: Array[String], intArr: Array[Int]): Boolean = {
    strArr.corresponds(intArr)(_.length == _)
  }
  Predef println question8(Array("d", "dd"), Array(1, 2))

  /*
   * 12.9
   * 不使用柯里化实现corresponds。然后尝试从前一个练习的代码来调用。
   * 你遇到了什么问题？
   */
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
   * 12.10
   * 实现一个unless控制抽象，工作机制类似if,但条件是反过来的。
   * 第一个参数需要是换名调用的参数吗？你需要柯里化吗？
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