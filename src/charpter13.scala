
import scala.collection.mutable.ArrayBuffer

object charpter13 extends App {
  /*
   * 13.1
   * 编写一个函数,给定字符串, 产出一个包含所有字符的下标的映射. 
   * 举例来说,indexes("Mississippi")应返回一个映射, 让'M'对应集{0},
   * 'i'对应集{1,4,7,10}, 以此类推
   * 使用字符到可变集的映射, 另外, 你如何保证集是经过排序的?
   */
  def question1(str: String): Map[Char, Iterable[Int]] =
    (collection.mutable.Map[Char, Iterable[Int]]() /: (str zip (0 until str.size))) {
      (map, kv) => map += kv._1 -> (map.getOrElse(kv._1, Iterable[Int]()) ++ Iterable(kv._2))
    }.toMap

  //  question1("hhaaa") foreach println

  /*
   * 13.2
   * 重复前一个联系,这次用字符到列表的不可变映射
   */
  def question2(str: String): Map[Char, Iterable[Int]] =
    (Map[Char, Iterable[Int]]() /: (str zip (0 until str.size))) {
      (map, kv) => map ++ Map(kv._1 -> (map.getOrElse(kv._1, Iterable[Int]()) ++ Iterable(kv._2)))
    }
  //  question2("hhaaa") foreach println

  /*
   * 13.3
   * 编写一个函数, 从一个整型链表中去除所有0值
   */
  def question3(link: List[Int]): List[Int] = {
    link filter { _ != 0 }
    // 或者 link.foldLeft(List[Int]()) { (l, n) => if (n == 0) l else l :+ n }
  }

  /*
   * 13.4
   * 编写一个函数,接受一个字符串的集合,以及一个字符串到整型值的映射,
   * 返回整型集合, 其值为能和集合中某个字符串相对应的映射的值.
   * 举例来说,给定Array("Tom","Fred","Harry")和Map("Tom"->3,"Dick"->4,"Harry"->5),
   * 返回Array(3,5), 提示: 用flatMap将get返回的Option值组合在一起
   */
  def question4(arr: Array[String], map: Map[String, Int]): Array[Int] = {
    arr flatMap { map.get _ }
  }
  question4(Array("a", "b"), Map("a" -> 100, "b" -> 2)) foreach println

  /*
   * 13.5
   * 实现一个函数, 作用与mkString相同, 使用reduceLeft
   */
  trait IMakeString {
    this: Iterable[String] =>
    def makeString(): String = this reduceLeft { _ + _ }
  }
  val a = new ArrayBuffer[String]() with IMakeString
  a += "dd"
  println(a.makeString)

  /*
   * 13.6
   * 给定整型列表lst,
   * (lst :\ List[Int]()){_::_}
   * (List[Int]() /: lst){_:+_}
   * 以上2个表达式结果为得到队列自身
   * ----------------------------
   * 逆序排列
   */
  def question6(lst: List[Int]): List[Int] = {
    (List[Int]() /: lst) { (l, n) => n :: l }
    // 或者 (lst :\ List[Int]()) { (n, l) => l :+ n }
  }

  /*
   * 13.7
   * 表达式(prices zip quantities) map { p => p._1 * p._2 }有些不够优雅.
   * 我们不能用(prices zip quantities) map { _ * _ }, 因为_ * _是一个带两个参数的函数,
   * 而我们需要的是一个带单个类型为元组的参数的函数, 
   * Function对象的tupled方法可以将带两个参数的函数改为以元组为参数的函数.
   * 将tupled应用于惩罚函数,以便我们可以用它来映射由对偶组成的列表
   */
  def question7(arr1: Iterable[Int], arr2: Iterable[Int]): Iterable[Int] = {
    arr1 zip arr2 map { Function.tupled { (a, b) => a * b } }
  }

  /*
   * 13.8
   * 编写一个函数, 将Double数组转换成二维数组, 传入列数作为参数,
   * 举例来说, Array(1,2,3,4,5,6)和散列,返回Array(Array(1,2,3),Array(4,5,6)).
   * 用grouped方法
   */
  def question8[A](arr: Iterable[A], n: Int) = {
    arr.grouped(n)
  }
  question8(Array(1, 2, 3, 4), 2) foreach { println }

  /*
   * 13.9
   * Harry Hacker
   * 写了一个从命令行接受一系列文件名的程序, 对每个文件名, 
   * 他都启动一个新的线程来读取文件内容并更新一个字母出现频率映射,声明为:
   *   val frequencies = new scala.collection.mutable.HashMap[Char, Int]
   * 			 with scala.collection.mutable.SynchronizedMap[Char,Int]
   * 当读到字母c时, 他调用
   *   frequencies(c) = frequencies.getOrElse(c,0)+1
   * 为什么这样做得不到正确答案? 如果他用如下方式实现呢:
   *   import scala.collection.JavaConversions.asScalaConcurrentMap
   *   val frequencies: scala.collection.mutable.ConcurrentMap[Char, Int] = 
   *     new java.util.concurrent.ConcurrentHashMap[Char, Int]
   */
  
  /*
   * 13.10
   * Harry Hacker把文件读取到字符串中,
   * 然后想对字符串的不同部分用并行集合来并发地更新字母出现频率映射.
   * 他用了如下代码:
   *   val frequencies = new scala.collection.mutable.HashMap[Char, Int]
   *   for (c <- str.par) frequencies(c) = frequencies.getOrElse(c,0) + 1
   * 为什么说这个想法很糟糕? 要真正地并行化这个计算, 他该怎么做呢?
   */
  def question10(str: String): Map[Char, Int] = {
    str.par.aggregate(Map[Char, Int]())(
      (m, c) => m + (c -> (m.getOrElse(c, 0) + 1)),
      (a, b) => (Map[Char, Int]() /: (a.keys ++ b.keys)) {
        (m, c) => m + (c -> (a.getOrElse(c, 0) + b.getOrElse(c, 0)))
      })
  }
  question10("a" * 22 + "b" * 34) foreach println
}