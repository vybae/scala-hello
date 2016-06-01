
object charpter17 extends App {
  def max[T <: Comparable[T]](arg0: T, arg1: T) = if (arg0.compareTo(arg1) > 0) arg0 else arg1

  /*
   * question1
   */
  class MyPair1[A, B](val p: (A, B)) {
    def swap(): MyPair1[B, A] = new MyPair1(p._2 -> p._1)
  }

  /*
   * question2
   */
  class MyPair2[T](var p: (T, T)) {
    def swap(): MyPair2[T] = new MyPair2[T](p._2 -> p._1)
  }

  /*
   * question3
   */
  def swap[A, B](p: (A, B)): (B, A) = (p._2 -> p._1)

  /*
   * question4
   * 
   */
  class Person(val name: String) {}
  class Student(name: String) extends Person(name) {}
  class MyPair4[T](val first: T, val second: T) {
    def replaceFirst[R >: T](newFirst: R) = new MyPair4[R](newFirst, second)
  }
  object MyPair4 {
    def apply[T](first: T, second: T) = new MyPair4[T](first, second)
  }
  val p = MyPair4[Student](new Student("a"), new Student("b"))
  p.replaceFirst(new Person("c"))

  /*
   * question5
   * 协变
   * 实现Comparable[Int],即可以调用compareTo(Int)也可以调用compareTo(RichInt)
   */

  /*
   * question6
   */
  def middle[T](lst: Iterable[T]) = lst.takeRight(lst.size / 2 + 1).head
  Predef println middle("hel")

  /*
   * question9
   * 
   * 下面的定义会报异常 
   * covariant type T occurs in contravariant position in type T of value newFirst
   * 因为协变类型T出现在了函数参数的逆变点
   * class MyPair9[+T](val p: (T, T)) {
   *   def replaceFirst(newFirst: T): MyPair9[T] = null
   * }
   */
  class MyPair9[T]() {
    def replaceFirst(newFirst: T): MyPair9[T] = null
  }
  class NastyDoublePair[T >: Double](val p: (T, T)) extends MyPair9[T] {
    override def replaceFirst(newFirst: T): NastyDoublePair[T] = new NastyDoublePair(math.sqrt(newFirst.asInstanceOf[Double]) -> p._2)
  }

  Predef println new NastyDoublePair[Double](2.0 -> 1.0).replaceFirst(16.0).p

  /*
   * question10
   */
  class MyPair10[A, B](val p: (A, B)) {
    def swap()(implicit ev: A =:= B): MyPair10[B, A] = new MyPair10(p._2 -> p._1)
  }
  val p10 = new MyPair10(1 -> 2)
  p10.swap
}