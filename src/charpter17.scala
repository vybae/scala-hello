
object charpter17 extends App {
  def max[T <: Comparable[T]](arg0: T, arg1: T) = if (arg0.compareTo(arg1) > 0) arg0 else arg1

  /*
   * 17.1
   * 定义一个不可变类Pair[T,S]，带一个swap方法，返回组件交换过位置的新对偶。
   */
  class MyPair1[A, B](val p: (A, B)) {
    def swap(): MyPair1[B, A] = new MyPair1(p._2 -> p._1)
  }

  /*
   * 17.2
   * 定义一个可变类Pair[T]，带一个swap方法，交换对偶中组件的位置。
   */
  class MyPair2[T](var p: (T, T)) {
    def swap(): MyPair2[T] = new MyPair2[T](p._2 -> p._1)
  }

  /*
   * 17.3
   * 给定类Pair[T,S]，编写一个泛型方法swap，接受对偶作为参数并返回组件交换过位置的新对偶。
   */
  def swap[A, B](p: (A, B)): (B, A) = (p._2 -> p._1)

  /*
   * 17.4
   * 在17.3节中，如果我们想把Pair[Person]的第一个组件替换成Student，为什么不需要给replaceFirst方法定一个下界？
   * --因为Student是Pair的子类（型），所以不需要给replaceFirst方法定一个下界即可把Pair[Person]的第一个组件替换成Student。
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
   * 17.5
   * 为什么RichInt实现的是 Comparable[Int]而不是Comparable[RichInt]？
   * --由于隐式转换的原因, Int可以被隐式转换成RinchInt, 所以 实现Comparable[Int]可以更通用,
   * 即可以调用compareTo(Int)也可以调用compareTo(RichInt)
   */

  /*
   * 17.6
   * 编写一个泛型方法middle，返回任何Iterable[T]的中间元素。举例来说，middle["World"]应得到'r'。
   */
  def middle[T](lst: Iterable[T]) = lst.takeRight(lst.size / 2 + 1).head
  //  Predef println middle("hello")

  /*
   * 17.7
   * 查看Iterable[+A]特质。哪些方法使用了类型参数A？为什么在这些方法中类型参数位于协变点？"
   * 
   * --foldLeft, foldRight, groupBy 等方法使用了类型参数A。因为在函数参数中，型变是反转过来的——它的参数是协变的。
   */
  
  /*
   * 17.8
   * 在17.10节中，replaceFirst方法带有一个类型界定。为什么你不能对可变的Pair[T]定义一个等效的方法？
   *   def replaceFirst[R >: T](newFirst : R) { first = newFirst)  //错误
   *   
   * --
   */
  
  /*
   * 17.9
   *  在一个不可变类Pair[+T]中限制方法参数看上去可能有些奇怪。不过，先假定你可以在Pair[+T]中定义def replaceFirst(newFirst : T)
   *  问题在于，该方法可能会被重写（以某种不可靠的方式）。构造出这样的一个示例。
   *  定义一个Pair[Double]的子类NastyDoublePair，重写replaceFirst方法，用newFirst的平方根开做新对偶。
   *  然后对实际类型为 NastyDoublePair的 Pair[Any]调用 replaceFirst("Hello")。
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
   * 17.10
   * 给定可变类Pair[S, T]，使用类型约束定义一个swap方法，当类型参数相同时可以被调用。
   */
  class MyPair10[A, B](val p: (A, B)) {
    def swap()(implicit ev: A =:= B): MyPair10[B, A] = new MyPair10(p._2 -> p._1)
  }
  val p10 = new MyPair10(1 -> 2)
  p10.swap
}