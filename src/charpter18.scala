
import scala.collection.mutable.ArrayBuffer

object charpter18 extends App {
  /*
   * 18.1 & 18.2
   * 1)
   *   实现一个Bug类，对沿着水平线爬行的虫子建模。move方法向当前方向移动，turn方法让虫子转身，show方法打印出当前的位置。
   *   让这些方法可以被串接调用。例如： 
   *   bugsy.move(4).show().move(6).show().turn().move(5).show()
   *   上述代码应显示 4 10 5。
   * 2)
   *   为前一个练习中的Bug类提供一个流利接口，达到能编写如下代码的效果：
   *   bugsy move 4 and show and then move 6 and show turn around move 5 and show
   *   
   */
  class Bug {
    var position = 0
    var op_index = 0
    val op = Array((x: Int, y: Int) => x + y, (x: Int, y: Int) => x - y)

    def move(n: Int): this.type = {
      this.position = op(op_index)(this.position, n)
      this
    }
    def turn(): this.type = {
      this.op_index = (this.op_index + 1) % 2
      this
    }
    def show(): this.type = {
      println(this.position);
      this
    }

    def and(a: BugAction): this.type = {
      a match {
        case charpter18.then => this
        case charpter18.show => this.show()
        case charpter18.turn => this.turn()
      }
    }
  }
  class BugAction {}
  case object then extends BugAction {}
  case object show extends BugAction {}
  case object turn extends BugAction {}

  val bugsy = new Bug
  bugsy.move(3).show().turn().move(1).show()
  bugsy move 3 and show and turn and then move 3 and show and then move 5 and show

  /*
   * 18.3
   * 完成18.1节中的流利接口，以便我们可以做出如下调用：
   * book set Title to "Scala for the Impatient" set Author to "Cay Horstmann"
   */
  class Book(var title: String, var author: String) {
    var nextAction: BookAction = _

    def set(action: BookAction): this.type = {
      action match {
        case Title => {
          nextAction = Title
          this
        }
        case Author => nextAction = Author; this
      }
    }

    def to(arg: String): this.type = {
      nextAction match {
        case Title => {
          title = arg
          this
        }
        case Author => author = arg; this
      }
    }
  }
  class BookAction
  case object Title extends BookAction
  case object Author extends BookAction

  new Book("c", "d") set Title to "a" set Author to "b"

  /*
   * 18.4
   * 实现18.2节中被嵌套在Network类中的Member类的equals方法。两个成员要想相等，必须属于同一个网络。
   */
  class Network {
    class Member(val name: String) {
      val contacts = new ArrayBuffer[Member]
      def equals(m: Member): Boolean = m.isInstanceOf[Member]
    }

    private val members = new ArrayBuffer[Member]

    def join(name: String) = {
      val m = new Member(name)
      members += m
      m
    }
  }

  /*
   * 18.5
   * 考虑如下类型别名
   *   type NetworkMember = n.Member forSome { val n: Network }
   * 和函数
   *   def process(m1: NetworkMember, m2: NetworkMember) = (m1, m2)
   * 这与18.8节中的process函数有什么不同？
   * 
   * process 和 process2的区别是
   * process接受相同或不同网络的成员
   * 而18.8节中的函数(process2)则拒绝那些来自不同网络的成员。
   */
  type netMember = n.Member forSome { val n: Network }
  def process(m1: netMember, m2: netMember) = (m1, m2)
  def process2[M <: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2) //Network#Member

  val chatter = new Network
  val myFace = new Network
  val fred = chatter.join("Fred")
  val wilma = chatter.join("Wilma")
  val barney = myFace.join("Barney")
  process(fred, wilma) // OK  接受相同网络的成员
  process(fred, barney) // OK 接受不同网络的成员
  //  process2(fred, barney) // ERROR
  fred.contacts += wilma // OK
  //  fred.contacts += barney //ERROR

  /*
   * 18.6
   * Scala 类库中的Either类型可以被用于要么返回结果，要么返回某种失败信息的算法。
   * 编写一个带有两个参数的函数：一个已排序整型数组和一个整数值。要么返回该整数值在数组中的下标，要么返回最接近该值的元素的下标。
   * 使用一个中置类型作为返回类型。
   */
  def testScalaEither(seq: Seq[Int], value: Int): Array[Int] Either Int = {
    val idx = seq.indexOf(value)
    if (idx > -1) Right(idx)
    else {
      val lst = seq zip (0 until seq.length) map { kv => kv._2 -> math.abs(kv._1 - value) } toArray

      Left((Array(lst.head) /: lst.tail) {
        (lst, kv) =>
          {
            if (kv._2 < lst(0)._2) Array(kv)
            else if (kv._2 == lst(0)._2) lst :+ kv
            else lst
          }
      } map { _._1 })
    }
  }
  println("------------------")
  val tse = testScalaEither(Seq(1, 2, 3, 5), 4)
  if (tse.isRight) println(tse) else tse.left.get foreach println

  /*
   * 18.7
   * 实现一个方法，接受任何具备如下方法的类的对象和一个处理该对象的函数。
   * 调用该函数，并在完成或有任何异常发生时调用close方法。
   */
  def dealWithClose(obj: { def close(): Unit }) {
    try {
      ???
    }
    catch {
      case t: Throwable => obj.close()
    }
  }

  /*
   * 18.8
   * 编写一个函数printValues, 带有三个参数f、from和to, 打印出所有给定区间范围内的值经过f计算后的结果, 
   * 这里的f应该是任何带有接受Int产出Int的apply方法的对象. 例如:
   *   printValues { (x: Int) => x * x,3,6 } // 将打印 9 16 25 36
   *   printValues { Array(1,1,2,3,5,8,13,21,34,55),3,6 } //将打印 3 5 8 13
   */
  def printValues(f: { def apply(x: Int): Int }, from: Int, to: Int) {
    from to to foreach { x => println(f.apply(x)) }
  }
  //  printValues((x: Int) => x * x, 1, 3)
  //  printValues(Array(1,2,3,4,5,6,7),1,3)

  /*
   * 18.9
   * 考虑如下对物理度量建模的类:
   *   abstract class Dim[T](val value: Double, val name: String) {
   *     protected def create(v: Double): T
   *     def +(other: Dim[T]) = create(value + other.value)
   *     override def toString() = value + " " + name
   *   }
   * 以下是具体子类:
   *   class Seconds(v: Double) extends Dim[Seconds] (v, "s") {
   *     override def create(v: Double) = new Seconds(v)
   *   }
   * 但不清楚状况的人可能会定义
   *   class Meters(v: Double) extends Dim[Meters](v, "m") {
   *     override def create(v: Double) = new Seconds(v)
   *   }
   * 允许米(Meter)和秒(Second)相加
   * 使用自身类型来防止发生这样的情况
   */
  abstract class Dim[T](val value: Double, val name: String) {
    this: T =>
    protected def create(v: Double): T
    def +(other: Dim[T]) = create(value + other.value)
    override def toString() = value + " " + name
  }
  class Seconds(v: Double) extends Dim[Seconds](v, "s") {
    override def create(v: Double) = new Seconds(v)
  }
  //如果定义成 class Meters(v: Double) extends Dim[Seconds](v, "m") 将会报错
  class Meters(v: Double) extends Dim[Meters](v, "m") {
    override def create(v: Double) = new Meters(v)
  }
  val sec1 = new Seconds(1)
  val sec2 = new Meters(2)
  // sec1 + sec2 // 报错, 无法相加

  /*
   * 18.10
   * 自身类型通常可以被扩展自类的特质替代,但某些情况下使用自身类型会改变初始化和重写的顺序, 构造出这样的一个示例 
   */
  class Base { val x = 0 }
  class Ext extends Base {}
  trait ITest1 {
    this: Base =>
    override val x = 1
  }
  trait ITest2 {
    this: Base =>
    override val x = 2
  }
  val ext = new Ext with ITest1 with ITest2
  val ext2 = new Ext with ITest2 with ITest1
  Predef println "ext: " + ext.x + " -- ext2: " + ext2.x
}