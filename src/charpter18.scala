
import scala.collection.mutable.ArrayBuffer

object charpter18 extends App {
  /*
   * question 1 & 2
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
   * question3
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
   * question4
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
   * question5
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
   * question6 
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
   * question7
   */
  def dealWithClose(obj: { def close(): Unit }) {
    try {

    }
    catch {
      case t: Throwable => obj.close()
    }
  }

  /*
   * question8
   */
  def printValues(f: { def apply(x: Int): Int }, from: Int, to: Int) {
    from to to foreach { x => println(f.apply(x)) }
  }
  //  printValues((x: Int) => x * x, 1, 3)
  //  printValues(Array(1,2,3,4,5,6,7),1,3)

  /*
   * question9
   */
  abstract class Dim[T](val value: Double, val name: String) {
    protected def create(v: Double): Dim[T]
    def +(other: Dim[T]) = create(value + other.value)
    override def toString() = value + " " + name
  }
  class Seconds(v: Double) extends Dim[Seconds](v, "s") {
    override def create(v: Double) = new Seconds(v)
  }
  class Meters(v: Double) extends Dim[Meters](v, "m") {
    override def create(v: Double) = new Meters(v)
  }
  val sec1 = new Seconds(1)
  val sec2 = new Seconds(2)
  sec1 + sec2

  /*
   * question10
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