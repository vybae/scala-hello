
import java.io.File
import scala.io.Source

object charpter21 extends App {
  /*
   * 21.1
   * ->的工作原理是什么? 
   * 或者说, "Hello" -> 42 和 42 -> "Hello"怎么会和对偶扯上关系?
   * 
   * "hello" -> 43会转化成("hello",43)是由于隐式转化的原因
   * ArrowAssoc[A]内定义了名为 -> [B](y: B): Tuple2[A, B]的方法
   * 当使用到类似于 "hello" -> 43 的实例时
   * 函数被调用, 返回Tuple对象
   */

  /*
   * 21.2 & 21.3
   * 2)
   *   定义一个操作符+%, 将一个给定的百分比添加到某个值.
   *   举例来说,120 +% 10 等于132.
   *   提示由于操作符是方法, 而不是函数, 你需要提供一个implicit
   * 3)
   *   定义一个!操作符, 计算某个整数的阶乘. 举例来说, 5!应得到120.
   *   你将会需要一个经过丰富的类和一个隐式转换.
   */
  case class MyInt(v: Int) {
    def +%(op: Int) = v + v / 100.0 * op
    def !(): Int = {
      if (v == 1) 1
      else v * (MyInt(v - 1)!)
    }
  }
  implicit def int2MyInt(v: Int) = new MyInt(v)
  // q2
  //  Predef println 33 +% 10
  // q3
  //  Predef println (5!)

  /*
   * 21.4
   * 有些人很喜欢那些读起来隐约像英语句子的流利API.
   * 创建一个这样的API, 用来从控制台读取整数、浮点数以及字符串.
   * 例如:
   *   Read in aString askingFor "Your Name" and anInt askingFor "Your age" and aDouble askingFor "Your Weight"
   */
  def question4() {
    Read() in aString askingFor "Your Name" and anInt askingFor "Your age" and aDouble askingFor "Your Weight"
  }
  class Read {
    def in(op: TypeOP): Read = this
    def and(op: TypeOP): Read = this
    def askingFor(s: String): Read = this
  }
  object Read { def apply() = new Read() }
  class TypeOP {}
  case object aString extends TypeOP {}
  case object anInt extends TypeOP {}
  case object aDouble extends TypeOP {}

  /*
   * 21.5
   * 提供执行下述运算所需要的代码:
   *   smaller(Fraction(1,3), Fraction(3,1))
   * 给出一个扩展自Ordered[Fraction]的RichFraction类
   */
  case class Fraction(x: Int, y: Int) {
    def getVal() = x / y
    def *(that: Fraction) = new Fraction(this.x * that.x, this.y * that.y)
    override def toString() = "Fraction(" + x + ", " + y + ")"
  }
  def smaller(f1: Fraction, f2: Fraction)(implicit order: Fraction => Ordered[Fraction]) = if (order(f1) < f2) f1 else f2
  //申明一个隐式值
  implicit val sod: Fraction => Ordered[Fraction] = (f: Fraction) => new Ordered[Fraction] {
    override def compare(t: Fraction) =
      if (f.x / f.y < t.x / t.y) -1
      else if (f.x / f.y > t.x / t.y) 1
      else if (f.x % f.y != 0 || t.x % t.y != 0) {
        if (f.x % f.y < t.x % t.y) -1
        else if (f.x % f.y > t.x % t.y) 1
        else 0
      }
      else 0
  }
  //  Predef println smaller(Fraction(1, 2), Fraction(1, 3))

  /*
   * 21.6
   * 比较java.awt.Point类的对象, 按词典顺序比较(即依次比较x坐标和y坐标的值)
   */
  class PointOrdering6 extends java.awt.Point with Ordered[java.awt.Point] {
    def compare(that: java.awt.Point): Int =
      if (this.x > that.x || (this.x == that.x && this.y > that.y)) 1
      else if (this.x == that.x && this.y == that.y) 0
      else -1
  }

  /*
   * 21.7
   * 继续前一个练习, 根据两个点到原点的距离进行比较.
   * 你如何在两种排序之间切换?
   */
  class PointOrdering7 extends java.awt.Point with Ordered[java.awt.Point] {
    def compare(that: java.awt.Point): Int =
      if (this.x * this.x + this.y * this.y > that.x * that.x + that.y * that.y) 1
      else if (this.x * this.x + this.y * this.y < that.x * that.x + that.y * that.y) -1
      else 0
  }
  type d = Int =:= AnyVal
  
  /*
   * 21.8
   * 在REPL中使用implicitly命令来召唤隐式对象. 你得到了哪些对象?
   */
  
  /*
   * 21.9
   * 在Predef.scala中查找=:=对象,解释它的的工作原理
   * 
   * 例如 
      def getFirstChar[T](arg: T)(implicit ev: T =:= String) = ev(arg).head
   *  事先编译器并不知道参数arg是什么类型的, 但由ev参数将其隐式地转化成了String, 就可以调用head方法了
   */
  def getFirstChar[T](arg: T)(implicit ev: T =:= String) = ev(arg).head
    
  /*
   * 21.10
   * 表达式"abc".map(_.toUpper)的结果是一个String, 
   * 但"abc".map(_.toInt)的结果是一个Vector.
   * 为什么会这样?
   */
  val r1 = "abc".map(_.toUpper)
  val r2 = "abc".map(_.toInt)
  Predef println r1
  Predef println r2
}