
import java.io.File
import scala.io.Source

object charpter21 extends App {
  /*
   * question 1
   * "hello" -> 43会转化成("hello",43)是由于隐式转化的原因
   * ArrowAssoc[A]内定义了名为 -> [B](y: B): Tuple2[A, B]的方法
   * 当使用到类似于 "hello" -> 43 的实例时
   * 函数被调用, 返回Tuple对象
   */

  /*
   * question 2 & 3
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
   * question4
   * Read in aString askingFor "Your Name" and anInt askingFor "Your age" and aDouble askingFor "Your Weight"
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
   * question5
   * smaller(Fraction(1,3), Fraction(3,1))
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
   * question 6 
   */
  class PointOrdering6 extends java.awt.Point with Ordered[java.awt.Point] {
    def compare(that: java.awt.Point): Int =
      if (this.x > that.x) 1
      else if (this.x < that.x) -1
      else if (this.y > that.y) 1
      else if (this.y < that.y) -1
      else 0
  }

  /*
   * question 7
   */
  class PointOrdering7 extends java.awt.Point with Ordered[java.awt.Point] {
    def compare(that: java.awt.Point): Int =
      if (this.x * this.x + this.y * this.y > that.x * that.x + that.y * that.y) 1
      else if (this.x * this.x + this.y * this.y < that.x * that.x + that.y * that.y) -1
      else 0
  }
  type d = Int =:= AnyVal
  
  /*
   * question 9
   * 例如 
      def getFirstChar[T](arg: T)(implicit ev: T =:= String) = ev(arg).head
   *  事先编译器并不知道参数arg是什么类型的, 但由ev参数将其隐式地转化成了String, 就可以调用head方法了
   */
  def getFirstChar[T](arg: T)(implicit ev: T =:= String) = ev(arg).head
    
  /*
   * question 10
   */
  val r1 = "abc".map(_.toUpper)
  val r2 = "abc".map(_.toInt)
  Predef println r1
  Predef println r2
}