
import scala.collection.mutable.ArrayBuffer

object charpter11 extends App {
  /*
   * 11.1
   * 根据优先级规则，3+4 -> 5 和3 -> 4+5是如何被求值的?
   */
  def question1() {
    val res1 = 3 + 4 -> 5
    //3 -> 4 + 5 错的
    val res2 = 3 -> (4 + 5)
    println(res1)
    println(res2)
  }
  
  /*
   * 11.2
   * BigInt类有一个pow方法,但没有用操作符字符.Scala类库的设计者为什么没有选用**或者^作为乘方操作符呢?
   */

  /*
   * 11.3
   * 实现Fraction类, 支持 + - * / 操作,
   * 支持约分, 列入将15/-6变成-5/-2,
   * 除以最大公约数,像这样:
   * class Fraction(n:Int, d:Int) {
   * 	private
   * }
   */
  class Fraction {
    var n: Int = _
    var d: Int = _

    def +(that: Fraction): Fraction = Fraction(this.n * that.d + that.n * this.d, this.d * that.d)
    def -(that: Fraction): Fraction = Fraction(this.n * that.d - that.n * this.d, this.d * that.d)
    def *(that: Fraction): Fraction = Fraction(this.n * that.n, this.d * that.d)
    def /(that: Fraction): Fraction = Fraction(this.n * that.d, this.d / that.n)

    def this(n: Int, d: Int) {
      this()
      this.n = n
      this.d = d
      simplify()
    }

    def simplify() {
      val r = n % d
      if (r == 0) { //6 3
        n /= d
        d = 1
      }
      else if (d % r == 0) { // 6 4
        n /= r
        d /= r
      }
    }

    override def toString() = 1.0 * n / d toString
  }
  object Fraction {
    def apply(n: Int, d: Int): Fraction = new Fraction(n, d)
  }

  Predef println Fraction(1, 2) - Fraction(1, 4)

  /*
   * 11.4  
   * 实现一个Money类,加入美元和美分字段。提供+,-操作符以及比较操作符==和<。
   * 举例来说，Money(1,75)+Money(0,50)==Money(2,25)应为true。
   * 你应该同时提供*和/操作符吗？为什么？
   */
  class Money(var cent: Int) {

    def this(dollar: Int, cent: Int) {
      this(dollar * 100 + cent)
    }

    def +(that: Money): Money = Money(this.cent + that.cent)
    def -(that: Money): Money = Money(this.cent - that.cent)
    def ==(that: Money): Boolean = this.cent == that.cent
  }
  object Money {
    def apply(cent: Int) = new Money(cent)
    def apply(dollar: Int, cent: Int) = new Money(dollar, cent)
  }

  Predef println Money(1, 50) == Money(0, 150)

  /*
   * 11.5
   * 提 供操作符用于构造HTML表格。
   * 例如:
   *   Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM,.NET"
   * 应产出:
   *   <table><tr><td>Java</td></tr><td>Scala</td></tr><tr><td>Gosling…
   */
  class Table {
    val eles = ArrayBuffer[ArrayBuffer[String]](ArrayBuffer[String]())
    var curs = 0

    def |(s: String): Table = {
      eles(curs) append s
      this
    }
    def ||(s: String): Table = {
      curs += 1
      eles append ArrayBuffer[String](s)
      this
    }

    override def toString(): String = {
      var res = "<table>"
      eles foreach { x =>
        {
          res += "<tr>"
          x foreach { y => res += "<td>" + y + "</td>" }
          res += "</tr>"
        }
      }
      res + "</table>"
    }
  }
  object Table {
    def apply(): Table = new Table
  }

  Predef println Table() | "a" | "b" || "c" | "d"

  /*
   * 11.6
   * 提供一个ASCIIArt类，其对象包含类似这样的图形:
   *  /\_/\
	 * ( ' ' )
   * (  -  )
 	 *  | | |
   * (__|__)
   * 提供将两个ASCIIArt图形横向或纵向结合的操作符。选用适当优先级的操作符命名。
   * 纵向结合的示例
   *   /\_/\     -----
   *  ( ' ' )  / Hello \
   *  (  -  ) <  Scala |
   *   | | |   \ Coder /
   *  (__|__)    -----
   */
  class ASCIIArt(val eles: Array[String]) {

    def +(that: ASCIIArt): ASCIIArt = {
      val len = if (this.eles.length > that.eles.length) this.eles.length else that.eles.length
      val res = new Array[String](len)
      0 until len foreach { i => res(i) = (if (i < this.eles.length) this.eles(i) else "") + (if (i < that.eles.length) that.eles(i) else "") }
      ASCIIArt(res)
    }

    override def toString(): String = {
      var res = ""
      eles foreach { res += _ + "\r\n" }
      res
    }
  }
  object ASCIIArt {
    def apply(eles: Array[String]): ASCIIArt = new ASCIIArt(eles)
  }

  Predef println ASCIIArt(Array("|    |", " |  | ", "  ||  ")) + ASCIIArt(Array("  ||  ", " |  | ", "|    |"))

  /*
   * 11.7
   * 实现一个BigSequence类,将64个bit的序列打包在一个Long值中。
   * 提供apply和update操作来获取和设置某个具体的bit
   */
  class BigSequence {
    var bits = new Array[Boolean](64)
    def this(bits: Array[Boolean]) = {
      this()
      this.bits = bits
    }
    def apply(n: Int): Boolean = this.bits(n)
    def update(n: Int, value: Boolean): Unit = bits(n) = value
  }

  val bs = new BigSequence
  bs(45) = true
  Predef println bs(45)

  /*
   * 11.8
   * 提供一个Matrix类—你可以选择需要的是一个2*2的矩阵，任意大小的正方形矩阵，
   * 或m*n的矩阵。支持+和*操作。*操作应同样适用于单值，例如mat*2。
   * 单个元素可以通过mat(row,col)得到
   */
  class Matrix(var row: Int, var col: Int) {
    def +(that: Matrix): Matrix = Matrix(this.row + that.row, this.col + that.col)
    def +(n: Int): Matrix = Matrix(this.row + n, this.col + n)
    def *(that: Matrix): Matrix = Matrix(this.row * that.row, this.col * that.col)
    def *(n: Int): Matrix = Matrix(this.row * n, this.col * n)

    override def toString(): String = {
      var s = ""
      0 until row foreach { x =>
        {
          0 until col foreach { y => s += "*" }
          s += "\r\n"
        }
      }
      s
    }
  }
  object Matrix {
    def apply(row: Int, col: Int) = new Matrix(row, col)
  }

  Predef println Matrix(3, 4) + 2

  /*
   * 11.9 & 11.10
   * 9)
   *   为RichFile类定义unapply操作，提取文件路径，名称和扩展名。
   *   举例来说，文件/home/cay/readme.txt的路径为/home/cay,
   *   名称为readme,扩展名为txt
   * 10)
   *   为RichFile类定义一个unapplySeq，提取所有路径段。
   *   举例来说，对于/home/cay/readme.txt，
   *   你应该产出三个路径段的序列:home,cay和readme.txt
   */
  class RichFile(val file: java.io.File) {
  }
  object RichFile {

    def apply(file: java.io.File) = new RichFile(file)

    def unapply(rf: RichFile): Option[(String, String, String)] = {
      if (rf.file.exists()) {
        val reg = """(.+)\.([^.]+)""".r
        val reg(name, ext) = rf.file.getName
        Some(rf.file.getPath.dropRight(rf.file.getName.length + 1), name, ext)
      }
      else {
        None
      }
    }

    def unapplySeq(p: String): Option[Seq[String]] = {
      val f = new java.io.File(p)
      if (f.exists()) {
        val path = f.getPath.dropRight(f.getName.length + 1)
        Some(path.split("\\"))
      }
      else {
        None
      }
    }
  }
}