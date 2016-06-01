
import scala.collection.mutable.ArrayBuffer

object charpter11 extends App {
  def question1() {
    val res1 = 3 + 4 -> 5
    //3 -> 4 + 5 错的
    val res2 = 3 -> (4 + 5)
    println(res1)
    println(res2)
  }

  /*
   * question3
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
   * question 4  
   * 不该提供 *和/
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
   * question 5
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
   * question6
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
   * question 7
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
   * question 8
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
   * question 9 & 10
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