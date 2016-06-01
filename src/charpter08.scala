
object charpter08 extends App {
  /*
   * question 1 to 2
   */
  class BankAccount(initBalance: Double) {
    var per = 0.001
    private var balance = initBalance

    def deposit(n: Double) {
      balance += n
    }
    def withdraw(n: Double) {
      balance -= n
    }

    def earnMonthlyInterest() {
      balance *= per
    }
  }
  class CheckingAccount(initBalance: Double) extends BankAccount(initBalance) {
    private var sum = 0
    override def deposit(n: Double) {
      super.deposit(if (sum < 3) n else n - 1)
      sum += 1
    }
    override def withdraw(n: Double) {
      super.withdraw(if (sum < 3) n else n + 1)
      sum += 1
    }
    override def earnMonthlyInterest() {
      super.earnMonthlyInterest()
      sum = 0
    }
  }

  /*
   * question 4
   */
  abstract class Item {
    def price(): Double
    def description(): String
  }
  class SimpleItem(var p: Double, var des: String) extends Item {

    def this(p: Double) {
      this(p, "")
    }

    def price(): Double = {
      p
    }

    def description(): String = {
      des
    }
  }
  class Bundle(var th: Array[SimpleItem]) extends Item {

    def price(): Double = {
      var p = 0.0
      th foreach { p += _.price() }
      p
    }

    def description(): String = {
      var s = ""
      th foreach { s += _.description }
      s
    }
  }
  /*
   * question 5
   */
  class Point(var x: Double, var y: Double) {}
  class LabelPoint(var des: String, x: Double, y: Double) extends Point(x, y) {

  }
  /*
   * question 6
   */
  abstract class Shape(var p: Point) {
    def centerPoint(): Point
  }
  class Rectange(p: Point) extends Shape(p) {
    def centerPoint(): Point = {
      p
    }
  }
  class Circle(p: Point) extends Shape(p) {
    def centerPoint(): Point = {
      p
    }
  }
  /*
   * question 7
   */
  class Square(p: java.awt.Point, d: java.awt.Dimension) extends java.awt.Rectangle(p, d) {
    def this() {
      this(new java.awt.Point(0, 0), new java.awt.Dimension(0, 0))
    }
    def this(p: java.awt.Point) {
      this(p, new java.awt.Dimension(0, 0))
    }
  }
}