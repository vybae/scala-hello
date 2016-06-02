
object charpter08 extends App {
  /*
   * 8.1 & 8.2
   * 1) 扩展如下的BankAccount类, 新类CheckingAcount对每次存款和取款都收取1美元的手续费,
   *  class BankAccount(initialBalance: Double) {
   *  	private var balance = initialBalance
   *    def deposit(amount: Double) = { balance += amount; balance }
   *    def withdraw(amount: Double) = { balance -= amount; balance }
   *  }
   * 2) 扩展前一个联系的BankAccount类, 新类SavingsAccount每个月都有利息产生(earnMonthlyInterest方法被调用),
   *  并且有每月三次免手续费的存款或取款, 在earnMonthlyInterest方法中重置交易计数
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
   * 8.3
   * 翻开你喜欢的Java或C++教科书, 一会找到用来讲解继承层级的示例, 可能是员工、宠物、图形或者类似的东西， 用Scala来实现这个示例
   */

  /*
   * 8.4
   * 定义一个抽象类Item, 假如方法price和description, SimpleItem是一个在构造器中给出价格和描述的物件,
   * 利用val可以重写def这个事实, Bundle是一个可以包含其他物件的物件, 其价格是所有打包物件的价格之和,
   * 同时提供一个将物件添加到打包当中的机制, 以及一个合适的description方法
   */
  abstract class Item {
    def price(): Double
    def description(): String
  }
  class SimpleItem(var p: Double, var des: String) extends Item {

    def this(p: Double) {
      this(p, "")
    }

    def price(): Double = p

    def description(): String = des
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
   * 8.5
   * 设计一个Point类, 其x和y坐标可以通过构造器提供,
   * 提供一个子类LabelPoint, 其构造器接受一个标签值和x & y坐标,
   * 比如:
   *   new LabeledPoint { "Black Thursday", 1929, 230.07 }
   */
  class Point(var x: Double, var y: Double) {}
  class LabelPoint(var des: String, x: Double, y: Double) extends Point(x, y) {}
  
  /*
   * 8.6
   * 定义一个抽象类Shape、一个抽象方法centerPoint，以及该抽象的子类Rectangle和Circle,
   * 为子类提供合适的构造器，并重写centerPoint方法
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
   * 8.7
   * 提供一个Square类，扩展自java.awt.Rectangle并提供三个构造器：
   *  一个以给定的端点和宽度构造正方形，
   *  一个以（0，0）为端点和宽度构造正方形，
   *  一个以（0，0）为端点、0为宽度构造正方形
   */
  class Square(p: java.awt.Point, d: java.awt.Dimension) extends java.awt.Rectangle(p, d) {
    def this() {
      this(new java.awt.Point(0, 0), new java.awt.Dimension(0, 0))
    }
    def this(p: java.awt.Point) {
      this(p, new java.awt.Dimension(0, 0))
    }
  }
  
  /*
   * 8.8
   * 编译8.6节中的Person和SecretAgent类并使用javap分析类文件, 总共有多少那么的getter方法?
   * 它们分别取什么值?
   */
  
  /*
   * 8.9
   * 在8.10节的Creature类中, 将val range替换成一个def, 如果你在Ant子类中也用def的话会有什么效果?
   * 如果在子类中使用val又会有什么效果? 为什么?
   */
  
  /*
   * 文件scala.collection.immutable.Stack.scala包含如下定义:
   *   class Stack[A] protected (protected val elems: List[A])
   * 请解释protected关键字的含义
   */
}