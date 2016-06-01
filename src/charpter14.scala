
object charpter14 extends App {
  val p: PartialFunction[Int, String] = { case z if z % 2 == 0 => "Even" }
  val p2: PartialFunction[Int, String] = { case x if x % 2 == 1 => "Odd" }
  val pt = p.orElse(p2)

  def question2(pair: (Int, Int)): (Int, Int) = {
    pair match {
      case (x, y) => (y, x)
    }
  }

  def question3(arr: Array[Int]): Array[Int] = {
    arr match {
      case Array(a, b, rest @ _*) => Array(b, a) ++ rest
    }
  }

  /*
   * question4
   */
  abstract class Item(var name: String, var price: Double) {
    val model: String
    def this() {
      this("", 0)
    }
    override def toString(): String = model + "(name:" + name + ", price:" + price + ")"
  }
  case class Article() extends Item {
    val model = "Article"
    def this(name: String, price: Double) {
      this()
      this.name = name
      this.price = price
    }
  }
  case class Bundle(items: Item*) extends Item {
    val model = "Bundle"
    def this(name: String, price: Double) {
      this()
      this.name = name
      this.price = price
    }
  }
  case class Multiple(count: Int, item: Item) extends Item {
    val model = "Multiple_" + item.model
    this.name = item.name
    this.price = item.price
    override def toString(): String = count + " * " + super.toString
  }

  val m = new Multiple(3, new Bundle(new Article("sdsdsd", 33)))
  Predef println m

  /*
   * question5
   */
  def leafSum(tree: List[Any]): Int = {
    var sum = 0
    tree foreach {
      _ match {
        case n: Int => sum += n
        case lst: List[Any] => sum += leafSum(lst)
      }
    }
    sum
  }

  /*
   * question6 & 7 & 8
   *     +
   *    /|\
   *   * 2 -
   *  /\   |
   * 3  8  5
   * 计算 (3 * 8) + 2 + (-5) 
   */
  sealed abstract class BinaryTree {
    def leafSum(): Int
    var parent: BinaryTree = null
    var children: Iterable[BinaryTree] = null
    var op: (Int, Int) => Int = { _ + _ }
    var initValue = 0

    def this(oper: String) {
      this()
      this.op = eval(oper)
    }
    def eval(oper: String): (Int, Int) => Int = {
      oper match {
        case "+" => _ + _
        case "-" => _ - _
        case "*" => _ * _
        case "/" => _ / _
      }
    }
  }
  case class Leaf(value: Int) extends BinaryTree {
    def leafSum(): Int = value
  }
  case class Node() extends BinaryTree {
    def leafSum(): Int = (initValue /: children) { (sum, l) => op(sum, l.leafSum()) }

    def this(children: BinaryTree*) {
      this()
      this.children = children
      children foreach { _.parent = this }
    }
    def this(oper: String, children: BinaryTree*) {
      this(children: _*)
      if ("*/".contains(oper)) initValue = 1
      this.op = eval(oper)
    }
  }
  object Node {
    def apply(children: BinaryTree*) = new Node(children: _*)
    def apply(oper: String, children: BinaryTree*) = new Node(oper, children: _*)
  }
  val tree1 = Node(Leaf(2), Leaf(3), Node("*", Leaf(5), Leaf(5)), Node("-", Leaf(10)))
  Predef println tree1.leafSum()

  def question9(list: List[Option[Int]]): Int = {
    list collect { case Some(x) => x } sum
  }

  Predef println question9(List(None, Some(2), Some(2)))

  /*
   * question 10
   * 这里简单加一下
   */
  def compose(f1: Double => Option[Double], f2: Double => Option[Double]): Double => Option[Double] = {
    x =>
      (f1(x), f2(x)) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(a), Some(b)) => Some(a + b)
      }
  }
  Predef println compose(x => if (x > 0) Some(x) else None, x => Some(x * 2))(2)
}