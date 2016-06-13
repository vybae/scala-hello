/*
 * o(∩_∩)o 第二章，欢迎一起学scala，记得把注释取消了哦~
 * welcome to learn scala together, remember cancelling comments o~
 */
object charpter02 {
  /*
   * 2.1 
   * 一个数字如果为正数，则它的signum为1;
   * 如果是负数,则signum为-1;
   * 如果为0,则signum为0.编写一个函数来计算这个值
   * */
  //  def signum(x: Int): Int = {
  //    if (x > 0) { 1 }
  //    else if (x < 0) { -1 }
  //    else { 0 }
  //  }
  //  def signum(x: Int): Int = if (x > 0) 1 else if (x < 0) -1 else 0;
  //  def signum(x: Int) = { if (x > 0) 1 else if (x < 0) -1 else 0 }
  def signum(x: Int) = if (x > 0) 1 else if (x < 0) -1 else 0

  /*
   * 2.2 
   * 一个空的块表达式{}的值是什么？类型是什么？
   * */
  //  def checkEmptyBlockType() = {
  //    println({});
  //    println({}.getClass())
  //  }
  def checkEmptyBlockType { println({}); println({}.getClass()) }

  /*
   * 2.3 
   * 指出在Scala中何种情况下赋值语句x=y=1是合法的。
   * (提示：给x找个合适的类型定义)
   */
  def checkAssignLegal {
    var x: Unit = ()
    println("x's type is: " + x.getClass)
    var y = 1
    x = y = 1
  }

  /*
   * 2.4 
   * 针对下列Java循环编写一个Scala版本:
   * for(int i=10;i>=0;i–)System.out.println(i);
   */
  def ScalaForeach {
    //    1.to(10).reverse.foreach { (i: Int) => Predef.println(i) }
    //    1.to(10).reverse.foreach { i => Predef println i }
    //    1.to(10).reverse.foreach { i => println(i) }
    //    1.to(10).reverse foreach { println _ }
    (1 to 10 reverse) foreach println
  }

  /*
   * 2.5 
   * 编写一个过程countdown(n:Int)，打印从n到0的数字
   */
  def countdown(n: Int) {
    n match {
      case n if n >= 0 => {
        (0 to n reverse) foreach println
      }
      case n if n < 0 => {
        n to 0 foreach println
      }
    }
  }

  /*
   * 2.6 
   * 编写一个for循环,计算字符串中所有字母的Unicode代码的乘积。
   * 举例来说，"Hello"中所有字符串的乘积为9415087488L
   */
  def calculateCharsUnicodeProduct(s: String) = {
    var res: Long = 1
    s foreach { res *= _.toLong }
    res
  }

  /*
   * 2.7 
   * 同样是解决前一个练习的问题，但这次不使用循环。
   * （提示：在Scaladoc中查看StringOps）
   */
  //  def computeCharsUnicodeProduct(s: String) = (1.toLong /: s) { _ * _ }  // 这是what鬼，以后会学到哦
  def computeCharsUnicodeProduct(s: String) = s.foldLeft(1.toLong) { _ * _ }

  /*
   * 2.8 
   * 编写一个函数product(s:String)，
   * 计算前面练习中提到的乘积
   * 2.9 
   * 把前一个练习中的函数改成递归函数
   */
  //  def product(s: String) = { // 对吗?为毛呢?
  //    if (s.length() == 1) {
  //      s(0) toLong
  //    } else {
  //      s(0).toLong * product(s.tail)
  //    }
  //  }
  def product(s: String): Long = {
    if (s.length() == 1) s(0) toLong
    else s(0).toLong * product(s.tail)
  }

  /*
   * 2.10 
   * 编写函数计算xn,其中n是整数，使用如下的递归定义:
   */
  def question10(x: Int, n: Int): BigInt = n match {
    case 0 => 1
    case n if n < 0 => 1 / question10(x, -n)
    case n if n % 2 == 0 => question10(x, n / 2) pow 2
    case n if n % 2 == 1 => x * question10(x, n - 1)
  }

  def main(args: Array[String]): Unit = {
    // checkEmptyBlockType() //取消了运行下试试看呢?
  }
}