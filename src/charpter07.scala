
object charpter07 extends App {
  /*
   * 7.1
   * 编写示例程序, 展示为什么
   *   package com.hot.impatient
   * 不同于
   *   package com
   *   package hot
   *   package impatient
   *   
   * 答: 
   *   package com {
   *     def fun1(){}
   *     packagehot {
   *     	 def fun2(){}
   *         package impatient {
   *           def fun2(){}
   *         }
   *     }
   *   }
   * 第一种引用方式是无法调用到fun1和fun2的
   * 而第二种引用方式可以调用到所有com包下的函数
   */

  /*
   * 7.2
   * 编写一段让你的scala朋友们感到困惑的代码, 使用一个不在顶部的com包
   * (略...)
   */

  /*
   * 7.3
   * 编写一个包random, 加入函数nextInt():Int, nextDouble():Double 和setSeed(seed:Int):Unit,
   * 生成随机数的算法采用线性同余生成器:
   *   后值=(前值 * a + b) mod (2 pow n)
   * 其中, a=1664525, b=1013904223, 难2, 前值的初始值为seed
   */
  def question3(): Unit = {
    object random {
      var seed: Int = _
      val a = BigDecimal(1664525)
      val b = BigDecimal(1013904223)
      val n = 32

      def nextInt(): Int = {
        val temp = (seed * a + b) % BigDecimal(2).pow(n)
        seed = temp.toInt
        seed
      }

      def nextDouble(): Double = {
        val temp = (seed * a + b) % BigDecimal(2).pow(n)
        seed = temp.toInt
        temp.toDouble
      }
    }

    Predef println random.nextDouble()
    Predef println random.nextDouble()
    Predef println random.nextDouble()
  }
  
  /*
   * 7.4
   * 在你看来Scala的设计者为什么要提供package object语法而不是简单的让你将函数和变量添加到包中呢？
   */
  
  /*
   * 7.5
   * private[com] def giveRaise(rate:Double)的含义是什么？有用吗？
   * 除了com包可访问，其他包都不能访问。
   */

  /*
   * 7.6
   * 编写一段程序, 将java哈希映射中的所有元素拷贝到scala哈希映射,
   * 用引入语句重命名这两个类
   */
  def question6(): Unit = {

    import java.util.{ HashMap => JavaHashMap }
    import scala.collection.mutable.HashMap

    val map = new JavaHashMap[String, String]()
    map.put("1", "a")
    map.put("2", "b")
    map.put("3", "c")

    val smap = new HashMap[String, String]()

    map.keySet().toArray foreach { key =>
      smap += (key.toString -> map.get(key))
    }

    println(smap.mkString)
  }
  
  /*
   * 7.7
   * 在前一个练习中，将所有引入语句移动到尽可能小的作用域里
   */
  
  /*
   * 7.8
   * 以下代码的作用是什么？这是个好主意吗？ 
   *   import java._ 
   *   import javax._
   *  
   *  引入java和javax下所有的子包, 不是个好主意, 引入的作用域太大, 容易引发命名冲突
   */

  /*
   * 7.9
   * 编写一段程序, 引入java.lang.System类,从user.name系统属性读取用户名,
   * 从console对象读取一个密码, 如果密码不是"secret", 则在标准错误流中打印一个消息,
   * 如果密码 是"secret", 则在标准输出流中打印一个问候消息,
   * 不要使用任何其他引入, 也不要使用任何限定词(带据点的那种)
   */
  def question9(): Unit = {
    val pwd = scala.io.StdIn.readLine()
    if (pwd == "secret") println("hello, " + System.getProperty("user.name"))
    else System.err.println("password is wrong")
  }
  
  /*
   * 7.10
   * 除了StringBuilder,还有哪些java.lang的成员是被scala包覆盖的？
   */

  question9()
}