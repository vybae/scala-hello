
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.meta.beanGetter
import scala.annotation.varargs
import scala.annotation.tailrec

object charpter15 extends App {

  /*
   * 15.1
   * 编写四个JUnit测试案例, 分别使用带或不带某个参数的@Test注解. 用JUnit执行这些测试
   */
  def question1(str: String) {

    val m = Map(1 -> 2).get(1): @unchecked
  }

  /*
   * 15.2
   * 创建一个类的示例, 展示注解可以出现的所有位置.
   * 用@deprecated作为你的示例注解.
   */
  @deprecated class TestAnnotation @deprecated() (@deprecated(message= "aaa") var name: Int) {
    @deprecated var age: Int = _

    @deprecated def fun1[@deprecated T](@deprecated arg:T) {}
  }
  
  /*
   * 15.3
   * Scala类库中的哪些注解用到了元注解@param、@field、@getter、
   * @setter、@beanGetter或@beanSetter?
   */

  /*
   * 15.4
   * 编写一个Scala方法sum, 带有可变长度的整型参数, 返回所有参数之和.
   * 从Java调用该方法
   */
  @varargs def sum(arg: Int*) = arg.sum

  /*
   * 15.5
   * 编写一个返回包含某文件所有行的字符串的方法. 
   * 从Java调用该方法.
   */
  
  /*
   * 15.6
   * 编写一个Scala对象, 该对象带有一个易失(volatile)的Boolean字段.
   * 让某一个线程睡眠一段时间, 然后将该字段设为true, 打印消息, 然后退出.
   * 而另一个线程不停地检查该字段是否为true, 如果是,它将打印一个消息并退出.
   * 如果不是, 他将短暂睡眠, 然后重试, 如果变量不是易失的, 会发生什么?
   * 
   */
  object TestVolatile {
    @volatile var flag = false

    def change(): Boolean = {
      if (flag) flag = false else flag = true
      flag
    }

    def run() {

    }
  }

  /*
   * 15.7
   * 给出一个示例, 展示如果方法可被重写, 则尾递归优化为非法.
   */
  @tailrec final def recurse(range: Iterable[Int], init: Int): Int = {
    if (range.tail == Nil) {
      init
    } else {
      recurse(range.tail, init + range.head)
    }
  }

  Predef println recurse(1 to 4, 0)

  /*
   * 15.8
   * 将allDifferent方法添加到对象, 编译并检查字节码. 
   * @specialized注解产生了哪些方法?
   */
  object TestSpecialized {
    def allDifferent[@specialized(Int, Long) T](start: T, end: T): (T, T) = {
      (start, end)
    }
  }

  /*
   * Range.foreach方法被注解为@specialized(Unit). 为什么?
   * 通过一下命令检查字节码:
   *   javap -classpath /path/to/scala/lib/scala-library.jar
   *     scala.collection.immutable.Range
   * 并考虑Function1上的@specialized注解. 
   * 点击Scaladoc中的Function.scala链接进行查看
   */
  
  /*
   * 15.10
   * 添加assert(n >= 0)到factorial方法, 在启用断言的情况下编译并校验factorial(-l)会抛异常.
   * 在禁用断言的情况下编译. 会发生什么?
   * 用javap检查该断言调用
   */
  def factorial(n: Int): Int = {
    assert(n > 0)
    n
  }
  Predef println factorial(0)
}