
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.meta.beanGetter
import scala.annotation.varargs
import scala.annotation.tailrec

object charpter15 extends App {

  def question1(str: String) {

    val m = Map(1 -> 2).get(1): @unchecked
  }

  /*
   * question 2
   */
  @deprecated class TestAnnotation @deprecated() (@deprecated(message= "aaa") var name: Int) {
    @deprecated var age: Int = _

    @deprecated def fun1[@deprecated T](@deprecated arg:T) {}
  }

  /*
   * question 4
   */
  @varargs def sum(arg: String*) = {
    
  }

  /*
   * question 6
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
   * question 7
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
   * question 8
   */
  object TestSpecialized {
    def allDifferent[@specialized(Int, Long) T](start: T, end: T): (T, T) = {
      (start, end)
    }
  }

  /*
   * question 10
   */
  def factorial(n: Int): Int = {
    assert(n > 0)
    n
  }
  Predef println factorial(0)
}