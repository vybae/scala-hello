package other

import sys.process._
import java.io.File
import scala.actors._
import scala.actors.Actor._

object hello {

  /*
   * 同步消息
   */
  val account = actor {
    var balance = 100.0
    while (true) {
      receive {
        case Deposit(amount) => {
          balance += amount
          sender ! Balance(balance)
        }
      }
    }
  }

  val reply = account !? Deposit(333)
  reply match {
    case Balance(x) => println("current balance is : " + x)
  }

  /*
   * 消息通道
   */
  def main(args: Array[String]): Unit = {
    var str = "abcdefghijklmnopqrstuvwxyz"
    for (i <- 0 until 26) str += ('A' + i).asInstanceOf[Char]
    Predef println str
  }

  def test() = {
    val master = actor {
      var count = 0
      var sum = 0.0
      val channel = new Channel[Int]

      1 to 10 foreach {
        x => new Sub().start() ! Messager(Array(1, 2, 3, 4), channel);
      }
      loopWhile(count < 10) {
        channel.react {
          case v: Int =>
            count += 1
            sum += v
            println(sum)
        }
      }
    }
    master.start()
  }
}
case class Deposit(n: Double) {}
case class Balance(n: Double) {}

case class Messager(input: Iterable[Int], out: OutputChannel[Int]) {}
class Sub extends Actor {
  def act(): Unit = {
    react {
      case Messager(input, channel) => channel ! input.sum
    }
  }
}
