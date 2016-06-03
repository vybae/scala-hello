
import scala.actors._
import scala.actors.Actor._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import sun.awt.image.ImageWatched.Link
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.awt.image.RasterFormatException
import java.io.FileInputStream
import java.awt.Graphics
import java.io.FileOutputStream
import com.sun.image.codec.jpeg.JPEGImageEncoder
import com.sun.image.codec.jpeg.JPEGCodec
import scala.util.matching.Regex
import java.util.Scanner
import java.util.StringTokenizer
import java.util.Date

object charpter20 {
  val cpuNum: Int = 4
  /*
   * 20.1 & 20.10
   * 1)
   *   编写一个程序, 生成由n个随机数组成的数组, (其中n是一个很大的值, 比如1000000), 然后通过将工作分发给多个actor的方式计算这些数的平均值, 
   *   每个actor计算子区间之内的值的和, 将结果发给一个能生成结果的actor.
   *   如果你在双线程或者四线程的计算机上运行此程序, 这和单线程的解决方案相比有多大差别? 快多少?
   *   caculate the avg of integer array
   * 2)
   *   重写20.1的程序, 使用消息通道来通信	
   */
  def question1(n: Int) {
    // init
    val data = 1 to n map { n => Random.nextInt(n) }

    // deal with data, get a new array to balance the workload
    val arr = ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 1 to cpuNum) arr += ArrayBuffer[Int]()
    var rowIndex = 0
    for (i <- 0 until n) {
      arr(rowIndex) append data(i)
      rowIndex = (rowIndex + 1) % cpuNum
    }
    // transport connection entity
    case class Messager(input: Iterable[Int], channel: OutputChannel[Int])

    // sum child actor
    class Sum extends Actor {
      def act(): Unit = {
        reactWithin(1000) {
          case Messager(input, channel) => {
            channel ! input.sum
          }
        }
      }
    }

    // avg actor
    val avg = actor {
      var count = 0
      var total = 0
      val chnl = new Channel[Int]
      for (i <- 0 until cpuNum) {
        new Sum().start() ! Messager(arr(i), chnl)
      }
      /*
       * 使用消息通道通信
       */
      loopWhile(count < cpuNum) {
        chnl.react {
          case x: Int => {
            total += x
            count += 1
            if (count == cpuNum) {
              println("the data array's avg is: " + 1.0 * total / n)
            }
          }
        }
      }
      /*
       * 使用Actor自带的接收方法
       */
      //      loopWhile(count < cpuNum) {
      //        react {
      //          case x: Int => {
      //            total += x
      //            count += 1
      //            if (count == cpuNum) {
      //              println("the data array's avg is: " + 1.0 * total / n)
      //            }
      //          }
      //        }
      //      }
    }
    avg.start()
  }

  /*
   * 20.2
   * 编写一段程序, 读取一个大型图片到BufferedImage对象中, 用javax.imageio.ImageIO.read方法.
   * 使用多个actor, 每个actor对图片的某一个特定条带区域进行反色处理.
   * 当所有条带反色后, 输出结果.
   * 下面的代码为了方便起见， 设定分片图像的宽度为1024的整除数
   */
  def question2(path: String) {
    // 初始设置变量
    val img = ImageIO.read(new File(path))
    val result = new BufferedImage(img.getWidth, img.getHeight, BufferedImage.TYPE_INT_RGB)
    val subWidth = 64

    // 将图片分片
    val subs = ArrayBuffer[BufferedImage]()
    var (q, r) = img.getWidth / subWidth -> img.getWidth % subWidth
    for (i <- 0 until (if (r == 0) q else q + 1)) {
      subs += img.getSubimage(i * subWidth, 0, if (r != 0 && i == q) r else subWidth, img.getHeight)
    }

    // 反转颜色
    val len = subs.size
    var rgbs = new ArrayBuffer[Array[Int]](subs.size)
    for (i <- 0 until len) {
      val b = subs(i)
      val act = actor {
        reactWithin(1000) {
          case x: Array[Int] =>
            result.setRGB(subWidth * i, 0, subWidth, img.getHeight, x map { 255 - _ }, 0, subWidth)
        }
      }
      var ar = new Array[Int](b.getWidth * b.getHeight)
      b.getRGB(0, 0, b.getWidth, b.getHeight, ar, 0, b.getWidth)
      act ! ar
    }

    ImageIO.write(result, "jpg", new File("""C:\Users\Public\Pictures\Sample Pictures\cc.jpg"""))
  }

  /*
   * 20.3 & 20.4 & 20.5 & 20.7
   * 3)
   *   编写一个程序, 对给定目录下所有子目录的所有文件中匹配某个给定的正则表达式的单次进行计数.
   *   对每一个文件各采用一个actor, 另外再加上一个actor用来遍历所有子目录, 还有一个actor将结果汇总到一起.
   * 4)
   *   修改前一个练习的程序, 显示所有匹配的单词.
   * 5)
   *   修改前一个练习的程序, 显示所有匹配的单词, 每一个都带有一个包含他的文件的列表.
   * 7)
   *   给练习3的程序添加一个监管actor, 监控读取文件的actor并记录任何因IOException推出的actor.
   *   尝试通过移除那些计划要被处理的文件的方式触发IOException.
   */
  def StatisticsWords(reg: Regex) {
    import scala.collection.mutable._
    val files = getFiles(new File("test.txt"), new File("parsing.xml"))

    val statistics = actor {
      val map = Map[String, Int]()
      var count = 0

      loopWhile(count < files.size) {
        reactWithin(1000) {
          case k: String => map(k) = map.getOrElse(k, 0) + 1
          case 0 =>
            count += 1
        }

      }
      map foreach println
    }.start()

    class SubAct extends Actor {
      def act(): Unit = {
        trapExit = true
        link(statistics)
        reactWithin(1000) {
          case f: File =>
            {
              val in = new Scanner(f)
              while (in.hasNext()) {
                val st = new StringTokenizer(in.next())
                while (st.hasMoreTokens()) {
                  statistics ! (st.nextToken() + "@@@" + f.getPath)
                }
              }
              statistics ! 0
            }
        }
      }
    }
    files foreach { new SubAct().start() ! _ }
  }
  def getFiles(files: File*): Iterable[File] = {
    val dirs = files filter { _.isDirectory() }
    if (dirs.length > 0) {
      (Array[File]() /: files) {
        (arr, fs) => arr ++ files.filter { _.isFile() } ++ getFiles(fs)
      }
    }
    else {
      files
    }
  }

  /*
   * 20.6
   * 编写一个程序, 构造100个actor, 这些actor使用while(true)/receive循环, 
   * 当接收到"Hello"消息时, 调用println(Thread.currentThread),
   * 同时构造另外100个actor, 它们做同样的事, 不过采用loop/react.
   * 将他们全部启动, 给他们全部都发送一个消息.
   * 第一种acotr占用了多少线程, 第二种actor占用了多少线程?
   */
  def question6() {

    class Act1 extends Actor {
      val map = scala.collection.mutable.Map[String, Int]()
      def act(): Unit = {
        while (true) {
          receiveWithin(500) {
            {
              case "Hello" => {
                val key = Thread.currentThread().getName
                map(key) = map.getOrElse(key, 0) + 1
              }
              case TIMEOUT => {
                println("while/receive: " + map.size)
              }
            }
          }
        }
      }
    }
    class Act2 extends Actor {
      val map = scala.collection.mutable.Map[String, Int]()
      def act(): Unit = {
        loop {
          reactWithin(500) {
            {
              case "Hello" => {
                val key = Thread.currentThread().getName
                map(key) = map.getOrElse(key, 0) + 1
              }
              case TIMEOUT => {
                println("loop/react: " + map.size)
              }
            }
          }
        }
      }
    }
    val d1 = new Date().getTime
    1 to 100 map { x => new Act1().start() ! "Hello" }
    println(new Date().getTime - d1)
    val d2 = new Date().getTime
    1 to 100 map { x => new Act2().start() ! "Hello" }
    println(new Date().getTime - d2)
  }
  /*
   * 20.8
   * 展示一个基于actor的程序是如何在发送同步消息时引发死锁的.
   */
  def question8() {
    
  }
  
  /*
   * 20.9
   * 做出一个针对练习3的程序的有问题的实现, 在这个实现当中, 
   * actor将更新一个共享计数器.
   * 你能展示出程序运行四错误的吗?
   */

  def main(args: Array[String]): Unit = {
  }
}

