
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
   * question 1 & 10
   * caculate the avg of integer array
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
   * question2
   * 这里为了方便起见， 分片图像的宽度为1024的整除数
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
   * question 3 & 4 & 5 & 7
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
   * question6
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
   * question 8
   */
  def question8() {

  }

  def main(args: Array[String]): Unit = {
  }
}

