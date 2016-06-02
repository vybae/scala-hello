
import scala.io.Source
import java.util.Scanner
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.BufferedWriter
import java.io.ObjectOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.PrintWriter

object charpter09 {
  /*
   * 9.1
   * 编写一小段Scala代码, 将某个文件中的行倒转顺序(将最后一行作为第一行,以此类推)
   */
  def question1(input: String, output: String): Unit = {
    val src = Source.fromFile(input, "UTF-8")
    val lines = src.getLines()
    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(output)))
    try {
      (lines.toArray reverse) foreach { x =>
        {
          println(x)
          bw.write(x + "\r\n")
          bw.flush()
        }
      }
    }
    finally {
      src.close()
      bw.close()
    }
  }

  /*
   * 9.2
   * 编写Scala程序,从一个带有制表符的文件读取内容,将每个制表符替换成一组空格,使得制表符隔开的n列仍然保持纵向对齐,
   * 并将结果写入同一个文件
   */
  def question2(input: String): Unit = {
    val src = Source.fromFile(input, "UTF-8")
    val lines = src.getLines().toArray
    src.close()
    val maxw = lines.map { _.split("\\t").length }.max
    val lens = new Array[Int](maxw)
    0 until maxw foreach { i => lens(i) = lines.map { x => if (i < x.split("\\t").length) x.split("\\t")(i).length else 0 }.max }
    val bw = new PrintWriter(input)
    try {
      (lines.toArray reverse) foreach { x =>
        {
          val arr = x.split("\\t")
          var res = ""
          for (i <- 0 until arr.length) {
            res += arr(i) + " " * (lens(i) - arr(i).length + 1)
          }
          bw.write(res + "\r\n")
          bw.flush()
        }
      }
    }
    finally {
      bw.close()
    }
  }

  /*
   * 9.3
   * 编写一小段Scala代码, 从一个文件读取内容并把所有字符数大于12的单词打印到控制台,
   * 如果你能用单行代码完成会有额外奖励 
   */
  def question3(input: String): Unit = {
    Source.fromFile(input, "UTF-8").getLines() foreach { x => """\b\w{13,}\b""".r.findAllIn(x) foreach { d => println(d) } }
  }

  /*
   * 9.4
   * 编写Scala程序, 从包含浮点数的文本文件读取内容, 打印出文件中所有浮点数之和、平均数、最大值、最小值
   */
  def question4(input: String): Unit = {
    val arr = new collection.mutable.ArrayBuffer[Double]()
    Source.fromFile(input, "UTF-8").getLines.map(x => """\b\d+(\.\d+)?\b""".r.findAllIn(x)) foreach (x => x foreach (arr += _.toDouble))

    Predef println "sum: " + arr.sum + ", avg: " + arr.sum / arr.length + ", min: " + arr.min
  }

  /*
   * 9.5
   * 编写Scala程序,向文件中写入2的n次方及其倒数,指数n从0到20,对齐各列:
   *    1    1
   *    2    0.5
   *    4    0.25
   */
  def question5(output: String, n: Int): Unit = {
    val bw = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(output)))
    try {
      if (n >= 0) {
        0 to n foreach (x => {
          val r = (BigInt(2) pow x).toString
          bw.write(r + " " * (3 - r.length()) + 1.0 / r.toInt + "\r\n")
        })
      }
    }
    finally {
      bw.close()
    }
  }

  /*
   * 9.6
   * 编写正则表达式, 匹配Java或C++程序代码中类似"like this, maybe with \" or \\"这样的带引号的字符串,
   * 编写Scala程序将某个源文件中所有类似的字符串打印出来
   */
  def question6(s: String): Unit = {
    // regex: ".+(?<!\\)"  以不以转义的引号为截止
    """".+(?<!\\)"""".r.findAllIn(s) foreach { x => println(x) }
  }

  /*
   * 9.7
   * 编写Scala程序,从文本文件读取内容,并打印出所有非浮点数的词法单元, 要求使用正则表达式
   */
  def question7(input: String): Unit = {
    Source.fromFile(input, "UTF-8").getLines foreach (x => x.split("\\s+") foreach (y => if (!y.matches("""\d+(\.\d+)?""")) println(y)))
  }

  /*
   * 9.8
   * 编写Scala程序, 打印出某个网页中所有img标签的src属性, 使用正则表达式和分组
   */
  def question8(input: String): Unit = {
    val reg = """(?<=<img).*src\s*=\s*["'](.+)["'].*(?=/>)""".r
    reg.findAllIn(input).foreach { x => println(x.trim()) }
  }

  /*
   * 9.9
   * 盘点给定目录及其子目录中总共有多少以class为扩展名的文件
   */
  def question9(files: File*): Iterable[File] = {
    //val classFiles = files filter { x => x.isFile() && x.getName.endsWith(".class") }
    val dirs = files.filter { _.isDirectory }
    if (dirs.length > 0) {
      //classFiles ++ question9(dirs.flatMap { _.listFiles().filter { x => x.isDirectory() || x.isFile() && x.getName.endsWith(".class") } }: _*)
      (Iterable[File]() /: dirs) {
        (lst, f) => lst ++ files.filter { x => x.isFile() && x.getName.endsWith(".class") } ++ question9(f.listFiles(): _*)
      }
    }
    else {
      files filter { x => x.isFile() && x.getName.endsWith(".class") }
    }
  }

  /*
   * 9.10
   * 扩展哪个可序列化的Person类, 让它能以一个集合保存某个人的朋友信息, 
   * 构造出一些Person对象, 让他们中的一些人成为朋友, 然后将Array[Person]保存到文件,
   * 将这个数组从文件中重新读出来, 校验朋友关系是否完好
   */
  def question10(): Unit = {
    val p = new Person("Tom")
    p.addFriend(new Person("Jack"))

    val os = new ObjectOutputStream(new FileOutputStream("result.txt"))
    os.writeObject(p)
    os.close()
    val oi = new ObjectInputStream(new FileInputStream("result.txt"))
    var sp = oi.readObject.asInstanceOf[Person]
    os.close()

    Predef println sp
  }
  class Person(val name: String) extends Serializable {
    var friends = new collection.mutable.ArrayBuffer[Person]()

    def addFriend(p: Person*) {
      friends.appendAll(p)
    }

    override def toString(): String = {
      var res = "My name is: " + name + ", my friends are: "
      friends foreach { res += _.name + "," }
      res.dropRight(1)
    }
  }

  def main(args: Array[String]): Unit = {
    //question6("""  "adfasdf\"\""  """)
    val lst = question9(new File("""bin""")) 
    lst foreach { x => println(x.getName) }
    println(lst.size)
  }
}