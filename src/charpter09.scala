
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

  def question3(input: String): Unit = {
    Source.fromFile(input, "UTF-8").getLines() foreach { x => """\b\w{13,}\b""".r.findAllIn(x) foreach { d => println(d) } }
  }

  def question4(input: String): Unit = {
    val arr = new collection.mutable.ArrayBuffer[Double]()
    Source.fromFile(input, "UTF-8").getLines.map(x => """\b\d+(\.\d+)?\b""".r.findAllIn(x)) foreach (x => x foreach (arr += _.toDouble))

    Predef println "sum: " + arr.sum + ", avg: " + arr.sum / arr.length + ", min: " + arr.min
  }

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

  def question6(s: String): Unit = {
    // regex: ".+(?<!\\)"  以不以转义的引号为截止
    """".+(?<!\\)"""".r.findAllIn(s) foreach { x => println(x) }
  }

  def question7(input: String): Unit = {
    Source.fromFile(input, "UTF-8").getLines foreach (x => x.split("\\s+") foreach (y => if (!y.matches("""\d+(\.\d+)?""")) println(y)))
  }

  def question8(input: String): Unit = {
    val reg = """(?<=<img).*src\s*=\s*["'](.+)["'].*(?=/>)""".r
    reg.findAllIn(input).foreach { x => println(x.trim()) }
  }

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