
import scala.xml._
import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer

object charpter16 extends App {

  /*
   * 因为Node的父类NodeSeq的apply方法返回的是内部对象Seq[Node]的第一个元素
   * 而Node实现了SeqLike[Node, NodeSeq]特质， 返回的即是它自身
   */
  def question1() {
    val divs: NodeBuffer = <div id="container"><a/><a/></div><div>sdsd</div>
    Predef println divs(0)
    Predef println divs(0)(0)
  }

  def question2() {
    val xml =
      <ul>
        <li>Open: [</li>
        <li>Close: ]</li>
        <li>Open: {{</li>
        <li>Close: }}</li>
      </ul>
  }

  def question3() {

    <li>Fred</li> match { case <li>{ Text(e) }</li> => println(e) }
    /*
     *  <li>{"Fred"}</li> match {case <li>{Text(e)}</li> => println(e) }
     *  会报matchError， 因为这里的<li>{"Fred"}</li> 的子节点类型是 scala.xml.Atom
     *  而<li>Fred</li>的子节点类型才是 scala.xml.Text
     *  验证如下
     */
    <li>Fred</li> match { case <li>{ e }</li> => println(e.label + ", 类型为: " + e.getClass.getName) }
    <li>{ "Fred" }</li> match { case <li>{ e }</li> => println(e.label + ", 类型为: " + e.getClass.getName) }
  }

  def question4(xml: Node) {
    xml match {
      case e @ <img/> if (!e.attributes.exists { _.key == "alt" }) => println(e)
      case _ => println("None")
    }
  }

  def question5(xml: Node) {
    xml match {
      case e @ <img/> if (e.attributes.exists { _.key == "src" }) => println(e.attributes("src").text)
      case e @ <img>{ _ }</img> if (e.attributes.exists { _.key == "src" }) => println(e.attributes("src").text)
      case _ => println("None")
    }
  }

  def question6(xml: Node) {
    xml match {
      case e @ <a>{ t }</a> if (e.attributes.exists { _.key == "href" }) => println(t + " -> " + e.attributes("href").text)
      case _ => println("None")
    }
  }

  def question7(map: Map[String, String], container: Elem = <dl/>) = {
    container.copy(child = (container.child /: map) { (lst, m) => lst ++ <dt>{ m._1 }</dt><dd>{ m._2 }</dd> })
  }

  def question8(xml: Elem): Map[String, String] = {
    (Seq[String]() /: xml.child) {
      (seq, elem) => seq :+ elem.text
    }.grouped(2).map { x => x(0) -> x(1) }.toMap
  }

  def question9(xml: Node): Seq[Node] = {
    new RuleTransformer(new RewriteRule() {
      override def transform(n: Node) = n match {
        case e @ <img/> if (!e.attributes.exists { _.key == "alt" }) => e.asInstanceOf[Elem] % Attribute(null, "alt", "TODO", Null)
        case _ => n
      }
    }).transform(xml)
  }

  def question10(path: String): Seq[Node] = {
    question9(xml.parsing.ConstructingParser.fromFile(new java.io.File(path), true).document().docElem)
  }

  def test() {
    val items = <li>a</li><li>b</li><li>c</li><li>d</li><li>e</li>
    val str = "ss"
    val div = <div><ul><a>{ str }</a></ul></div>
    val div2 = <script><![CDATA[asd asdsd]]></script>

    div match {
      case <div><ul>{ e }</ul></div> => Predef println e
    }

    val div3 = div.copy(child = div.child ++ <img/>)
    Predef println div3
  }

  //  Predef println question7(Map("b" -> "2", "a" -> "1"))
  //  Predef println question8(<dl><dt>b</dt><dd>2</dd><dt>a</dt><dd>1</dd></dl>)
  //  Predef println question9(<div><img a="sdasd"/><img/></div>)
  Predef println question10("parsing.xml")
}