
import scala.xml._
import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer

object charpter16 extends App {

  /*
   * 16.1
   * <fred />(0) 得到什么? <fred />(0)(0) 呢? 为什么?
   *   <fred />, <fred />, 因为:
   *   NodeSeq.apply(0)返回的是集合形式的内部对象Seq[Node]的第一个元素
   *   所以无论调用多少次永远是返回的自身
   */
  
  def question1() {
    val divs: NodeBuffer = <div id="container"><a/><a/></div><div>sdsd</div>
    Predef println divs(0)
    Predef println divs(0)(0)
  }

  /*
   * 16.2
   * 如下代码的值是什么?
   * <ul>
   *    <li>Open bracket: [</li>
   *    <li>Close bracket: ]</li>
   *    <li>Open brace: {</li>
   *    <li>Close brace: }</li>
   * </ul>
   * 你如何修复它?
   * 编译不通过,解决如下:
   */
  def question2() {
    val xml =
      <ul>
        <li>Open: [</li>
        <li>Close: ]</li>
        <li>Open: {{</li>
        <li>Close: }}</li>
      </ul>
  }

  /*
   * 16.3
   * 比对
   *   <li>Fred</li> match { case <li>Text(t)</li> => t }
   * 和
   *   <li>{"Fred"}</li> match { case <li>Text(t)</li> => t }
   * 为什么它们的行为不同?
   */
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

  /*
   * 16.4
   * 读取一个XHTML文件并打印所有不带alt属性的img元素
   */
  def question4(xml: Node) {
    xml match {
      case e @ <img/> if (!e.attributes.exists { _.key == "alt" }) => println(e)
      case _ => println("None")
    }
  }

  /*
   * 16.5
   * 打印XHTML文件中所有图像的名称, 即, 打印所有位于img元素内的src属性值.
   */
  def question5(xml: Node) {
    xml match {
      case e @ <img/> if (e.attributes.exists { _.key == "src" }) => println(e.attributes("src").text)
      case e @ <img>{ _ }</img> if (e.attributes.exists { _.key == "src" }) => println(e.attributes("src").text)
      case _ => println("None")
    }
  }

  /*
   * 16.6
   * 读取XHTML文件并打印一个包含了文件中给出的所有超链接及其URL的表格。
   */
  def question6(xml: Node) {
    xml match {
      case e @ <a>{ t }</a> if (e.attributes.exists { _.key == "href" }) => println(t + " -> " + e.attributes("href").text)
      case _ => println("None")
    }
  }

  /*
   * 16.7
   * 编写一个函数，带一个类型为Map[String, String]的参数，返回一个dl元素，其中针对映射中每个键对应有一个dt，每个值对应有一个dd。例如：
   */
  def question7(map: Map[String, String], container: Elem = <dl/>) = {
    container.copy(child = (container.child /: map) { (lst, m) => lst ++ <dt>{ m._1 }</dt><dd>{ m._2 }</dd> })
  }

  /*
   * 16.8
   * 编写一个函数，接受dl元素，将它转成Map[String,String]。该函数应该是前一个练习中的反向处理，前提是所有dt后代都是唯一（各不相同）的。
   */
  def question8(xml: Elem): Map[String, String] = {
    (Seq[String]() /: xml.child) {
      (seq, elem) => seq :+ elem.text
    }.grouped(2).map { x => x(0) -> x(1) }.toMap
  }

  /*
   * 16.9
   * 对一个XHTML文档进行变换，对所有不带alt属性的img元素添加一个alt="TODO"属性，其余内容完全不变。
   */
  def question9(xml: Node): Seq[Node] = {
    new RuleTransformer(new RewriteRule() {
      override def transform(n: Node) = n match {
        case e @ <img/> if (!e.attributes.exists { _.key == "alt" }) => e.asInstanceOf[Elem] % Attribute(null, "alt", "TODO", Null)
        case _ => n
      }
    }).transform(xml)
  }

  /*
   * 16.10
   * 编写一个函数，读取XHTML文档，执行前一个练习中的变换，并保存结果。确保保存了DTD及所有CDATA内容。
   */
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