
import java.awt.geom.Ellipse2D

object charpter10 extends App {

  /*
   * 10.1
   * java.awt.Rectangle类有2个很有用的方法translate和grow, 但可惜的是像java.awt.geom.Ellipse2D这样的类中没有,
   * 在scala中, 你可以解决掉这个问题, 定义一个RectangleLike的特质, 假如具体的translate和grow方法, 
   * 提供任何你需要用来实现的抽象方法, 以便你可以像如下代码这样混入该特质
   */
  trait RectangleLike {
    this: Ellipse2D.Double =>
    def translate(x: Double, y: Double) {
      this.x = x;
      this.y = y;
    }

    def grow(x: Double, y: Double) {
      this.x += x;
      this.y += y;
    }
  }

  /*
   * 10.2
   * 通过把scala.math.Ordered[Point]混入java.awt.Point的方式, 定义OrderedPoint类,
   * 按辞典方式排序, 也就是说, 如果x < x' 或者x = x'且 y < y' 则 (x,y) < (x', y')
   */
  class OrderedPoint extends java.awt.Point with Ordered[java.awt.Point] {
    def compare(that: java.awt.Point): Int = {
      if (this.x <= that.x && this.y < that.y) -1
      else if (this.x == that.x && this.y == that.y) 0
      else 1
    }
  }
  
  /*
   * 10.3
   * 查看BitSet类,将它的所有超类和特质绘制成一张图, 忽略类型参数, 然后给出该特质的线性化规格说明
   */

  /*
   * 10.4
   * 提供一个CryptoLogger类, 将日志消息以凯撒密码加密, 缺省情况下密钥为3, 不过使用者也可以重写它, 提供缺省密钥和-3作为密钥时的使用示例
   */
  trait Logger { def log(str: String, key: Int) }
  class CryptoLogger extends Logger {
    def log(str: String, key: Int = 3): Unit = {
      Predef println (for (i <- str) yield (97 + (i - 97 + key + (if (key < 0) 26 else 0)) % 26).toChar).toString
    }
  }
  
  /*
   * 10.5
   * JavaBeans规范里有一种提法叫做属性变更监听器(property change listener),
   * 这是bean用来通知其属性变更的标准方式, 
   * PropertyChangeSupport类对于任何想要支持属性变更监听器的bean而言是个便捷的超类,
   * 但可惜已有其他超类的类, 比如JComponent , 必须重新实现相应的方法, 
   * 将PropertyChangeSupport重新实现为一个特质, 然后把它混入到java.awt.Point类中
   */

  /*
   * 10.6
   * 在Java AWT类库中, 我们有一个Container类, 一个可以用于各种组件的Component子类, 举例来说,
   * Button是一个Component, 但Panel是Container, 这是一个运转中的组合模式,
   * Swaing有JComponent和JContainer,但如果你仔细看的话,你会发现一些奇怪的细节,
   * 尽管把其他组件添加到比如JButton中毫无意义, JComponent依然扩展自Container,
   * Swing的设计者们理想情况下应该会更倾向于图10-4中的设计,
   * 但在Java中那是不可能的, 请解释这是为什么?Scala中如何用特质来设计出这样的效果呢?
   */
  trait Component {}
  trait JComponent extends Component {}
  class JButton extends JComponent {}
  trait Container extends Component {}
  trait JContainer extends JComponent with Container {}
  class JPanel extends JContainer {}

  /*
   * 10.7
   * 市面上有不下数十种关于Scala特质的教程, 用的都是些 "在叫的狗"啦, "讲哲学的青蛙"啦之类的傻乎乎的示例,
   * 阅读和理解这些机巧的继承层级很乏味且对于理解问题没什么帮助, 但自己设计了一套继承层级就不同了,
   * 会很有启发, 做一个你自己的关于特质的继承层级,要求体现出叠加在一起的特质, 具体的和抽象的方法,
   * 以及具体的和抽象的字段
   */
  
  /*
   * 10.8
   * 在java.io类库中, 你可以通过BufferedInputStream修饰器来给输入流增加缓冲机制,
   * 用特质来重新实现缓冲, 简单起见, 重写read方法
   */
  trait BufferedInputStream extends java.io.BufferedInputStream {
    override def read(): Int = {
      ???
    }
  }
  
  /*
   * 10.9
   * 使用本章的日志生成器特质, 给前一个练习中的方案增加日志功能, 要求体现出缓冲的效果
   */
  
  /*
   * 实现一个IterableInputStream类, 扩展java.io.InputStream并混入Iterable[Byte]特质
   */

}