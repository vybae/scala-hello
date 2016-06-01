
import java.awt.geom.Ellipse2D

object charpter10 extends App {

  /*
   * question1
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
   * question2
   */
  class OrderedPoint extends java.awt.Point with Ordered[java.awt.Point] {
    def compare(that: java.awt.Point): Int = {
      if (this.x <= that.x && this.y < that.y) -1
      else if (this.x == that.x && this.y == that.y) 0
      else 1
    }
  }

  /*
   * question4
   */
  trait Logger { def log(str: String, key: Int) }
  class CryptoLogger extends Logger {
    def log(str: String, key: Int = 3): Unit = {
      Predef println (for (i <- str) yield (97 + (i - 97 + key + (if (key < 0) 26 else 0)) % 26).toChar).toString
    }
  }

  /*
   * question6
   */
  trait Component {}
  trait JComponent extends Component {}
  class JButton extends JComponent {}
  trait Container extends Component {}
  trait JContainer extends JComponent with Container {}
  class JPanel extends JContainer {}

  /*
   * question7
   */
  trait BufferedInputStream extends java.io.BufferedInputStream {
    override def read(): Int = {
      ???
    }
  }

}