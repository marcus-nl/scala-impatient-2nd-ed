import java.awt.Point
import java.beans.{PropertyChangeEvent, PropertyChangeListener}

import scala.collection.mutable.{HashMap, MultiMap, Set}

import chapter10._

// 1
trait RectangleLike {
  def getX(): Double
  def getY(): Double
  def getWidth(): Double
  def getHeight(): Double
  def setFrame(x: Double, y: Double, w: Double, h: Double): Unit
  def translate(dx: Double, dy: Double): Unit = {
    setFrame(getX() + dx, getY() + dy, getWidth(), getHeight())
  }
  def grow(dw: Double, dh: Double): Unit = {
    setFrame(getX(), getY(), getWidth() + dw, getHeight() + dh)
  }
  override def toString()
    = s"[${getX()}, ${getY()}, ${getWidth()}, ${getHeight()}]"
}

val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike
egg.translate(10, -10)
egg
egg.grow(10, 20)
egg

// 2
class OrderedPoint extends Point with scala.math.Ordered[Point] {
  override def compare(that: Point): Int = {
    if (this.x == that.x)
      this.y.compare(that.y)
    else
      this.x.compare(that.x)
  }
}

// 3
// BitSet >> BitSetLike >> SortedSetLike >> Sorted >> SetLike >> Set >> IterableLike
// >> GenIterableLike >> Parallelizable >> GenTraversableLike >> GenTraversableOnce
// >> FIlterMonadic >> HasNewBuilder >> TraversableLike >> Equals >>
// >> SortedSet >> Set >> GenSetTemplate >> GenericTraversableTemplate
// >> GenSet >> GenIterable >> GenTraversable >> GenSetLike >> A=>Boolean
// >> Iterable >> Traversable

// 4
class ConcreteLogger extends ConsoleLogger {}
def logger1 = new ConcreteLogger
def logger2 = new ConcreteLogger with CryptoLogger
def logger3 = new ConcreteLogger with CryptoLogger { override val key = -3 }
logger1.log("Hello World!")
logger2.log("Hello World!")
logger3.log("Khoor#Zruog$")

// 5
trait PropertyChangeTemplate {
  private val KEY_ALL = "%ALL%"
  private val map = new HashMap[String,Set[PropertyChangeListener]] with MultiMap[String,PropertyChangeListener]

  def addPropertyChangeListener(listener: PropertyChangeListener): Unit = {
    map.addBinding(KEY_ALL, listener)
  }

  def addPropertyChangeListener(propertyName: String, listener: PropertyChangeListener): Unit = {
    map.addBinding(propertyName, listener)
  }

  def firePropertyChange(propertyName: String, oldValue: Any, newValue: Any): Unit = {
    if (oldValue != newValue)
      firePropertyChange(new PropertyChangeEvent(this, propertyName, oldValue, newValue))
  }

  def firePropertyChange(event: PropertyChangeEvent): Unit = {
    val key = event.getPropertyName
    map.get(key) match {
      case None =>
      case Some(listeners) => listeners.foreach(_.propertyChange(event))
    }
  }
}
val observablePoint = new Point(3,4) with PropertyChangeTemplate {
  override def setLocation(x: Int, y: Int): Unit = {
    val oldX = this.x
    val oldY = this.y
    super.setLocation(x, y)
    firePropertyChange("x", oldX, this.x)
    firePropertyChange("y", oldY, this.y)
  }
}
observablePoint.addPropertyChangeListener("x", new PropertyChangeListener {
  override def propertyChange(evt: PropertyChangeEvent) = println(evt)
})
observablePoint.setLocation(4, 5)

// 6
// by making a trait for composite components, i.e. ContainerLike

// 7
// meh. this applies when a trait's changes are reflected in classes with the trait
// e.g. when a field is changed

// 8
// meh

// 9: something like this

// 10: see 9

// 11
abstract class IterableInputStream extends java.io.InputStream with Iterable[Byte] {
  override def iterator: Iterator[Byte] = new Iterator[Byte] {
    override def hasNext = available() > 0
    override def next() = read().toByte
  }
}
