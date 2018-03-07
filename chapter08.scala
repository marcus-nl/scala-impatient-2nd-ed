// chapter 8

// 1
class BankAccount(initialBalance: Double) {
  private var balance = initialBalance
  def currentBalance = balance
  def deposit(amount: Double) = { this.balance += amount; balance }
  def withdraw(amount: Double) = { this.balance -= amount; balance }
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  override def deposit(amount: Double) = super.deposit(amount - 1)
  override def withdraw(amount: Double) = super.withdraw(amount + 1)
}

val bankAccount = new BankAccount(1000)
bankAccount.deposit(10)
bankAccount.withdraw(50)
bankAccount.deposit(10)
bankAccount.withdraw(50)

val checkingAccount = new CheckingAccount(1000)
checkingAccount.deposit(10)
checkingAccount.withdraw(50)
checkingAccount.deposit(10)
checkingAccount.withdraw(50)

// 2
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  private var transactions: Int = 0
  def earnMonthlyInterest(interestFactor: Double): Unit = {
    transactions = 0
    super.deposit(currentBalance * interestFactor)
  }
  override def deposit(amount: Double) = {
    val cost = if (transactions < 3) 0 else 1
    super.deposit(amount - cost)
    transactions += 1
    currentBalance
  }
  override def withdraw(amount: Double) = {
    val cost = if (transactions < 3) 0 else 1
    super.withdraw(amount + cost)
    transactions += 1
    currentBalance
  }
}

val savingsAccount = new SavingsAccount(1000)
savingsAccount.deposit(10)
savingsAccount.withdraw(50)
savingsAccount.deposit(10)
savingsAccount.withdraw(50)

// 3 - skipped

// 4
abstract class Item {
  def price: Double
  def description: String
}

class SimpleItem(override val price: Double,
                 override val description: String) extends Item {
}

val item: Item = new SimpleItem(10, "Foo")
item.price
item.description

class Bundle extends Item {
  val items = scala.collection.mutable.ArrayBuffer[Item]()
  override def price = items.map(_.price).sum
  override def description = s"Bundle of ${items.size} items" // or: items.map(_.description).mkString(", ")
  def add(item: Item) = items += item
}

val bundle = new Bundle()
bundle.add(item)
bundle.add(new SimpleItem(11, "Another item"))
bundle.price
bundle.description

// 5
class Point(val x: Double, val y: Double) {}
val p1 = new Point(1929, 230.07)
p1.x
p1.y

class LabeledPoint(val label: String, x: Double, y: Double) extends Point(x, y) {}
val p2 = new LabeledPoint("Black Thursday", 1929, 230.07)
p2.label
p2.x
p2.y

// 6
abstract class Shape {
  def centerPoint: Point
}
class Rectangle(val topLeft: Point,
                val width: Double, val height: Double // or bottomRicht: Point
               ) extends Shape {
  def centerPoint = new Point(topLeft.x + width / 2.0, topLeft.y + height / 2.0)
}
class Circle(override val centerPoint: Point, val radius: Double) extends Shape {}

// 7
class Square(x: Int, y: Int, size: Int) extends java.awt.Rectangle(x, y, size, size) {
  def this(size: Int) = this(0, 0, size)
  def this() = this(0, 0, 0)
  override def toString() = s"Square[x=$x, y=$y, size=$size]"
}

new Square(1, 2, 3)
new Square(4)
new Square()

// 8
class Person(val name: String) {
  override def toString = s"${getClass.getName}[name=$name]"
}

class SecretAgent(codeName: String) extends Person(codeName) {
  override val name = "secret"
  override val toString = "secret"
}
// Person has 1 field and 1 getter
// SecretAgent has an additional 1 field and 1 getter

// 9
class Creature {
  def range: Int = 10
  val env: Array[Int] = new Array[Int](range)
  override def toString = s"${getClass.getName}[${env.size}]"
}
class Ant extends Creature {
  override def range = 2
}
val ant = new Ant

// 10
// 1st protected: applies to the constructor, so visible to subclasses
// 2nd protected: applies to the val elems, so visible to subclasses

// 11
class Pnt(private val packed: Long) extends AnyVal {
  def x = (packed >> 32).toInt
  def y = packed.toInt
  override def toString = s"Pnt[packed=$packed,x=$x,y=$y]"
}
object Pnt {
  def apply(x: Int, y: Int) = new Pnt((x.toLong << 32) | y)
}
val pnt1 = Pnt(10, 20)


