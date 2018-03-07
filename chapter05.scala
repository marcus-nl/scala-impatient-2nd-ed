// chapter 5
import scala.beans.BeanProperty

// 1
class Counter(private var value: Int = 0) {
  def increment(): Unit = {
    if (value < Int.MaxValue)
      value += 1
  }
  def current = value
}
val counter = new Counter(Integer.MAX_VALUE);
counter.current
counter.increment()
counter.current

// 2
class BankAccount {
  private var _balance = 0L
  def balance = _balance
  def deposit(amount: Long): Unit = {
    if (_balance > Long.MaxValue - amount)
      throw new ArithmeticException("balance would overflow");
    _balance += amount
  }
  def withdraw(amount: Long): Unit = {
    if (_balance < Long.MinValue + amount)
      throw new ArithmeticException("balance would underflow");
    _balance -= amount
  }
}
val bankAccount = new BankAccount()
bankAccount.deposit(Long.MaxValue)
bankAccount.balance
bankAccount.withdraw(10)
bankAccount.balance
bankAccount.deposit(10)
bankAccount.balance
try { bankAccount.deposit(1) } catch { case e: Exception => println(e) }
bankAccount.balance

// 3
class Time(private val hours: Int, private val minutes: Int) {
  def before(other: Time) =
    hours < other.hours ||
    hours == other.hours && minutes < other.minutes
}
new Time(19, 31).before(new Time(21, 30))
new Time(19, 31).before(new Time(19, 30))
new Time(19, 31).before(new Time(19, 31))
new Time(19, 31).before(new Time(19, 32))

// 4
class Time2(val hours: Int, val minutes: Int) {
  private val minsSinceMidnight = hours * 60 + minutes
  def before(other: Time2) = minsSinceMidnight < other.minsSinceMidnight
}
new Time2(19, 31).before(new Time2(21, 30))
new Time2(19, 31).before(new Time2(19, 30))
new Time2(19, 31).before(new Time2(19, 31))
new Time2(19, 31).before(new Time2(19, 32))

// 5
class Student(@BeanProperty var id: Long,
              @BeanProperty var name: String) {
  override def toString = s"Student[$id:$name]"
}
val larry = new Student(1L, "Larry")
larry.setName(larry.getName() + " Laffer")
larry.id = larry.id + 1
larry

// 6
class Person(a: Int) {
  var age = Math.max(a, 0)
}
new Person(5).age
new Person(-1).age

// 7
class Person2(fullName: String) { // plain parameter since it isn't used in any method
  private val parts = fullName.split(' ')
  val firstName = parts(0)
  val lastName = parts(1)
}
val rw = new Person2("Roger Wilco")
rw.firstName
rw.lastName

// 8
class Car(val manufacturer: String,
          val modelName: String,
          val modelYear: Int,
          var licensePlate: String) {

  def this(manufacturer: String, modelName: String) {
    this(manufacturer, modelName, -1, "")
  }
  def this(manufacturer: String, modelName: String, modelYear: Int) {
    this(manufacturer, modelName, modelYear, "")
  }
  def this(manufacturer: String, modelName: String, licensePlate: String) {
    this(manufacturer, modelName, -1, licensePlate)
  }
  override def toString() = s"Car[$manufacturer, $modelName, $modelYear, $licensePlate)"
}
new Car("Honda", "Civic")
new Car("Honda", "Civic", "ZL-GJ-37")
new Car("Honda", "Civic", 2008, "ZL-GJ-37")

// 9: yeah, much shorter. not going to bother.

// 10
class Employee {
  var name = "John Q. Public"
  var salary = 0.0
  def this(initialName: String, initialSalary: Double) {
    this()
    name = initialName
    salary = initialSalary
  }
}
