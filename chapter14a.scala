// chapter 14

// 1 - skipped

// 2
def swap(pair: (Int,Int)) = pair match {
  case (a, b) => (b, a)
}
swap((1,2))

// or strictly spreaking, since swap is a method:
val swp: PartialFunction[(Int,Int),(Int,Int)] = { case (a, b) => (b, a) }
swp((1,2))

// 3
def arrswap(array: Array[Int]) = array match {
  case Array(a, b, rest @ _*) => Array(b, a) ++ rest
}
arrswap(Array(1,2,3,4))

// or in place:
def arraySwapInPlace(array: Array[Int]): Unit = array match {
  case Array(a, b, rest @ _*) => { array(0) = b; array(1) = a }
}
val arr = Array(1,2,3,4)
arraySwapInPlace(arr)
arr

// 4
abstract class Item {}
case class Article(desc: String, price: Double) extends Item {}
case class Bundle(desc: String, discount: Double, items: Item*) extends Item {}
case class Multiple(num: Int, item: Item) extends Item {}

def price(it: Item): Double = it match {
  case Article(_, p) => p
  case Bundle(_, d, its @ _*) => its.map(price _).sum - d
  case Multiple(num, item) => num * price(item)
}

price(Multiple(10, Article("Blackwell Toaster", 29.95)))

// 5
def leafSum(list: List[Any]): Int =
  list.map {
    case value: Int => value;
    case inner: List[Any] => leafSum(inner)
  }.sum

val tree = List(List(3, 8), 2, List(5))
leafSum(tree)

// or
def leafSum2(list: List[Any]): Int = list match {
  case Nil => 0
  case (value: Int)::rest => value + leafSum2(rest)
  case (inner: List[Any])::rest => leafSum2(inner) + leafSum2(rest)
}
leafSum2(tree)

// 6
sealed abstract class BinTree
case class Leaf(value: Int) extends BinTree
case class Node(left: BinTree, right: BinTree) extends BinTree

def leafSum(tree: BinTree): Int = tree match {
  case Leaf(value) => value
  case Node(left, right) => leafSum(left) + leafSum(right)
}

leafSum(Node(Leaf(3), Leaf(8)))
