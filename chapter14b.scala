// 7
sealed abstract class Tree
case class Leaf(value: Int) extends Tree
case class Node(children: Tree*) extends Tree

def leafSum(tree: Tree): Int = tree match {
  case Leaf(value) => value
  case Node(children @ _*) => children.map(leafSum).sum
}

val tree = Node(Node(Leaf(3), Leaf(8)), Leaf(2), Node(Leaf(5)))
leafSum(tree)

// 8
sealed abstract class OpTree
case class OpLeaf(value: Int) extends OpTree
case class OpNode(op: Char, children: OpTree*) extends OpTree

def eval(tree: OpTree): Int = tree match {
  case OpLeaf(value) => value
  case OpNode('+', children @ _*) => children.map(eval).sum
  case OpNode('-', children @ _*) => children.map(eval).foldLeft(0)(_ - _)
  case OpNode('*', children @ _*) => children.map(eval).reduce(_ * _)
}

val expr = OpNode('+', OpNode('*', OpLeaf(3), OpLeaf(8)), OpLeaf(2), OpNode('-', OpLeaf(5)))
eval(expr)

// 9
def sum(list: List[Option[Int]]) =
  (for (Some(value) <- list) yield value).sum

sum(List(None, Some(5), None, Some(6)))

// 10
type Fun = Double => Option[Double]
def compose(g: Fun, f: Fun): Fun = f(_) flatMap g

def f(x: Double) = if (x != 1) Some(1 / (x - 1)) else None
def g(x: Double) = if (x >= 0) Some(Math.sqrt(x)) else None
val h = compose(g, f)
h(2)
h(1)
h(0)
