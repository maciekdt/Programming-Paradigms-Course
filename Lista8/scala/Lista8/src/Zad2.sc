sealed trait Graphs[+A]
case class Graph[A](value: A, nodes: List[Graph[A]]) extends Graphs[A]

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


def graphToNode[A](graph : Graph[A]): BT[A] = {
  graph match {
    case (Graph(value, Nil)) => Node(value, Empty, Empty)
    case Graph(value, head::Nil) => Node(value, graphToNode(head), Empty)
    case Graph(value, head::head2::Nil) => Node(value, graphToNode(head), graphToNode(head2))
    case Graph(value, head::tail) => Node(value, graphToNode(head), graphToNode(Graph(tail.head.value, tail.tail)))
  }
}

val g = Graph(1, List(Graph(2, List()), Graph(3, List()), Graph(4, List(Graph(5, List()), Graph(6, List())))))
println(graphToNode(g))