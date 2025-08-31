import tree.*

@main def hello(): Unit =
  val tree = Tree(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14);
  val tree2 = tree + -100
  var list = List[Int]()

  tree2.foreach((value) => {
    if (value % 2 == 0) then list = (list :+ value)
  })
