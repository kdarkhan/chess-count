package com.darkhan.chess

object Main extends App {

  println("Starting application")

  val defaultBoard = Board(8, 8)
  val pieces = List(
    Queen,
    Queen,
    Queen,
    Queen,
    Queen,
    Queen,
    Queen,
    Queen
  )

  val solver = MoveCountSolver(defaultBoard)

  println("Solving 8-queens-problem")
  println(s"The answer is ${solver.solve(pieces)}")

  println("Complete")
}
