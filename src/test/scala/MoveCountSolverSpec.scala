import org.specs2._
import com.darkhan.chess._

class MoveCountSolverSpec extends Specification {
  def is = s2"""
   Solver for 8x8 board should
     solve 8-queens-problem          $eightQueens
     return 0 for 9-queens           $nineQueens
     return 40320 for 8-rooks        $eightRooks
     return 1 for empty pieces list  $emptyPieces

    Solver for 1x1 board should
     return 0 for any two pieces   $twoPiecesOnSingleTile
     return 1 any single piece     $singlePieceOnSingleTile

    Solver for 2x2 board should
     return 2 for two bishops           $twoBishopsOnSmallBoard
     return 2 for two rooks             $twoRooksOnSmallBoard
     return 1 for four knights          $fourKnightsOnSmallBoard
                                 """

  val defaultBoard = Board(8, 8)
  val defaultSolver = MoveCountSolver(defaultBoard)

  val singleTileBoard = Board(1, 1)
  val singleTileSolver = MoveCountSolver(singleTileBoard)

  val smallBoard = Board(2, 2)
  val smallSolver = MoveCountSolver(smallBoard)

  def eightQueens =
    defaultSolver.solve(
      List.fill(8)(Queen)
    ) must beEqualTo(92)

  def nineQueens =
    defaultSolver.solve(
      List.fill(9)(Queen)
    ) must beEqualTo(0)

  def eightRooks =
    defaultSolver.solve(
      List.fill(8)(Rook)
    ) must beEqualTo(40320)

  def emptyPieces = defaultSolver.solve(List.empty) must beEqualTo(1)

  def twoPiecesOnSingleTile = {
    val pairs = List(Rook, Queen, King, Bishop, Knight).combinations(2)
    forall(pairs)(x => singleTileSolver.solve(x) must beEqualTo(0))
  }

  def singlePieceOnSingleTile = {
    val pieces = List(Rook, Queen, King, Bishop, Knight)
    forall(pieces)(x => singleTileSolver.solve(List(x)) must beEqualTo(1))
  }

  def twoBishopsOnSmallBoard = {
    smallSolver.solve(List(Bishop, Bishop)) must beEqualTo(4)
  }

  def twoRooksOnSmallBoard = {
    smallSolver.solve(List(Rook, Rook)) must beEqualTo(2)
  }

  def fourKnightsOnSmallBoard = {
    smallSolver.solve(List(Knight, Knight, Knight, Knight)) must beEqualTo(1)
  }
}
