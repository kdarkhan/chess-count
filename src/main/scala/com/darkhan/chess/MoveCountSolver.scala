package com.darkhan.chess

import scala.annotation.tailrec

case class MoveCountSolver(board: Board) {

  def solve(pieces: List[Piece]): Int = {
    // a map from piece to count of how many times that piece appears in the original list
    // this is needed to avoid double counting when there are multiple versions of the same piece
    // such as two queens
    val pieceMap: Map[Piece, Int] =
      pieces.groupBy(identity).mapValues(_.length)
    solveHelper(Some(Tile(0, 0)), pieceMap, Set.empty, Set.empty)
  }

  private def solveHelper(currentTile: Option[Tile],
                          pieces: Map[Piece, Int],
                          tilesWithPieces: Set[Tile],
                          coveredTiles: Set[Tile]): Int = {
    if (pieces.isEmpty) 1
    else {
      currentTile match {
        case Some(tile) =>
          val withTileCovered: Int = pieces map {
            case (piece, count) =>
              val possibleMoves = piece.getMoves(tile, board)
              if (possibleMoves.intersect(tilesWithPieces).nonEmpty) 0
              else {
                val newCoveredTiles = coveredTiles union possibleMoves
                val newTilesWithPieces = tilesWithPieces + tile
                val maybeNextTile = nextTile(newCoveredTiles, tile)
                if (count > 1) solveHelper(maybeNextTile, pieces + ((piece, count - 1)), newTilesWithPieces, newCoveredTiles)
                else solveHelper(maybeNextTile, pieces - piece, newTilesWithPieces, newCoveredTiles)
              }
          } sum
          val withTileNotCovered: Int = solveHelper(nextTile(coveredTiles, tile), pieces, tilesWithPieces, coveredTiles)
          withTileCovered + withTileNotCovered
        case None => 0
      }
    }
  }

  @tailrec
  private def nextTile(coveredTiles: Set[Tile],
                       currentTile: Tile): Option[Tile] = {
    val maybeTile = {
      if (currentTile.col + 1 < board.cols) Some(currentTile.east)
      else if (currentTile.row + 1 < board.rows)
        Some(Tile(currentTile.row + 1, 0))
      else None
    }
    maybeTile match {
      case Some(tile) =>
        if (coveredTiles.contains(tile)) nextTile(coveredTiles, tile)
        else Some(tile)
      case None => None
    }
  }
}
