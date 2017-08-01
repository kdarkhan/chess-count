package com.darkhan

import scala.annotation.tailrec

package object chess {

  /**
    * Represents a tile from the board
    * Coordinates (0,0) represent top-left corner ob the board
    *
    * @param row
    * @param col
    */
  case class Tile(row: Int, col: Int) {
    def west: Tile = Tile(row, col - 1)

    def east: Tile = Tile(row, col + 1)

    def north: Tile = Tile(row - 1, col)

    def south: Tile = Tile(row + 1, col)
  }

  /**
    * Represents the board
    *
    * @param rows
    * @param cols
    */
  case class Board(rows: Int, cols: Int) {
    def isValid(tile: Tile): Boolean = {
      tile.row >= 0 && tile.row < rows && tile.col >= 0 && tile.col < cols
    }

    def toWest(from: Tile): List[Tile] =
      repeatInDirection(from, _.west, Nil)

    def toEast(from: Tile): List[Tile] =
      repeatInDirection(from, _.east, Nil)

    def toNorth(from: Tile): List[Tile] =
      repeatInDirection(from, _.north, Nil)

    def toSouth(from: Tile): List[Tile] =
      repeatInDirection(from, _.south, Nil)

    def toNorthEast(from: Tile): List[Tile] =
      repeatInDirection(from, _.north.east, Nil)

    def toNorthWest(from: Tile): List[Tile] =
      repeatInDirection(from, _.north.west, Nil)

    def toSouthEast(from: Tile): List[Tile] =
      repeatInDirection(from, _.south.east, Nil)

    def toSouthWest(from: Tile): List[Tile] =
      repeatInDirection(from, _.south.west, Nil)

    /**
      * Helper method for generating list of tiles in a single direction
      * such as UP, DOWN, DIAGONAL UP-RIGHT, etc
      *
      * @param curTile
      * @param direction
      * @param acc
      * @return
      */
    @tailrec
    private def repeatInDirection(curTile: Tile,
                                  direction: Tile => Tile,
                                  acc: List[Tile]): List[Tile] = {
      val nextTile = direction(curTile)
      if (isValid(nextTile))
        repeatInDirection(nextTile, direction, nextTile :: acc)
      else acc
    }
  }

  trait Piece {
    def getMoves(from: Tile, board: Board): Set[Tile]
  }

  object Queen extends Piece {
    def getMoves(from: Tile, board: Board): Set[Tile] = {
      board.toNorth(from).toSet ++
        board.toSouth(from).toSet ++
        board.toEast(from).toSet ++
        board.toWest(from).toSet ++
        board.toNorthEast(from).toSet ++
        board.toNorthWest(from).toSet ++
        board.toSouthEast(from).toSet ++
        board.toSouthWest(from).toSet
    }
  }

  object Bishop extends Piece {
    def getMoves(from: Tile, board: Board): Set[Tile] = {
      board.toNorthEast(from).toSet ++
        board.toNorthWest(from).toSet ++
        board.toSouthEast(from).toSet ++
        board.toSouthWest(from).toSet
    }
  }
  object Rook extends Piece {
    def getMoves(from: Tile, board: Board): Set[Tile] = {
      board.toNorth(from).toSet ++
        board.toSouth(from).toSet ++
        board.toEast(from).toSet ++
        board.toWest(from).toSet
    }
  }

  object King extends Piece {
    def getMoves(from: Tile, board: Board): Set[Tile] = {
      Set(
        from.north,
        from.south,
        from.east,
        from.west,
        from.north.east,
        from.north.west,
        from.south.east,
        from.south.west
      ) filter board.isValid
    }
  }

  object Knight extends Piece {
    def getMoves(from: Tile, board: Board): Set[Tile] = {
      List[(Int, Int)](
        (1, 2),
        (1, -2),
        (-1, 2),
        (-1, -2),
        (2, 1),
        (2, -1),
        (-2, 1),
        (-2, -1)
      ) map {
        case (first, second) =>
          Tile(from.row + first, from.col + second)
      } filter board.isValid toSet
    }
  }
}
