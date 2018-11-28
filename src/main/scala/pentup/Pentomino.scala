package pentup


import pentup.Pentomino.Board.Partial

import scala.util.Random

class Pentomino {

}

object Util {

  implicit class RandomIterableOps[T](s: Iterable[T]) {
    def random(implicit rng: Random): T = s.iterator.drop(rng.nextInt(s.size)).next
  }

}

//Map(X -> Pos(5, 5, 0), N -> Pos(3, 2, 2), T -> Pos(5, 0, 3), Y -> Pos(1, 0, 5), F -> Pos(5, 2, 7), L -> Pos(0, 4, 5), W -> Pos(0, 1, 3), Z -> Pos(2, 5, 3))

object GlobalStats {
  var looseTries: Int = 0
  val atomicTries = new java.util.concurrent.atomic.AtomicLong
//  var totalTries: Long = 0
  var fixedTries: Int = 0
  val startTime: Long = System.currentTimeMillis()
  var lastPrint: Long = startTime

//  def stats: String = s"Stats: (loose: $looseTries, fixed: $fixedTries, sneaky: ${totalTries - looseTries - fixedTries} total: $totalTries | win: ${looseTries * 1.0/totalTries}"

  def report(): Unit = {
    val totalTries = atomicTries.getAndIncrement()
    if ((totalTries & 0xFFFL) == 0L && (System.currentTimeMillis() - lastPrint > 60000)) {
      lastPrint = System.currentTimeMillis()
      val totalms = lastPrint - startTime
      println(f"Tries: $totalTries%12d  Time (m): ${totalms / 60000.0}%7.2f  Tries / ms: ${totalTries / totalms}%d")
    }

  }
}

object Pentomino {

  def main(args: Array[String]): Unit = {
    implicit val rng: Random = new Random(420)

//    val best = meander(20,2)
    val best = search(1,0)
    println(s"Best Board: (${best.score}: \n${best.board}\n${best.board.tiles}")
//    println(GlobalStats.stats)
//    for (i <- 1 to 10) {
//      val board = Board().randBoard(8)
//      println(board)
//      println("Loose Tiles: ")
//      println(board.tiles.keySet.filter(board.isLoose))
//    }
//    for {
//      t <- Tile.All
//      o <- t.orientations.indices.iterator
//      c <- t.orientations(o).willFit(board.bitMap).iterator
//      b = Board(Map(t -> Pos(c.x, c.y, o)))
//    } {
//      println(board)
//      println(b)
//    }
  }

  def search(n: Int = 60, m: Int = 4)(implicit rng: Random): Best = {

    var best = Best()
    val startTime = System.nanoTime()
    for (i <- 1 to n) {
      val board = Board.Empty.randBoard(m).optimize()
      val totalTime = (System.nanoTime() - startTime) / 1000000000.0
      if (board.score > best.score) {
        best = board
        println(s"Best Board: n: ${best.board.tiles.size} score: ${best.board.score}")
        println(best.board)
      }
      println(s"Avg time: ${totalTime / i}%04.2f   ($i/$n)")
//      println(GlobalStats.stats)
    }
//    println(bestBoard.tiles)
//    val finalTime = (System.nanoTime() - startTime) / 1000000000.0
//    println(s"Avg time: ${finalTime / n}%04.2f   ($n/$n)")
    best
  }

  def randBoardAndMaximize(n: Int = 5)(implicit rng: Random): (Board, Best, Double) = {
    val start = System.nanoTime()
    val r = Board.Empty.randBoard(n)
    val m = r.optimize()
    val t = (System.nanoTime() - start) / 1000000000.0
    (r, m, t)
  }

  def meander(n: Int = 10, m: Int = 4)(implicit rng: Random): Best = {
    var board = Board.Empty.randBoard(m)
    var bestBoard = Best(board, board.score)
    val startTime = System.nanoTime()
    println("Seed Board:\n" + board)
    for (i <- 1 to n) {
      val maxxed = board.optimize()
      val totalTime = (System.nanoTime() - startTime) / 1000000000.0
      if (maxxed.score > bestBoard.score && maxxed.board.isRigid) bestBoard = maxxed
      println(s"Maximized: ${maxxed.score}")
      println(maxxed.board)
      println(f"Avg time: ${totalTime / i}%04.2f   ($i%d/$n%d)  BestScore: ${bestBoard.score}%4.2f")
//      println(board.tiles)
      board = maxxed.board.dropHalf
      println("DropHalf:")
      println(board)
//      println(board.tiles)
      if (board.tiles.size < m) {
        board = board.randBoard(m)
        println("Grown:")
        println(board)
      }
    }
    bestBoard
  }

//  def printAllTiles(): Unit = {
//    for {
//      t <- Tile.All
//      o <- 0 until t.numOrientations
//      b = Board(Map(t -> Pos(0, 0, o)))
//    } println(s"Tile: $t ($o/${t.numOrientations})\n w: ${b.bitMap.width} h: ${b.bitMap.height}\n" + b)
//  }

  abstract class Tile(canonical: BitMap, val color: fansi.Attr) {

    var idx: Int = -1

    val orientations: Array[Orientation] =
      Transform.all8.map(b => canonical.transform(b).shiftTopLeft).to[Set].toArray.map(Orientation)

    def numOrientations: Int = orientations.length

    def toBitMap(p: Pos): BitMap = orientations(p.o).canonical.shift(p.x, p.y)

//    def allPos(taken: BitMap): Iterator[Pos] = for {
//      o <- orientations.indices.iterator
//      b = orientations(o).canonical
//      y <- 0 to (8 - b.height)
//      x <- 0 to (8 - b.width)
//      p = Pos(x, y, o)
//      if !(taken intersects this.toBitMap(p))
//    } yield p

    private val orientationIndices = orientations.indices
    def allPos(taken: BitMap): Iterator[Pos] = for {
      o <- orientationIndices.iterator
      c <- orientations(o).willFit(taken).iterator
    } yield Pos(idx, o, c)

//    val orientations: Array[Long] = Transform.all8.map(b => canonical.transform(b).shiftTopLeft.z).to[Set].toArray
//
//    def numOrientations: Int = orientations.length
//
//    def toBitMap(p: Pos): BitMap = BitMap(orientations(p.o)).shift(p.x, p.y)
//
//    def allPos: Iterator[Pos] = for {
//      o <- orientations.indices.iterator
//      b = BitMap(orientations(o))
//      x <- 0 to (8 - b.width)
//      y <- 0 to (8 - b.height)
//    } yield Pos(x, y, o)

  }

  final case class Orientation(canonical: BitMap) {

    val (c1, c2, c3, c4, c5) = {
      val it = canonical.iterator
      (it.next(), it.next(), it.next(), it.next(), it.next())
    }

    val boxMask: BitMap = BitMap.box(9-canonical.width, 9-canonical.height)

    def willFit(taken: BitMap): BitMap = BitMap{
      val z = ~taken.z
      ((z >>> c1.z) & (z >>> c2.z)) & ((z >>> c3.z) & (z >>> c4.z)) & ((z >>> c5.z) & boxMask.z)
    }

  }

  object Tile {

    import fansi.Back._

    case object F extends Tile(BitMap(0x0000000000020306L), LightRed) {
//      override val orientations: Array[Orientation] = Array(Orientation(BitMap(0x0000000000020306L)))
    }

    case object I extends Tile(BitMap(0x000000000000001FL), Magenta)

    case object L extends Tile(BitMap(0x000000000000010FL), LightGreen)

    case object N extends Tile(BitMap(0x0000000000000E03L), Red)

    case object P extends Tile(BitMap(0x0000000000010303L), LightCyan)

    case object T extends Tile(BitMap(0x0000000000020207L), Yellow)

    case object U extends Tile(BitMap(0x0000000000000705L), Blue)

    case object V extends Tile(BitMap(0x0000000000010107L), Cyan)

    case object W extends Tile(BitMap(0x0000000000010306L), LightYellow)

    case object X extends Tile(BitMap(0x0000000000020702L), LightBlue)

    case object Y extends Tile(BitMap(0x000000000000020FL), LightMagenta)

    case object Z extends Tile(BitMap(0x0000000000060203L), Green)


        val All: Array[Tile] = Array(F, I, L, N, P, T, U, V, W, X, Y, Z)
//    val All: Array[Tile] = Array(F, L, U, T, W, X, Y, Z)

    for (t <- All.indices) All(t).idx = t
  }

//  final case class PB (tiles: Long, coords: Long, bitMap: BitMap) {
//    def add(tile: Tile, orientation: Orientation, coord: Coord): PB = {
//
//    }
//
//  }


  final case class Best(board: Board = Board.Empty, score: Double = 0.0)


  trait Board {

    def size: Int
    def bitMap: BitMap
    def score: Double = bitMap.score

    def add(p: Pos): Board = Partial(p, this, p.bitMap | bitMap, size + 1)

    def canAdd(tile: Tile): Boolean = tile.allPos(bitMap).hasNext

    def additions: Map[Tile, Seq[Pos]] = (for (t <- unused) yield t -> t.allPos(bitMap).toSeq).toMap

    def canGrow: Boolean = unused.exists(canAdd)

    def grow(implicit rng: Random): Board = if (!canGrow) this else {
      import Util._
      val a = additions
      add(a(a.keySet.random).random)
    }

    def dropHalf(implicit rng: Random): Board

    def randBoard(m: Int = 5)(implicit rng: Random): Board = if (size >= m || !canGrow) this else grow.randBoard(m)

    def used: List[Tile]
    def unused: List[Tile] = Tile.All.toList.filterNot(used.contains)

    def optimize(bestSoFar: Best = Best(), allowed: List[Tile] = unused, nLeft: Int = Tile.All.length - size): Best //= {
//      var bestBoard = bestSoFar
//      if (size >= 8 || allowed.isEmpty) {
//        if (!this.hasLooseTiles(taken) && this.isRigid) {
//          val s = taken.score
//          if (s > bestBoard.score) bestBoard = Best(this, s)
//        }
//      } else {
//        for (p <- allowed.head.allPos(taken).toSeq.par)
//          bestBoard = Board(p :: tiles).maxim(bestBoard, allowed.tail, taken | p.bitMap)
//        bestBoard = maxim(bestBoard, allowed.tail, taken)
//      }
//      bestBoard
//    }
//
//    def maximizeScoreUsing(t: Tile, bestSoFar: Best, allowed: List[Tile] = unused): Best = {
//      var bestBoard = bestSoFar
//      for {
//        p <- t.allPos(bitMap)
//        //        p <- t.allPos
//        //        if canAdd(t, p)
//        board = Board(p :: tiles)
//      } {
//        bestBoard = board.maximizeScore(bestBoard, allowed)
//        //        val maxBoard = board.maximizeScore(bestBoard, allowed)
//        //        if ((maxBoard.score > bestBoard.score) && maxBoard.isRigid) bestBoard = maxBoard
//      }
//      bestBoard
//    }
//
//    def maximizeScore(bestSoFar: Best = Best(), allowed: List[Tile] = unused): Best = {
//      var bestBoard = bestSoFar
//      if (tiles.size >= 8 || allowed.isEmpty) {
//        //        GlobalStats.totalTries += 1
//        //        if (hasLooseTiles) GlobalStats.looseTries += 1
//        //        if (isRigid) GlobalStats.fixedTries += 1
//        if (!this.hasLooseTiles() && this.isRigid) {
//          val s = score
//          if (s > bestBoard.score) bestBoard = Best(this, s)
//        }
//        //        if ((this.score > bestBoard.score) && this.isRigid) bestBoard = this
//      } else {
//        bestBoard = maximizeScoreUsing(allowed.head, bestBoard, allowed.tail)
//        bestBoard = maximizeScore(bestBoard, allowed.tail)
//      }
//      bestBoard
//    }

    //    def tileMap(t: Tile): BitMap = t.toBitMap(tiles(t))

    //    def tilesMap(s: Set[Tile]): BitMap = s.foldLeft(BitMap.Empty) { case (b, t) => b | tileMap(t) }

    def tiles: List[Pos]

    private def isShiftRigid(b: BitMap, shift: BitMap => BitMap, loose: Set[Pos] = tiles.toSet): Boolean =
      if (loose.isEmpty) true else {
        //        val fixed = loose.filter(t => t.toBitMap(tiles(t)).intersects(b))
        val fixed = loose.filter(_.bitMap.intersects(b))
        if (fixed.isEmpty) false
        else isShiftRigid(shift(BitMap.union(fixed.map(_.bitMap))), shift, loose -- fixed)
      }

    final def isRigid: Boolean = {
      isShiftRigid(BitMap.lBorder, _.shiftR) &&
        isShiftRigid(BitMap.rBorder, _.shiftL) &&
        isShiftRigid(BitMap.tBorder, _.shiftD) &&
        isShiftRigid(BitMap.bBorder, _.shiftU)
    }

    def hasLooseTiles(taken: BitMap = bitMap): Boolean

    def tile(x: Int, y: Int): Option[Tile] = tiles find {p => p.bitMap.at(x,y)} map {_.tile}
    //    def tile(x: Int, y: Int): Option[Tile] = tiles find { case (t, p) => t.toBitMap(p).at(x, y) } map (_._1)

    private def cellToString(x: Int, y: Int): fansi.Str = tile(x, y).map(_.color).getOrElse(fansi.Attr.Reset)("  ")

    private def rowToString(y: Int): fansi.Str = (0 to 7).map(cellToString(_, y)).mkString("║", "", "║\n")

    //    private def cellToString(x: Int, y: Int): String = tile(x,y).map(_.toString*2).getOrElse("  ")
    //    private def rowToString(y: Int): String =  (0 to 7).map(cellToString(_,y)).mkString("║","","║\n")

    private def bottomBorder: String = "╚════════════════╝\n"

    private def topBorder: String = "╔════════════════╗\n"

    override def toString: String = (0 to 7).map(rowToString).mkString(topBorder, "", bottomBorder)

  }

  object Board {
    case object Empty extends Board {
      override def size: Int = 0

      override def bitMap: BitMap = BitMap.Empty

      override def dropHalf(implicit rng: Random): Board = this

      override def used: List[Tile] = List()

      def optimize(bestSoFar: Best = Best(), allowed: List[Tile] = unused, nLeft: Int = Tile.All.length): Best =
        if (nLeft < 8) bestSoFar else {
          val candidates = allowed.head.allPos(bitMap).toSeq.par
          val best = candidates.map(p => add(p).optimize(bestSoFar, allowed.tail, nLeft - 1)).maxBy(_.score)
          optimize(best, allowed.tail, nLeft - 1)
        }

      override def tiles: List[Pos] = List()

      override def hasLooseTiles(taken: BitMap): Boolean = false
    }

    final case class Partial(head: Pos, tail: Board, bitMap: BitMap, size: Int) extends Board {

//      def bitMap: BitMap = head.bitMap | tail.bitMap

      def dropHalf(implicit rng: Random): Board = if (rng.nextBoolean()) tail.dropHalf.add(head) else tail.dropHalf

      def used: List[Tile] = head.tile :: tail.used

      override def tiles: List[Pos] = head :: tail.tiles

      def optimize(bestSoFar: Best = Best(), allowed: List[Tile] = unused, nLeft: Int = unused.size): Best =
        if (size + nLeft < 8) bestSoFar else {
        var best = bestSoFar
        if (size >= 8) {
//          GlobalStats.totalTries += 1
          GlobalStats.report()
//          if (isRigid) println(s"! l: ${hasLooseTiles()} s: $score r: $isRigid")
//          if (!hasLooseTiles()) println(s" l: ${hasLooseTiles()} s: $score r: $isRigid")
          if (!hasLooseTiles() && (score > best.score) && isRigid) best = Best(this, score)
        } else {
          for (p <- allowed.head.allPos(bitMap))
            best = add(p).optimize(best, allowed.tail, nLeft - 1)
          best = optimize(best, allowed.tail, nLeft - 1)
        }
        best
      }
//
//      def maximizeScoreUsing(t: Tile, bestSoFar: Best, allowed: List[Tile] = unused): Best = {
//        var bestBoard = bestSoFar
//        for {
//          p <- t.allPos(bitMap)
//          //        p <- t.allPos
//          //        if canAdd(t, p)
//          board = Board(p :: tiles)
//        } {
//          bestBoard = board.maximizeScore(bestBoard, allowed)
//          //        val maxBoard = board.maximizeScore(bestBoard, allowed)
//          //        if ((maxBoard.score > bestBoard.score) && maxBoard.isRigid) bestBoard = maxBoard
//        }
//        bestBoard
//      }
//
//      def maximizeScore(bestSoFar: Best = Best(), allowed: List[Tile] = unused): Best = {
//        var bestBoard = bestSoFar
//        if (tiles.size >= 8 || allowed.isEmpty) {
//          //        GlobalStats.totalTries += 1
//          //        if (hasLooseTiles) GlobalStats.looseTries += 1
//          //        if (isRigid) GlobalStats.fixedTries += 1
//          if (!this.hasLooseTiles() && this.isRigid) {
//            val s = score
//            if (s > bestBoard.score) bestBoard = Best(this, s)
//          }
//          //        if ((this.score > bestBoard.score) && this.isRigid) bestBoard = this
//        } else {
//          bestBoard = maximizeScoreUsing(allowed.head, bestBoard, allowed.tail)
//          bestBoard = maximizeScore(bestBoard, allowed.tail)
//        }
//        bestBoard
//      }

      //    def tileMap(t: Tile): BitMap = t.toBitMap(tiles(t))

      //    def tilesMap(s: Set[Tile]): BitMap = s.foldLeft(BitMap.Empty) { case (b, t) => b | tileMap(t) }


      def hasLooseTiles(taken: BitMap = bitMap): Boolean = isLoose(head, taken) || tail.hasLooseTiles(taken)

      def isLoose(p: Pos, b: BitMap = bitMap): Boolean = {
        val fits: BitMap = p.orientation.willFit(b - p.bitMap)
        (((fits.z << 9) >>> p.c.z) & 0x20502) != 0
      }


    }
  }


//
//  final case class Board(tiles: List[Pos] = List()) extends AnyVal {
//  }

  object BitMap {
    val Empty = BitMap(0L)
    val Full = BitMap(-1L)
    val lBorder = BitMap(0x0101010101010101L)
    val tBorder = BitMap(0xFFL)
    val rBorder = BitMap(0x8080808080808080L)
    val bBorder = BitMap(0xFF00000000000000L)

    def point(c: Coord): BitMap = BitMap(1L << c.z)

    def union(bms: Iterable[BitMap]): BitMap = bms.foldLeft(Empty)(_ | _)

    def fromIterator(i: Iterator[Coord]): BitMap = i.map(point).foldLeft(Empty)(_ | _)

    def fromFunc(f: Coord => Boolean): BitMap = fromIterator(Coord.iterator.filter(f))

    def box(w: Int, h: Int): BitMap = fromFunc(c => c.x < w && c.y < h)

    final class CoordIterator(var z: Long) extends Iterator[Coord] {
      override def next(): Coord = {
        val cz = java.lang.Long.numberOfTrailingZeros(z)
        z &= ~(1L << cz)
        Coord(cz)
      }

      override def hasNext: Boolean = z != 0L
    }


  }

  final case class BitMap(z: Long) extends AnyVal{

    import BitMap._

    def at(c: Coord): Boolean = (z & (1L << c.z)) != 0L

    def at(x: Int, y: Int): Boolean = (z & (1L << (x + 8 * y))) != 0L

    def shift(x: Int, y: Int): BitMap = BitMap(z << (x + 8 * y))

    def &(that: BitMap): BitMap = BitMap(this.z & that.z)

    def |(that: BitMap): BitMap = BitMap(this.z | that.z)

    def -(that: BitMap): BitMap = BitMap(this.z & ~that.z)

    private def collapseRows: Long = {
      val a = z | (z >>> 4)
      val b = a | (a >>> 2)
      (b | (b >>> 1)) & 0x0101010101010101L
    }

    private def collapseCols: Long = {
      val a = z | (z >>> 32)
      val b = a | (a >>> 16)
      (b | (b >>> 8)) & 0xFFL
    }

    def iterator: Iterator[Coord] = new CoordIterator(z)

    def width = 64 - java.lang.Long.numberOfLeadingZeros(collapseCols)

    def height = 8 - java.lang.Long.numberOfLeadingZeros(collapseRows) / 8

    def size = java.lang.Long.bitCount(z)

    def intersects(that: BitMap): Boolean = (this & that) != BitMap.Empty

    //    def borders(that: BitMap): Boolean = expanded intersects that
    //    def expandedLeft: BitMap = BitMap(z )

    def extract(c: Coord): Long = (z >> c.z) & 1L

    //    def move(c: Coord): Long = (z >> c.z) & 1L

//    def transform(f: Coord => Coord): BitMap = (for {
//      i <- 0 until 64
//      c = Coord(i)
//      b = BitMap(extract(c) << f(c).z)
//    } yield b).foldLeft(BitMap.Empty)(_ | _)
    def transform(f: Coord => Coord): BitMap = iterator.map(c => point(f(c))).foldLeft(Empty)(_ | _)

    //    def shiftTop: BitMap = if (intersects(BitMap.tBorder)) this else BitMap(z >>> 8).shiftTop
    //    def shiftLeft: BitMap = if (intersects(BitMap.lBorder)) this else BitMap(z >>> 1).shiftLeft

    def shiftTopLeft: BitMap = {
      if (z == 0L) this
      else if (!intersects(tBorder))
        shiftU.shiftTopLeft
      else if (!intersects(lBorder))
        shiftL.shiftTopLeft
      else this
    }

    def shiftL = BitMap((z & ~lBorder.z) >>> 1)

    def shiftR = BitMap((z & ~rBorder.z) << 1)

    def shiftU = BitMap((z & ~tBorder.z) >>> 8)

    def shiftD = BitMap((z & ~bBorder.z) << 8)

    def expand: BitMap = this | shiftL | shiftR | shiftU | shiftD

    def complement: BitMap = BitMap(~z)

    def findHole: BitMap = {
      var oldHole = Empty
      var newHole = BitMap(java.lang.Long.highestOneBit(~z))
      while (oldHole != newHole) {
        oldHole = newHole
        newHole = newHole.expand & complement
      }
      oldHole
    }

    def score: Double = if (this == Full) 0.0 else {
      val hole = findHole
      Math.sqrt(hole.size.toFloat) + (this | hole).score
    }

    override def toString: String = java.lang.Long.toBinaryString(z)

    def statsString: String = s"size: $size width: ${shiftTopLeft.width} height: ${shiftTopLeft.height} score: $score"
  }

  type Transform = Coord => Coord

  object Transform {
    val Id: Transform = a => a
    val R90: Transform = a => Coord(7 - a.y, a.x)
    val R180: Transform = R90.compose(R90)
    val R270: Transform = R180.compose(R90)
    val Flip: Transform = a => Coord(7 - a.x, a.y)
    val all8: Iterable[Transform] = for {
      r <- List(Id, R90, R180, R270)
      f <- List(Id, Flip)
    } yield r compose f
  }

  object Coord {
    def apply(x: Int, y: Int): Coord = Coord(x + (y << 3))
    def iterator: Iterator[Coord] = (0 to 63).iterator.map(Coord(_))
  }

  final case class Coord(z: Int) extends AnyVal {
    def x: Int = z & 7

    def y: Int = z >> 3
  }

  //  case class Pos(x: Int, y: Int, o: Int)

  object Pos {
//    def apply(x: Int, y: Int, o: Int): Pos = Pos(Coord(x, y).z | (o << 6))
    def apply(tile: Tile, o: Int, c: Coord): Pos = Pos((tile.idx << 9) | (o << 6) | c.z)
    def apply(t: Int, o: Int, c: Coord): Pos = Pos((t << 9) | (o << 6) | c.z)
  }

  final case class Pos(z: Int) extends AnyVal {
    def c: Coord = Coord(z & 0x3F)

    def x: Int = z & 7

    def y: Int = (z >> 3) & 7

    def o: Int = (z >> 6) & 7

    def t: Int = z >> 9

    def tile: Tile = Tile.All(t)

    def orientation: Orientation = tile.orientations(o)

    def bitMap: BitMap = orientation.canonical.shift(x, y)

    override def toString: String = s"Pos($x, $y, $o)"
  }

}