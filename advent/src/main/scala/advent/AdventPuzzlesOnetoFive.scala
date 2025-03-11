package advent

import scala.annotation.tailrec
import scala.io.Source
import cats.effect.IO

import scala.util.matching.Regex
import advent.iterators.*

object AdventPuzzlesOnetoFive {

  def say(): IO[String] = IO.delay("Hello Cats!")
  private def getDiff(a : List[Int], b: List[Int]): Long =  a.sorted.zip(b.sorted).map((ai,bi) => math.abs(ai-bi)).sum.toLong
  private def getSimilarity(a: List[Int], b: List[Int]): Long = a.map(a1 => a1*b.count(b1 => b1 == a1).toLong).sum

  //public
  def getColumnLists(path: String): (List[Int], List[Int]) = {
    val iterator = new WordIterator(path)
    var leftList = List[Int]()
    var rightList = List[Int]()
    while(iterator.hasNext) {
      val anum = iterator.next().toInt
      val bnum = iterator.next().toInt
      leftList = leftList :+ anum
      rightList = rightList :+ bnum
    }
    (leftList, rightList)
  }
  def findListDiffs(path: String): IO[Long] = {
    val (leftList, rightList) = getColumnLists(path)
    return IO.pure(getDiff(leftList, rightList))
  }
  def findSimilarity(path: String): IO[Long] = {
    val (leftList, rightList) = getColumnLists(path)
    return IO.pure(getSimilarity(leftList, rightList))
  }
  def findSafeReports(path: String): IO[Int] = {
    val lineIter = new LineIterator(path)
    val records = lineIter.map(line => Report(line.split("\\s+").map(_.toInt).toList))
    val safeties = records.map(_.isSafe())
    return IO.pure(safeties.count(x=>x))
  }
  case class Report(val data: List[Int]) {
    def isSafe(): Boolean = {
      @tailrec
      def innerRec(a: List[Int], isAsc: Option[Boolean]): Boolean = {
        a match {
          case _ :: Nil => true
          case head :: tail =>
            val newIsAsc = isAsc.orElse(Some(tail.head >= head))
            val diff = tail.head - head
            val good = if (newIsAsc.get) diff >= 1 && diff <= 3 else diff <= -1 && diff >= -3
            if (good) innerRec(tail, newIsAsc) else {
              false
            }
          case Nil => true
        }
      }
      innerRec(data, None)
    }
    def isSafeWithSkip(): Boolean = {
      data.indices.exists(i=>Report(data.patch(i, Seq.empty, 1)).isSafe())
    }
  }
  def findSafeReportsWithSkip(path: String): IO[Int] = {
    val lineIter = new LineIterator(path)
    val reports = for line <- lineIter
      yield Report(line.split("\\s+").map(_.toInt).toList)
    val safeties = reports.map(x=> (x.isSafe() || x.isSafeWithSkip()))
    return IO.pure(safeties.count(x=>x))
  }

  def extractMulRegex(path: String): IO[Int] = {

    def extractMulExpressions(input: String): List[(Int, Int)] = {
      val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r

      pattern.findAllMatchIn(input).map { m =>
        (m.group(1).toInt, m.group(2).toInt)
      }.toList
    }
    val lineIter = new LineIterator(path)
    val str = lineIter.mkString
    val pairs = extractMulExpressions(str)
    val sum = pairs.map(x => x._1*x._2).sum

    return IO.pure(sum)
  }

  def extractMulRegexDoDont(path: String): IO[Long] = {
    val lineIter = new LineIterator(path)
    val input = lineIter.mkString
    val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don\'t\(\)""".r

    val (_, sum) = pattern.findAllMatchIn(input).foldLeft((true, 0L)) {
      case ((_, sum), m ) if m.matched == "do()" => (true, sum)
      case ((_, sum), m) if m.matched == "don't()" => (false, sum)
      case ((true, sum), pattern(x, y)) if x != null && y != null => (true, sum + (x.toLong*y.toLong))
      case (state, _) => state
    }

    return IO.pure(sum)
  }

  val directions = List(
    (0, 1),  // Horizontal (right)
    (0, -1), // Horizontal (left)
    (1, 0),  // Vertical (down)
    (-1, 0), // Vertical (up)
    (1, 1),  // Diagonal (down-right)
    (-1, -1), // Diagonal (up-left)
    (1, -1), // Diagonal (down-left)
    (-1, 1)   // Diagonal (up-right)
  )

  def getXmasCounts(path: String): IO[Int] = {
    val lineIterator = new LineIterator(path)
    val lines: List[String] = lineIterator.toList
    val grid: List[List[Char]] = lines.map(x => x.toList)
    val n = lines.size
    val m = lines.head.size
    var vis: Array[Array[Boolean]] = Array.ofDim[Boolean](n, m)
    val xmas = "XMAS"
    var sum = 0
    def travel(x: Int, y: Int, pos: Int, dir: Int): Boolean = {
      if(x < 0 || x >= m || y < 0 || y >= n || pos > 3)
        return false
      if(grid(y)(x) == xmas(pos) && (pos == 3 || travel(x+directions(dir)(1), y+directions(dir)(0), pos+1, dir))) {
        vis(y)(x) = true
        return true
      }
      return false
    }
    for (i <- 0 until n;j <- 0 until m; dir <- 0 until directions.size) {
      if(travel(j, i, 0, dir)) {
          sum=sum+1
      }
    }
    return IO.pure(sum)
  }

  def getX_masCounts(path: String): IO[Int] = {
    val lineIterator = new LineIterator(path)
    val lines: List[String] = lineIterator.toList
    val grid: List[List[Char]] = lines.map(x => x.toList)
    val n = lines.size
    val m = lines.head.size
    var sum = 0
    def travel(x: Int, y: Int): Boolean = {
      val a = (travelDiag(x+directions(4)(1), y+directions(4)(0))) //down-right
      val b = (travelDiag(x+directions(5)(1), y+directions(5)(0))) //up-left

      val c = (travelDiag(x+directions(6)(1), y+directions(6)(0))) //down-left
      val d = (travelDiag(x+directions(7)(1), y+directions(7)(0))) //up-right

      (a,b,c,d) match {
        case (Right(a1), Right(b1), Right(c1), Right(d1)) if (a1 * b1 == -1) && (c1 * d1 == -1) =>
          true
        case _ =>
          false
      }
    }
    def travelDiag(x: Int, y: Int): Either[Exception, Int] = {
      if(x < 0 || x >= m || y < 0 || y >= n) {
        return Left(new IndexOutOfBoundsException())
      } else if(grid(y)(x) == 'M') {
        return Right(-1)
      } else if (grid(y)(x) == 'S') {
        return Right(1)
      } else {
        return Right(0)
      }
    }
    for (i <- 0 until n;j <- 0 until m) {
      if(grid(i)(j) == 'A' && travel(j, i)) {
        sum=sum+1
      }
    }
    return IO.pure(sum)
  }
}
