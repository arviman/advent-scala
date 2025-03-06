package advent

import scala.annotation.tailrec
import scala.io.Source

import cats.effect.IO

class LineIterator(fileName: String) extends Iterator[String] {
  private val source = Source.fromFile(fileName)
  private val lineIterator = source.getLines()

  override def hasNext: Boolean = lineIterator.hasNext

  override def next(): String = {
    if (!hasNext) {
      source.close()
      throw new NoSuchElementException("End of file reached")
    }
    lineIterator.next()
  }

  def close(): Unit = source.close()
}

class WordIterator(fileName: String) extends Iterator[String] {
  private val lineIter = new LineIterator(fileName)
  private var wordIter: Iterator[String] = Iterator.empty

  override def hasNext: Boolean = {
    while (!wordIter.hasNext && lineIter.hasNext) {
      val line = lineIter.next().trim
      if (line.nonEmpty) {
        wordIter = line.split("\\s+").iterator
      }
    }
    wordIter.hasNext
  }

  override def next(): String = {
    if (!hasNext) throw new NoSuchElementException("No more words")
    wordIter.next()
  }

  def close(): Unit = lineIter.close()
}
object HelloWorld {

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

}
