package advent.iterators
import scala.io.Source

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
