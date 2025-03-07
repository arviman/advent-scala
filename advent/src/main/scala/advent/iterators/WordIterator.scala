package advent.iterators

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