package advent

import cats.effect.IOApp
import cats.effect.IO

object Main extends IOApp.Simple {

  // This is your new "main"!
  def run: IO[Unit] = {
    // day 1
    //HelloWorld.findListDiffs("input/input1.txt").flatMap(IO.println)
    //HelloWorld.findSimilarity("input/input1.txt").flatMap(IO.println)
    //HelloWorld.findSafeReports("input/input2.txt").flatMap(IO.println)
    HelloWorld.findSafeReportsWithSkip("input/input2.txt").flatMap(IO.println)
  }
}
