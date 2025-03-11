package advent

import cats.effect.IOApp
import cats.effect.IO

object Main extends IOApp.Simple {

  // This is your new "main"!
  def run: IO[Unit] = {
    // day 1
    //AdventPuzzles.findListDiffs("input/input1.txt").flatMap(IO.println)
    //AdventPuzzles.findSimilarity("input/input1.txt").flatMap(IO.println)
    // day 2
    //AdventPuzzles.findSafeReports("input/input2.txt").flatMap(IO.println)
    //AdventPuzzles.findSafeReportsWithSkip("input/input2.txt").flatMap(IO.println)
    //AdventPuzzles.extractMulRegex("input/input3.txt").flatMap(IO.println)
    //AdventPuzzlesOnetoFive.extractMulRegexDoDont("input/input3.txt").flatMap(IO.println)
    //AdventPuzzlesOnetoFive.getXmasCounts("input/input4.txt").flatMap(IO.println)
    AdventPuzzlesOnetoFive.getX_masCounts("input/input4.txt").flatMap(IO.println)
  }
}
