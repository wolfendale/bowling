package bowling

import cats.instances.all._
import bowling.OFoldable._

object GameParser {

  private type Frame = PartialFunction[(List[Int], Int), (List[Int], Int)]

  private val strike: Frame = {
    case (10 :: x1 :: x2 :: in, out) =>
      (x1 :: x2 :: in, 10 + x1 + x2 + out)
  }

  private val spare: Frame = {
    case (x1 :: x2 :: x3 :: in, out) if x1 + x2 == 10 =>
      (x3 :: in, 10 + x3 + out)
  }

  private val open: Frame = {
    case (x1 :: x2 :: in, out) =>
      (in, x1 + x2 + out)
  }

  private val last: Frame = {
    case (x1 :: x2 :: x3 :: Nil, out) if x1 + x2 == 10 =>
      (Nil, 10 + x3 + out)
    case (10 :: x2 :: x3 :: Nil, out) =>
      (Nil, 10 + x2 + x3 + out)
  }

  def parseGame(throws: Int*): Int =
    throws.toList.ofoldM[Int] {
      // We need to wrap this in a function because of the way PartialFunctions
      // with multiple parameters work in Scala.
      // i.e. `((a, b)) => c` vs `(a, b) => c`
      case (a, b) => (last orElse strike orElse spare orElse open)(a, b)
    }
}
