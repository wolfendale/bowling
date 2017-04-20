import bowling.GameParser
import org.scalatest.{FlatSpec, MustMatchers}

class GameSpec extends FlatSpec with MustMatchers {

  "A strike" must "score 10 + the following 2 throws" in {
    GameParser.parseGame(10, 1, 1, 1, 1) mustEqual 16
  }

  "A spare" must "score 10 + the following throw" in {
    GameParser.parseGame(9, 1, 1, 1) mustEqual 13
  }

  "An open frame" must "score the value of the two throws in the frame" in {
    GameParser.parseGame(1, 2) mustEqual 3
  }

  "A perfect game" must "score correctly" in {
    GameParser.parseGame(Seq.fill(12)(10): _*) mustEqual 300
  }

  "A game of all (9, 0) throws" must "score correctly" in {
    GameParser.parseGame(9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0, 9, 0) mustEqual 90
  }

  "A game of spares" must "score correctly" in {
    GameParser.parseGame(Seq.fill(21)(5): _*) mustEqual 150
  }
}
