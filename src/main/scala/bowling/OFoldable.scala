package bowling

import cats.Monoid

import scala.annotation.tailrec

class OFoldable[In: Monoid](a: In) {

  def ofold[Out]
  (b: Out)
  (f: (In, Out) => (In, Out)): Out = {

    @tailrec
    def _ofold(a: In, b: Out)
    (f: (In, Out) => (In, Out)): (In, Out) = {

      val empty: In = Monoid.empty[In]

      f(a, b) match {
        case a @ (`empty`, _) => a
        case (in, out) => _ofold(in, out)(f)
      }
    }

    _ofold(a, b)(f)._2
  }

  def ofoldM[Out: Monoid]
  (f: (In, Out) => (In, Out)): Out =
    ofold(Monoid.empty[Out])(f)
}

object OFoldable {

  implicit def monoidInstance[A: Monoid](a: A): OFoldable[A] =
    new OFoldable(a)
}