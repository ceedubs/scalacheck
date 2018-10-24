/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2018 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

/*
 * The code in this file is based on http://hackage.haskell.org/package/QuickCheck-2.12.6.1/docs/Test-QuickCheck-Function.html
 */

sealed abstract class :->[A, +C] {
  def withDefault[CC >: C](c: CC): A => CC

  def map[D](f: C => D): A :-> D

  def table[CC >: C]: Stream[(A, CC)]
}

object :-> {
  final case class AndThen[A, B, C](f: (A :-> (B :-> C))) extends ((A, B) :-> C) {
    override def withDefault[CC >: C](c: CC): ((A, B)) => CC = { case (a, b) =>
      val fbcc: A :-> CC = f.map(_.withDefault(c)(b))
      fbcc.withDefault(c)(a)
    }

    override def map[D](g: C => D): ((A, B)) :-> D = AndThen(f.map(_.map(g)))

    override def table[CC >: C]: Stream[((A, B), CC)] =
      for {
        (a, fbc) <- f.table
        (b, c) <- fbc.table
      } yield ((a, b), c)
  }

  final case class Or[A, B, C](fac: A :-> C, fbc: B :-> C) extends (Either[A, B] :-> C) {
    override def withDefault[CC >: C](c: CC): Either[A, B] => CC =
      _.fold(fac.withDefault(c)(_), fbc.withDefault(c)(_))

    override def map[D](f: C => D): Either[A, B] :-> D = Or(fac.map(f), fbc.map(f))

    override def table[CC >: C]: Stream[(Either[A, B], CC)] =
      fac.table[CC].map{ case (a, c) => (Left(a), c)} ++ fbc.table[CC].map{ case(b, c) => (Right(b), c)}
  }

  final case class Const[C](value: C) extends (Unit :-> C) {
    override def withDefault[CC >: C](c: CC): Unit => CC = _ => value

    def map[D](f: C => D): Unit :-> D =
      Const(f(value))

    def table[CC >: C]: Stream[(Unit, CC)] = Stream(((), value))
  }

  case object Nil extends (Unit :-> Nothing) {
    override def withDefault[CC >: Nothing](c: CC): Unit => CC = _ => c

    def map[D](g: Nothing => D): Unit :-> D = Nil

    def table[CC >: Nothing]: Stream[(Unit, CC)] = Stream.empty
  }

  final case class Table[A, C](mappings: Stream[(A, C)], eqv: (A, A) => Boolean) extends (A :-> C) {
    override def withDefault[CC >: C](c: CC): A => CC = in =>
      mappings.collectFirst{ case (a, b) if eqv(in, a) => b }
      .getOrElse(c)

    def map[D](f: C => D): A :-> D = Table(mappings.map{ case (a, b) => (a, f(b) )}, eqv)

    def table[CC >: C]: Stream[(A, CC)] = mappings
  }

  final case class Mapped[A, B, C](fab: A => B, fba: B => A, fbc: B :-> C) extends (A :-> C) {
    override def withDefault[CC >: C](c: CC): A => CC = a => fbc.withDefault(c)(fab(a))

    def map[D](f: C => D): A :-> D = Mapped(fab, fba, fbc.map(f))

    def table[CC >: C]: Stream[(A, CC)] = fbc.table[CC].map{ case (b, c) => (fba(b), c)}
  }
}
