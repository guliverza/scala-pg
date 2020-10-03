package codewars

object Isomorphism {
  /**
   * The type [[Nothing]] has no value.
   * So it is impossible to construct an instance of it.
   * In this solution, wherever a situation arises where
   * for types to check, you need a function that takes a [[Nothing]],
   * you can use [[absurd]].
   */
  def absurd[R](n: Nothing): R = n

  // so, when are two type, `A` and `B`, considered equal?
  // a definition might be, it is possible to go from `A` to `B`,
  // and from `B` to `A`.
  // Going a roundway trip should leave you the same value.
  // Unfortunately it is virtually impossible to test this in Scala.
  // This is called Isomorphism.

  type ISO[A, B] = (A => B, B => A)

  // given ISO a b, we can go from a to b
  def substL[A, B]: ISO[A, B] => (A => B) = _._1

  // and vice versa
  def substR[A, B]: ISO[A, B] => (B => A) = _._2

  // There can be more than one ISO a b
  def isoBool: ISO[Boolean, Boolean] = (identity, identity)

  def isoBoolNot: ISO[Boolean, Boolean] = (!_, !_)

  // isomorphism is reflexive
  def refl[A]: ISO[A, A] = (x => x, x => x)

  // isomorphism is symmetric
  //  def symm[A, B]: ISO[A, B] => ISO[B, A] = (iso: (A => B, B => A)) => (iso._2, iso._1)
  def symm[A, B]: ISO[A, B] => ISO[B, A] = {
    case (ab: (A => B), ba: (B => A)) => (ba, ab)
  }

  // isomorphism is transitive
  def trans[A, B, C]: (ISO[A, B], ISO[B, C]) => ISO[A, C] = (ab: (A => B, B => A), bc: (B => C, C => B)) => (ab._1.andThen(bc._1), bc._2.andThen(ab._2))

  //  def trans[A, B, C]: (ISO[A, B], ISO[B, C]) => ISO[A, C] = (ab: ISO[A, B], bc: ISO[B, C]) => {
  //    case ((ab: (A => B), ba: (B =>A)), (bc: (B => C), cb: (C =>B))) => (ab.andThen(bc), cb.andThen(ba))
  //  }

  // We can combine isomorphism:
  //  def isoTuple[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A, C), (B, D)] = (ab: ISO[A, B], cd: ISO[C, D]) => ((ab._1, cd._1), ab._2, cd._2)
  //  def isoTuple[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A, C), (B, D)] = (ab: ISO[A, B], cd: ISO[C, D]) => {
  //    case ((ab: (A => B), ba: (B =>A)), (cd: (C => D), dc: (D =>C))) => ((ab, cd), (ba, dc))
  //  }
  def isoTuple[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A, C), (B, D)] = {
    case ((ab, ba), (cd, dc)) =>
      (x => (ab(x._1), cd(x._2)), y => (ba(y._1), dc(y._2)))
  }

  def isoList[A, B]: ISO[A, B] => ISO[List[A], List[B]] = {
    case (ab: ISO[A, B]) =>
      (l => l.map(ab._1), l => l.map(ab._2))
  }

  def isoOption[A, B]: ISO[A, B] => ISO[Option[A], Option[B]] = {
    case (ab: ISO[A, B]) =>
      (l => l.map(ab._1), l => l.map(ab._2))
  }

  def isoEither[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[Either[A, C], Either[B, D]] = {
    case ((ab, ba), (cd, dc)) =>
      (x => x.map(cd).swap.map(ab).swap, y => y.map(dc).swap.map(ba).swap)
  }

  def isoFunc[A, B, C, D]: (ISO[A, B], ISO[C, D]) => ISO[(A => C), (B => D)] = {
    case ((ab, ba), (cd, dc)) =>
      (ac => ba.andThen(ac).andThen(cd), bd => ab.andThen(bd).andThen(dc))
  }

  // Going another way is hard (and is generally impossible)
  def isoUnOption[A, B]: ISO[Option[A], Option[B]] => ISO[A, B] = {
    case (a: (Option[A] => Option[B]), b: (Option[B] => Option[A])) => (
      (x: A) => a(Option(x)).get,
      (y: B) => b(Option(y)).get
    )
  }

  // Remember, for all valid ISO, converting and converting back
  // Is the same as the original value.
  // You need this to prove some case are impossible.

  // We cannot have
  def isoUnEither[A, B, C, D]: (ISO[Either[A, B], Either[C, D]], ISO[A, C]) => ISO[B, D] = ???

  // Note that we have
  def isoEU: ISO[Either[List[Unit], Unit], Either[List[Unit], Nothing]] =
    (e => Left(if (e.isLeft) e.left.get else List()), e => e.map(absurd))

  // where Unit, has 1 value, (the value is also called Unit), and Void has 0 values.
  // If we have isoUnEither,
  // We have ISO[Unit, Nothing] by calling isoUnEither isoEU
  // That is impossible, since we can get a Nothing by substL on ISO[Unit, Nothing]
  // So it is impossible to have isoUnEither

  // And we have isomorphism on isomorphism!
  def isoSymm[A, B]: ISO[ISO[A, B], ISO[B, A]] = (a => (a._2, a._1), b => (b._2, b._1))
}
