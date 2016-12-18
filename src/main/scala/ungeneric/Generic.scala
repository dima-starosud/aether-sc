package ungeneric

import scala.language.higherKinds

trait GenericProp {
  type Bot
  type Top >: Bot
  type C[_ >: Bot <: Top]
}

trait UnboundProp extends GenericProp {
  type Bot = Nothing
  type Top = Any
}

sealed trait Generic[P <: GenericProp] extends Any {
  def apply[T >: P#Bot <: P#Top]: P#C[T]
}

object Generic {
  def apply[P <: GenericProp]: GenericBuilder[P] = new GenericBuilder[P]
}

object GenericBuilder {

  private final class GenericInstance[P <: GenericProp](val value: P#C[_]) extends AnyVal with Generic[P] {
    override def apply[T >: P#Bot <: P#Top]: P#C[T] = value.asInstanceOf[P#C[T]]
  }

}

final class GenericBuilder[P <: GenericProp] {
  type Any >: P#Bot <: P#Top
  type Type = P#C[Any]

  def apply(value: Type): Generic[P] = new GenericBuilder.GenericInstance[P](value)
}
