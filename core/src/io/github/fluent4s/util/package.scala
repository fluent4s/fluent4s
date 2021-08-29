package io.github.fluent4s

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

import scala.collection.IterableOnceOps
import scala.reflect.ClassTag

package object util {

  implicit class ValidatedOps[E, A](value: Validated[E, A]) {

    def combineMap[EE >: E, AA >: A](that: A => Validated[EE, AA])(implicit EE: Semigroup[EE], AA: Semigroup[AA]): Validated[EE, AA] =
      value.combine(value.andThen(that))
  }

  implicit class CollectionOps[+A, +CC[_], +C](value: IterableOnceOps[A, CC, C]) {

    def filterType[B](implicit tag: ClassTag[B]): CC[B] = value
      .filter(tag.runtimeClass.isInstance)
      .asInstanceOf[CC[B]]
  }
}