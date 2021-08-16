package net.opengrabeso

import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.ClassTag

package object earcut4s {
  /**
   * Helper functions and classes to make porting JS code easier */

  implicit def refAsBoolean[X <: AnyRef](x: X): Boolean = x != null
  implicit def intAsBoolean(x: Int): Boolean = x != 0
  implicit def doubleAsBoolean(x: Double): Boolean = x != 0

  def Infinity: Double = Double.PositiveInfinity

  object JSArray {
    def empty[T]: JSArray[T] = new JSArray[T]
    def apply[T](): JSArray[T] = empty[T]

    implicit def toArray[T: ClassTag](a: JSArray[T]): Array[T] = {
      a.data.toArray
    }
  }

  class JSArray[T](initialSize: Int = mutable.ArrayBuffer.DefaultInitialSize) {
    // not very efficient (everything stored as AnyRef), but simple and working
    val data = new mutable.ArrayBuffer[T](initialSize)

    def this(elems: Seq[T]) = {
      this(elems.size)
      data.addAll(elems)
    }

    def push(x: T): this.type = {
      data.addOne(x)
      this
    }

    def apply(i: Int): T = data(i)

    def sort(compareT: (T, T) => Double): Unit = {
      object ordering extends Ordering[T] {
        override def compare(x: T, y: T) = {
          val d = compareT(x, y)
          if (d < 0) -1
          else if (d > 0) +1
          else 0
        }
      }
      data.sortInPlace()(ordering)
    }

    def foreach[U](f: T => U): Unit = {
      data.foreach(f)
    }
  }
}
