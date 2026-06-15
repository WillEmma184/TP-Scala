package seance3

import scala.annotation.tailrec

enum MyList[+A]:
  case MyNil
  case MyCons(head: A, tail: MyList[A])
  
  import MyList.*
  
  def isEmpty: Boolean = this match
    case MyNil => true
    case _ => false

  def length: Int = this match
    case MyNil => 0
    case MyCons(head, tail) => 1 + tail.length
  
  def headOption: Option[A] = this match
    case MyNil => None
    case MyCons(h, _) => Some(h)
  
  def lastOption: Option[A] = this match
    case MyNil => None
    case MyCons(h, MyNil) => Some(h)
    case MyCons(_, t) => t.lastOption
  
  def ++[B >: A](other: MyList[B]): MyList[B] = this match
    case MyNil => other
    case MyCons(h, t) => MyCons(h, t ++ other)
  
  def reverse: MyList[A] =
    @tailrec
    def loop(current: MyList[A], acc: MyList[A]): MyList[A] = current match
      case MyNil => acc
      case MyCons(h, t) => loop(t, MyCons(h, acc))
    loop(this, MyNil)

  def myMap[B](f: A => B): MyList[B] = this match
    case MyNil => MyNil
    case MyCons(h, t) => MyCons(f(h), t.myMap(f))
  
  def myFilter(p: A => Boolean): MyList[A] = this match
    case MyNil => MyNil
    case MyCons(h, t) =>
      if p(h) then MyCons(h, t.myFilter(p))
      else t.myFilter(p)
  
  def myFilterNot(p: A => Boolean): MyList[A] = 
    this.myFilter(x => !p(x))
  
  def myFlatMap[B](f: A => MyList[B]): MyList[B] = this match
    case MyNil => MyNil
    case MyCons(h, t) => f(h) ++ t.myFlatMap(f)
  
  def myMapViaFlatMap[B](f: A => B): MyList[B] =
    this.myFlatMap(a => MyCons(f(a), MyNil))
  
  @tailrec
  final def myFoldLeft[B](z: B)(f: (B, A) => B): B = this match
    case MyNil => z
    case MyCons(h, t) => t.myFoldLeft(f(z, h))(f)
  
  def myFoldRight[B](z: B)(f: (A, B) => B): B = this match
    case MyNil => z
    case MyCons(h, t) => f(h, t.myFoldRight(z)(f))
  
  def myForall(p: A => Boolean): Boolean = this match
    case MyNil => true
    case MyCons(h, t) => p(h) && t.myForall(p)
  
  def myExists(p: A => Boolean): Boolean = this match
    case MyNil => false
    case MyCons(h, t) => p(h) || t.myExists(p)
  
  def take(n: Int): MyList[A] = this match
    case MyCons(h, t) if n > 0 => MyCons(h, t.take(n - 1))
    case _ => MyNil
  
  @tailrec
  final def drop(n: Int): MyList[A] = this match
    case MyCons(_, t) if n > 0 => t.drop(n - 1)
    case _ => this
  
  def zipWith[B, C](other: MyList[B])(f: (A, B) => C): MyList[C] = (this, other) match
    case (MyCons(h1, t1), MyCons(h2, t2)) => MyCons(f(h1, h2), t1.zipWith(t2)(f))
    case _ => MyNil
  
  override def toString: String = 
    def loop(list: MyList[A], acc: String): String = list match
      case MyNil => acc + "Nil"
      case MyCons(h, t) => loop(t, acc + s"$h :: ")
    loop(this, "")

end MyList

object MyList:
  import MyList.*
  
  def apply[A](elements: A*): MyList[A] =
    if elements.isEmpty then MyNil
    else MyCons(elements.head, apply(elements.tail*))
  
  def doubleAll(list: MyList[Int]): MyList[Int] =
    list.myMap(_ * 2)
  
  def convertToStrings(list: MyList[Int]): MyList[String] =
    list.myMap(_.toString)
  
  def keepPositives(list: MyList[Int]): MyList[Int] =
    list.myFilter(_ > 0)
  
  def duplicateEach[A](list: MyList[A]): MyList[A] =
    list.myFlatMap(x => MyCons(x, MyCons(x, MyNil)))
  
  def expandRange(list: MyList[Int]): MyList[Int] =
    list.myFlatMap(n => range(0, n))
  
  def range(start: Int, end: Int): MyList[Int] =
    if start > end then MyNil
    else MyCons(start, range(start + 1, end))
  
  def sum(list: MyList[Int]): Int =
    list.myFoldLeft(0)(_ + _)
  
  def product(list: MyList[Int]): Int =
    list.myFoldLeft(1)(_ * _)

object Bonus:
  import MyList.*
  
  def filterViaFold[A](list: MyList[A])(p: A => Boolean): MyList[A] =
    list.myFoldRight(MyNil: MyList[A])((h, acc) => if p(h) then MyCons(h, acc) else acc)
  
  def mapViaFold[A, B](list: MyList[A])(f: A => B): MyList[B] =
    list.myFoldRight(MyNil: MyList[B])((h, acc) => MyCons(f(h), acc))
  
  def flatMapViaFold[A, B](list: MyList[A])(f: A => MyList[B]): MyList[B] =
    list.myFoldRight(MyNil: MyList[B])((h, acc) => f(h) ++ acc)