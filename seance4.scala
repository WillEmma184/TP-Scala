package seance4

import scala.util.control.NonFatal

enum MyOption[+A]:
  case MySome(value: A)
  case MyNone
  
  import MyOption.*
  
  def isDefined: Boolean = this match
    case MySome(_) => true
    case MyNone    => false
  
  def isEmpty: Boolean = !isDefined
  
  def get: A = this match
    case MySome(v) => v
    case MyNone    => throw new NoSuchElementException("MyNone.get")
  
  def getOrElse[B >: A](default: => B): B = this match
    case MySome(v) => v
    case MyNone    => default
  
  def myMap[B](f: A => B): MyOption[B] = this match
    case MySome(v) => MySome(f(v))
    case MyNone    => MyNone
  
  def myFlatMap[B](f: A => MyOption[B]): MyOption[B] = this match
    case MySome(v) => f(v)
    case MyNone    => MyNone
  
  def myFilter(p: A => Boolean): MyOption[A] = this match
    case MySome(v) if p(v) => this
    case _                 => MyNone
  
  def orElse[B >: A](alternative: => MyOption[B]): MyOption[B] = this match
    case MySome(_) => this
    case MyNone    => alternative
  
  def toList: List[A] = this match
    case MySome(v) => List(v)
    case MyNone    => Nil

end MyOption

object MyOption:
  def apply[A](a: A): MyOption[A] =
    if a == null then MyNone else MySome(a)


enum MyEither[+E, +A]:
  case MyLeft(error: E)
  case MyRight(value: A)
  
  import MyEither.*
  
  def isRight: Boolean = this match
    case MyRight(_) => true
    case MyLeft(_)  => false
  
  def isLeft: Boolean = !isRight
  
  def myMap[B](f: A => B): MyEither[E, B] = this match
    case MyRight(v) => MyRight(f(v))
    case MyLeft(e)  => MyLeft(e)
  
  def myFlatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match
    case MyRight(v) => f(v)
    case MyLeft(e)  => MyLeft(e)
  
  def getOrElse[B >: A](default: => B): B = this match
    case MyRight(v) => v
    case MyLeft(_)  => default
  
  def orElse[EE >: E, B >: A](alternative: => MyEither[EE, B]): MyEither[EE, B] = this match
    case MyRight(_) => this
    case MyLeft(_)  => alternative
  
  def toOption: MyOption[A] = this match
    case MyRight(v) => MyOption.MySome(v)
    case MyLeft(_)  => MyOption.MyNone
  
  def swap: MyEither[A, E] = this match
    case MyRight(v) => MyLeft(v)
    case MyLeft(e)  => MyRight(e)
  
  def fold[B](fa: E => B, fb: A => B): B = this match
    case MyLeft(e)  => fa(e)
    case MyRight(v) => fb(v)

end MyEither


enum MyTry[+A]:
  case MySuccess(value: A)
  case MyFailure(exception: Throwable)
  
  import MyTry.*
  
  def isSuccess: Boolean = this match
    case MySuccess(_) => true
    case MyFailure(_) => false
  
  def isFailure: Boolean = !isSuccess
  
  def get: A = this match
    case MySuccess(v) => v
    case MyFailure(e) => throw e
  
  def getOrElse[B >: A](default: => B): B = this match
    case MySuccess(v) => v
    case MyFailure(_) => default
  
  def myMap[B](f: A => B): MyTry[B] = this match
    case MySuccess(v) =>
      try MySuccess(f(v))
      catch { case NonFatal(ex) => MyFailure(ex) }
    case MyFailure(e) => MyFailure(e)
  
  def myFlatMap[B](f: A => MyTry[B]): MyTry[B] = this match
    case MySuccess(v) => 
      try f(v)
      catch { case NonFatal(ex) => MyFailure(ex) }
    case MyFailure(e) => MyFailure(e)
  
  def recover[B >: A](pf: PartialFunction[Throwable, B]): MyTry[B] = this match
    case MyFailure(e) if pf.isDefinedAt(e) => 
      try MySuccess(pf(e))
      catch { case NonFatal(ex) => MyFailure(ex) }
    case _ => this
  
  def toOption: MyOption[A] = this match
    case MySuccess(v) => MyOption.MySome(v)
    case MyFailure(_) => MyOption.MyNone
  
  def toEither: MyEither[Throwable, A] = this match
    case MySuccess(v) => MyEither.MyRight(v)
    case MyFailure(e) => MyEither.MyLeft(e)

end MyTry

object MyTry:
  def apply[A](expr: => A): MyTry[A] =
    try MySuccess(expr)
    catch { case NonFatal(ex) => MyFailure(ex) }


object Exercices:
  import MyOption.*
  import MyEither.*
  import MyTry.*
  
  def parseInt(s: String): MyOption[Int] =
    s.toIntOption match
      case Some(n) => MySome(n)
      case None    => MyNone
  
  def parseIntEither(s: String): MyEither[String, Int] =
    s.toIntOption match
      case Some(n) => MyRight(n)
      case None    => MyLeft("Invalid integer format")
  
  def divide(a: Int, b: Int): MyOption[Double] =
    if b == 0 then MyNone else MySome(a.toDouble / b)
  
  def divideEither(a: Int, b: Int): MyEither[String, Double] =
    if b == 0 then MyLeft("Division by zero") else MyRight(a.toDouble / b)
  
  def safeSqrt(n: Double): MyOption[Double] =
    if n < 0 then MyNone else MySome(Math.sqrt(n))
  
  def calculation(n: MyOption[Int], d: MyOption[Int]): MyOption[Double] =
    n.myFlatMap(num => 
      d.myFlatMap(den => 
        divide(num, den).myMap(_ + 10).myFlatMap(safeSqrt)
      )
    )
  
  def lookupAndParse(map: Map[String, String], key: String): MyOption[Int] =
    map.get(key) match
      case Some(s) => parseInt(s)
      case None    => MyNone
  
  def sequenceOption[A](list: List[MyOption[A]]): MyOption[List[A]] =
    list.foldRight(MySome(List.empty[A]): MyOption[List[A]]) { (optElem, optAcc) =>
      optElem.myFlatMap(elem => optAcc.myMap(acc => elem :: acc))
    }
  
  def sequenceEither[E, A](list: List[MyEither[E, A]]): MyEither[E, List[A]] =
    list.foldRight(MyRight(List.empty[A]): MyEither[E, List[A]]) { (eitherElem, eitherAcc) =>
      eitherElem.myFlatMap(elem => eitherAcc.myMap(acc => elem :: acc))
    }


object Bonus:
  import MyOption.*
  import MyEither.*
  
  def map2Option[A, B, C](oa: MyOption[A], ob: MyOption[B])(f: (A, B) => C): MyOption[C] =
    oa.myFlatMap(a => ob.myMap(b => f(a, b)))
  
  def map2Either[E, A, B, C](ea: MyEither[E, A], eb: MyEither[E, B])(f: (A, B) => C): MyEither[E, C] =
    ea.myFlatMap(a => eb.myMap(b => f(a, b)))
  
  def traverseOption[A, B](list: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    list.foldRight(MySome(List.empty[B]): MyOption[List[B]]) { (elem, optAcc) =>
      f(elem).myFlatMap(b => optAcc.myMap(acc => b :: acc))
    }