package seance5

import scala.util.{Try, Success, Failure}

object Part1:
  
  def doubleOption(opt: Option[Int]): Option[Int] =
    opt.map(x => x * 2)
  
  def intToString(opt: Option[Int]): Option[String] =
    opt.map(x => x.toString)
  
  def parseString(opt: Option[String]): Option[Int] =
    opt.flatMap(x => x.toIntOption)
  
  def addTwoOptions(a: Option[Int], b: Option[Int]): Option[Int] =
    a.flatMap(x => b.map(y => x + y))
  
  def multiplyThreeOptions(a: Option[Int], b: Option[Int], c: Option[Int]): Option[Int] =
    a.flatMap(x => b.flatMap(y => c.map(z => x * y * z)))
  
  def lookupUser(
    aliases: Map[String, String],
    users: Map[String, String],
    alias: String
  ): Option[String] =
    aliases.get(alias).flatMap(x => users.get(x))
  
  def doubleOrZero(opt: Option[Int]): Int =
    opt.map(x => x * 2).getOrElse(0)
  
  def getConfig(
    primary: Option[String],
    fallback: Option[String],
    default: String
  ): String =
    primary.orElse(fallback).getOrElse(default)
  
  def positiveOnly(opt: Option[Int]): Option[Int] =
    opt.filter(x => x > 0)
  
  def parsePositiveDouble(s: String): Option[Int] =
    s.toIntOption.filter(x => x > 0).map(x => x * 2)


object Part2:
  
  def doubleEither(e: Either[String, Int]): Either[String, Int] =
    e.map(x => x * 2)
  
  def validatePositive(n: Int): Either[String, Int] =
    if n > 0 then Right(n) else Left("Must be positive")
  
  def validateInRange(n: Int): Either[String, Int] =
    validatePositive(n).flatMap(x => if x < 100 then Right(x) else Left("Must be less than 100"))
  
  def parseAndValidate(s: String): Either[String, Int] =
    s.toIntOption.toRight("Invalid integer").flatMap(x => validatePositive(x))
  
  def getUserBalance(
    users: Map[String, String],      
    balances: Map[String, Double],   
    username: String
  ): Either[String, Double] =
    users.get(username).toRight("User not found").flatMap { id =>
      balances.get(id).toRight("Balance not found").flatMap { bal =>
        if bal > 0 then Right(bal) else Left("Negative balance")
      }
    }
  
  def safeDivide(a: Int, b: Int): Either[String, Double] =
    if b == 0 then Left("Division by zero") else Right(a.toDouble / b)
  
  def divideChain(a: Int, b: Int, c: Int): Either[String, Double] =
    safeDivide(a, b).flatMap(res => safeDivide(res.toInt, c))
  
  def addEithers(
    a: Either[String, Int],
    b: Either[String, Int]
  ): Either[String, Int] =
    a.flatMap(x => b.map(y => x + y))
  
  def optionToEither[A](opt: Option[A], errorMsg: String): Either[String, A] =
    opt.toRight(errorMsg)
  
  case class Registration(username: String, email: String, age: Int)
  
  def validateRegistration(
    username: String,
    email: String,
    age: Int
  ): Either[String, Registration] =
    if username.isEmpty then Left("Username required")
    else if !email.contains("@") then Left("Invalid email")
    else if age < 13 || age > 120 then Left("Invalid age")
    else Right(Registration(username, email, age))


object Part3:
  
  def safeDivide(a: Int, b: Int): Try[Int] =
    Try(a / b)
  
  def parseAndDouble(s: String): Try[Int] =
    Try(s.toInt).map(x => x * 2)
  
  def parseAndAdd(s1: String, s2: String): Try[Int] =
    Try(s1.toInt).flatMap(x => Try(s2.toInt).map(y => x + y))
  
  def parseOrZero(s: String): Int =
    Try(s.toInt).getOrElse(0)
  
  def tryToOption[A](t: Try[A]): Option[A] =
    t.toOption
  
  def tryToEither[A](t: Try[A]): Either[String, A] =
    t.toEither match
      case Right(v) => Right(v)
      case Left(ex) => Left(ex.getMessage)
  
  def readFile(path: String): Try[String] =
    if path == "valid.txt" then Success("5")
    else if path == "zero.txt" then Success("0")
    else Failure(new java.io.FileNotFoundException(path))
  
  def processFile(path: String): Try[Int] =
    readFile(path).flatMap(s => Try(s.toInt)).flatMap(n => Try(100 / n))
  
  def processFileOrDefault(path: String): Int =
    processFile(path).getOrElse(-1)


object Part4:
  
  case class Address(street: String, city: String)
  case class User(name: String, address: Option[Address])
  
  def getUserCity(users: Map[String, User], username: String): Option[String] =
    users.get(username).flatMap(u => u.address).map(a => a.city)
  
  def parsePrice(s: String): Option[Double] =
    if s.length > 1 then s.substring(1).toDoubleOption else None
  
  def lookupAll[K, V](keys: List[K], map: Map[K, V]): Option[List[V]] =
    keys.foldRight(Option(List.empty[V])) { (key, accOpt) =>
      accOpt.flatMap(acc => map.get(key).map(value => value :: acc))
    }
  
  case class OrderLine(productId: String, quantity: Int)
  
  def calculateTotal(
    lines: List[OrderLine],
    prices: Map[String, Double]
  ): Option[Double] =
    lines.foldRight(Option(List.empty[Double])) { (line, accOpt) =>
      accOpt.flatMap(acc => prices.get(line.productId).map(pr => (pr * line.quantity) :: acc))
    }.map(list => list.sum)
  
  def processCommand(command: String, base: Int): Either[String, Int] =
    val parts = command.split(" ")
    if parts.length != 2 then Left("Invalid command format")
    else
      val op = parts(0)
      val valueStr = parts(1)
      valueStr.toIntOption.toRight("Invalid value").flatMap { value =>
        if value <= 0 then Left("Value must be positive")
        else if op == "add" then Right(base + value)
        else if op == "subtract" then Right(base - value)
        else Left(s"Unknown operation: $op")
      }
  
  case class Config(host: String, port: Int, timeout: Int)
  
  def parseConfig(
    host: String,
    portStr: String,
    timeoutStr: String
  ): Try[Config] =
    Try(portStr.toInt).flatMap { port =>
      Try(timeoutStr.toInt).map { timeout =>
        Config(host, port, timeout)
      }
    }
  
  def tryParse[A](s: String, parsers: List[String => Option[A]]): Option[A] =
    parsers.foldLeft(Option.empty[A]) { (acc, parser) =>
      acc.orElse(parser(s))
    }
  
  def flatten[A](opt: Option[Option[A]]): Option[A] =
    opt.flatten


object Part5:
  
  def addThreeFor(a: Option[Int], b: Option[Int], c: Option[Int]): Option[Int] =
    for
      x <- a
      y <- b
      z <- c
    yield x + y + z
  
  case class Address(city: String)
  case class User(name: String, address: Option[Address])
  
  def getUserCityFor(users: Map[String, User], username: String): Option[String] =
    for
      user    <- users.get(username)
      address <- user.address
    yield address.city
  
  case class Person(name: String, age: Int)
  
  def validateName(name: String): Either[String, String] =
    if name.nonEmpty then Right(name) else Left("Name required")
  
  def validateAge(age: Int): Either[String, Int] =
    if age > 0 && age < 150 then Right(age) else Left("Invalid age")
  
  def createPersonFor(name: String, age: Int): Either[String, Person] =
    for
      validName <- validateName(name)
      validAge  <- validateAge(age)
    yield Person(validName, validAge)
  
  def safeDivideFor(s1: String, s2: String): Try[Double] =
    for
      n1  <- Try(s1.toDouble)
      n2  <- Try(s2.toDouble)
      res <- if n2 == 0 then Failure(new ArithmeticException("Division by zero")) else Success(n1 / n2)
    yield res
  
  def parsePositiveDoubleFor(s: String): Option[Int] =
    for
      n <- s.toIntOption
      if n > 0
    yield n * 2
  
  def complexCalculation(
    a: Int, b: Int,
    c: Int, d: Int
  ): Either[String, Double] =
    def safeDivide(x: Int, y: Int): Either[String, Double] =
      if y == 0 then Left(s"Division by zero: $x / $y")
      else Right(x.toDouble / y)
    
    for
      res1 <- safeDivide(a, b)
      res2 <- safeDivide(c, d)
    yield res1 + res2


object Bonus:
  
  case class AuthToken(value: String)
  case class Post(title: String, content: String)
  case class UserData(id: String, posts: List[Post])
  
  def authenticate(user: String, pass: String): Either[String, AuthToken] =
    if user == "admin" && pass == "secret" then Right(AuthToken("token123"))
    else Left("Invalid credentials")
  
  def fetchUser(token: AuthToken, userId: String): Either[String, UserData] =
    if userId == "user1" then Right(UserData("user1", List(Post("Hello", "World"))))
    else Left(s"User not found: $userId")
  
  def getFirstPostTitle(
    user: String,
    pass: String,
    userId: String
  ): Either[String, String] =
    for
      token     <- authenticate(user, pass)
      data      <- fetchUser(token, userId)
      firstPost <- data.posts.headOption.toRight("No posts found")
    yield firstPost.title
  
  case class Config(host: String, port: Int, timeout: Int)
  
  def parseConfigString(config: String): Either[String, Config] =
    val pairs = config.split(";").map(_.split("=")).collect {
      case Array(k, v) => k -> v
    }.toMap
    
    for
      host    <- pairs.get("host").toRight("Missing host")
      portStr <- pairs.get("port").toRight("Missing port")
      port    <- portStr.toIntOption.toRight("Invalid port")
      timeStr <- pairs.get("timeout").toRight("Missing timeout")
      timeout <- timeStr.toIntOption.toRight("Invalid timeout")
    yield Config(host, port, timeout)