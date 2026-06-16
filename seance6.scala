package seance6

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.parser.*
import io.circe.syntax.*

case class Person(name: String, age: Int, email: Option[String])
case class Address(street: String, city: String, zipCode: String, country: String)
case class User(id: Int, name: String, address: Address, active: Boolean)
case class Product(id: String, name: String, price: Double, inStock: Boolean)
case class OrderLine(productId: String, quantity: Int)
case class Order(id: String, customerId: Int, lines: List[OrderLine], total: Double)

object Decoders:
  
  given personDecoder: Decoder[Person] = new Decoder[Person]:
    def apply(c: HCursor): Decoder.Result[Person] =
      for
        name  <- c.get[String]("name")
        age   <- c.get[Int]("age")
        email <- c.get[Option[String]]("email")
      yield Person(name, age, email)
  
  given addressDecoder: Decoder[Address] = deriveDecoder[Address]
  given userDecoder: Decoder[User] = deriveDecoder[User]
  given productDecoder: Decoder[Product] = deriveDecoder[Product]
  given orderLineDecoder: Decoder[OrderLine] = deriveDecoder[OrderLine]
  given orderDecoder: Decoder[Order] = deriveDecoder[Order]

object Encoders:
  
  given personEncoder: Encoder[Person] = new Encoder[Person]:
    def apply(p: Person): Json = 
      Json.obj(
        "name"  -> Json.fromString(p.name),
        "age"   -> Json.fromInt(p.age),
        "email" -> p.email.fold(Json.Null)(Json.fromString)
      )
  
  given addressEncoder: Encoder[Address] = deriveEncoder[Address]
  given userEncoder: Encoder[User] = deriveEncoder[User]
  given productEncoder: Encoder[Product] = deriveEncoder[Product]
  given orderLineEncoder: Encoder[OrderLine] = deriveEncoder[OrderLine]
  given orderEncoder: Encoder[Order] = deriveEncoder[Order]

object JsonOps:
  import Decoders.given
  import Encoders.given
  
  def parsePerson(json: String): Either[Error, Person] =
    decode[Person](json)
  
  def parseProducts(json: String): Either[Error, List[Product]] =
    decode[List[Product]](json)
  
  def parseActiveUser(json: String): Either[String, User] =
    decode[User](json).left.map(_.getMessage).flatMap { user =>
      if (user.active) Right(user)
      else Left("User is not active")
    }
  
  def personToJson(person: Person): String =
    person.asJson.noSpaces
  
  def personToJsonPretty(person: Person): String =
    person.asJson.spaces2
  
  def addProcessedField(json: String): Either[Error, String] =
    parse(json).map { j =>
      j.mapObject(_.add("processed", Json.fromBoolean(true)))
    }.map(_.noSpaces)

object Advanced:
  import Decoders.given
  import Encoders.given
  
  def extractCity(json: String): Either[Error, String] =
    parse(json).flatMap { j =>
      j.hcursor.downField("user").downField("address").get[String]("city")
    }
  
  case class ProductWithDefault(id: String, name: String, price: Double, inStock: Boolean)
  
  given productWithDefaultDecoder: Decoder[ProductWithDefault] = new Decoder[ProductWithDefault]:
    def apply(c: HCursor): Decoder.Result[ProductWithDefault] =
      for
        id      <- c.get[String]("id")
        name    <- c.get[String]("name")
        price   <- c.get[Double]("price")
        inStock <- c.getOrElse[Boolean]("inStock")(true)
      yield ProductWithDefault(id, name, price, inStock)
  
  case class ProductRenamed(productId: String, name: String)
  
  given productRenamedDecoder: Decoder[ProductRenamed] = new Decoder[ProductRenamed]:
    def apply(c: HCursor): Decoder.Result[ProductRenamed] =
      for
        productId <- c.get[String]("product_id")
        name      <- c.get[String]("name")
      yield ProductRenamed(productId, name)
  
  sealed trait Shape
  case class Circle(radius: Double) extends Shape
  case class Rectangle(width: Double, height: Double) extends Shape
  
  given shapeDecoder: Decoder[Shape] = new Decoder[Shape]:
    def apply(c: HCursor): Decoder.Result[Shape] =
      c.get[String]("type").flatMap {
        case "circle"    => c.get[Double]("radius").map(Circle.apply)
        case "rectangle" => 
          for
            w <- c.get[Double]("width")
            h <- c.get[Double]("height")
          yield Rectangle(w, h)
        case other => Left(DecodingFailure(s"Unknown shape type: $other", c.history))
      }
  
  case class ValidationError(field: String, message: String)
  
  def validateUser(json: String): Either[List[ValidationError], User] =
    decode[User](json).left.map(e => List(ValidationError("user", e.getMessage)))

object Application:
  import Decoders.given
  import Encoders.given
  
  def processOrdersJson(json: String): Either[Error, List[Order]] =
    decode[List[Order]](json)
  
  def calculateOrdersTotal(json: String): Either[Error, Double] =
    decode[List[Order]](json).map(_.map(_.total).sum)
  
  def filterProducts(json: String, maxPrice: Double): Either[Error, List[Product]] =
    decode[List[Product]](json).map(_.filter(p => p.inStock && p.price < maxPrice))
  
  def usersToMap(json: String): Either[Error, Map[Int, String]] =
    decode[List[User]](json).map(_.map(u => u.id -> u.name).toMap)
  
  def generateReport(json: String): Either[Error, String] =
    decode[List[Order]](json).map { orders =>
      val totalOrders = orders.length
      val totalAmount = orders.map(_.total).sum
      val avgAmount = if (totalOrders == 0) 0.0 else totalAmount / totalOrders
      Json.obj(
        "totalOrders"   -> Json.fromInt(totalOrders),
        "totalAmount"   -> Json.fromDoubleOrNull(totalAmount),
        "averageAmount" -> Json.fromDoubleOrNull(avgAmount)
      ).noSpaces
    }

object Bonus:
  
  case class Config(host: String, port: Int, debug: Boolean)
  given configCodec: Codec[Config] = deriveCodec[Config]
  
  case class TreeNode(value: String, children: List[TreeNode])
  
  given treeNodeDecoder: Decoder[TreeNode] = new Decoder[TreeNode]:
    def apply(c: HCursor): Decoder.Result[TreeNode] =
      for
        v <- c.get[String]("value")
        ch <- c.getOrElse[List[TreeNode]]("children")(Nil)
      yield TreeNode(v, ch)
      
  given treeNodeEncoder: Encoder[TreeNode] = new Encoder[TreeNode]:
    def apply(t: TreeNode): Json =
      Json.obj(
        "value"    -> Json.fromString(t.value),
        "children" -> Json.fromValues(t.children.map(apply))
      )
  
  def updateNestedField(json: String, path: List[String], newValue: String): Either[Error, String] =
    parse(json).map { rootJson =>
      def transform(j: Json, remainingPath: List[String]): Json = remainingPath match {
        case Nil => Json.fromString(newValue)
        case head :: tail =>
          j.mapObject { obj =>
            obj.apply(head) match {
              case Some(child) => obj.add(head, transform(child, tail))
              case None        => obj
            }
          }
      }
      transform(rootJson, path).noSpaces
    }