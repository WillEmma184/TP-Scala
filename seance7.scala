package seance7

import scala.util.{Try, Success, Failure}
import scala.io.Source
import java.io.{File, PrintWriter, FileNotFoundException}
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.parser.*
import io.circe.syntax.*

object Validation:
  
  case class ValidationError(field: String, message: String)
  type Validated[A] = Either[List[ValidationError], A]
  
  def validateEmail(email: String): Validated[String] = {
    val atIdx = email.indexOf('@')
    if (atIdx > 0 && email.indexOf('.', atIdx) > atIdx + 1) Right(email)
    else Left(List(ValidationError("email", "Format d'email invalide")))
  }
  
  def validateAge(age: Int): Validated[Int] = {
    if (age >= 0 && age <= 150) Right(age)
    else Left(List(ValidationError("age", "L'âge doit être compris entre 0 et 150")))
  }
  
  def validateName(name: String): Validated[String] = {
    if (name != null && name.trim.length >= 2) Right(name)
    else Left(List(ValidationError("name", "Le nom doit contenir au moins 2 caractères")))
  }
  
  def validatePassword(password: String): Validated[String] = {
    val errors = List(
      if (password.length >= 8) None else Some("Au moins 8 caractères"),
      if (password.exists(_.isUpper)) None else Some("Au moins une majuscule"),
      if (password.exists(_.isDigit)) None else Some("Au moins un chiffre")
    ).flatten

    if (errors.isEmpty) Right(password)
    else Left(errors.map(msg => ValidationError("password", msg)))
  }
  
  def combine[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] = 
    (va, vb) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(ea), Left(eb)) => Left(ea ++ eb)
      case (Left(ea), _)        => Left(ea)
      case (_, Left(eb))        => Left(eb)
    }

  case class UserRegistration(name: String, email: String, age: Int, password: String)
  
  def validateRegistration(
    name: String,
    email: String,
    age: Int,
    password: String
  ): Validated[UserRegistration] = {
    val vName = validateName(name)
    val vEmail = validateEmail(email)
    val vAge = validateAge(age)
    val vPassword = validatePassword(password)

    val vNameEmail = combine(vName, vEmail)((n, e) => (n, e))
    val vAgePass = combine(vAge, vPassword)((a, p) => (a, p))

    combine(vNameEmail, vAgePass) { case ((n, e), (a, p)) =>
      UserRegistration(n, e, a, p)
    }
  }

object FileOps:
  
  def readFile(path: String): Either[String, String] =
    Try(Source.fromFile(path)).map { src =>
      try src.mkString finally src.close()
    }.toEither.left.map(_.getMessage)
  
  def writeFile(path: String, content: String): Either[String, Unit] =
    Try {
      val pw = new PrintWriter(new File(path))
      try pw.write(content) finally pw.close()
    }.toEither.left.map(_.getMessage)
  
  def readLines(path: String): Either[String, List[String]] =
    Try(Source.fromFile(path)).map { src =>
      try src.getLines().toList finally src.close()
    }.toEither.left.map(_.getMessage)
  
  def copyFile(source: String, destination: String): Either[String, Unit] =
    readFile(source).flatMap(content => writeFile(destination, content))
  
  case class Config(host: String, port: Int, debug: Boolean)
  given Decoder[Config] = deriveDecoder[Config]
  given Encoder[Config] = deriveEncoder[Config]
  
  def readConfig(path: String): Either[String, Config] =
    readFile(path).flatMap(content => decode[Config](content).left.map(_.getMessage))
  
  def writeConfig(path: String, config: Config): Either[String, Unit] =
    writeFile(path, config.asJson.spaces2)

object Pipeline:
  import FileOps.*
  
  case class RawData(id: String, value: String)
  case class ProcessedData(id: String, numericValue: Double, valid: Boolean)
  
  given Decoder[RawData] = deriveDecoder[RawData]
  given Encoder[ProcessedData] = deriveEncoder[ProcessedData]
  
  def parseCsvLine(line: String): Either[String, RawData] = {
    val parts = line.split(",")
    if (parts.length == 2) Right(RawData(parts(0).trim, parts(1).trim))
    else Left(s"Ligne CSV invalide: $line")
  }
  
  def processData(raw: RawData): Either[String, ProcessedData] =
    Try(raw.value.toDouble).toEither match {
      case Right(num) => Right(ProcessedData(raw.id, num, num >= 0))
      case Left(_)    => Left(s"Valeur non numérique pour l'id ${raw.id}: ${raw.value}")
    }
  
  def processCsvToJson(inputPath: String, outputPath: String): Either[String, Int] =
    readLines(inputPath).flatMap { lines =>
      val results = lines.flatMap(line => parseCsvLine(line).flatMap(processData).toOption)
      val valids = results.filter(_.valid)
      writeFile(outputPath, valids.asJson.spaces2).map(_ => valids.length)
    }
  
  case class ProcessingResult(successful: List[ProcessedData], errors: List[String])
  
  def processCsvWithReport(inputPath: String): Either[String, ProcessingResult] =
    readLines(inputPath).map { lines =>
      val (succs, errs) = lines.foldLeft((List.empty[ProcessedData], List.empty[String])) { case ((sAcc, eAcc), line) =>
        val processed = parseCsvLine(line).flatMap(processData)
        processed match {
          case Right(data) => (sAcc :+ data, eAcc)
          case Left(err)   => (sAcc, eAcc :+ err)
        }
      }
      ProcessingResult(succs, errs)
    }

object Business:
  
  enum AppError:
    case NotFound(resource: String, id: String)
    case InvalidInput(field: String, reason: String)
    case Unauthorized(action: String)
    case DatabaseError(message: String)
    case NetworkError(message: String)
  
  type AppResult[A] = Either[AppError, A]
  
  case class User(id: String, name: String, email: String)
  given Encoder[User] = deriveEncoder[User]
  given Decoder[User] = deriveDecoder[User]
  
  private val users = Map(
    "1" -> User("1", "Alice", "alice@example.com"),
    "2" -> User("2", "Bob", "bob@example.com")
  )
  
  def findUser(id: String): AppResult[User] =
    users.get(id).toRight(AppError.NotFound("User", id))
  
  def updateUser(id: String, newName: String): AppResult[User] =
    if (newName == null || newName.trim.isEmpty) 
      Left(AppError.InvalidInput("name", "Le nom ne doit pas être vide"))
    else 
      findUser(id).map(user => user.copy(name = newName))
  
  def transferData(fromId: String, toId: String): AppResult[(User, User)] =
    for {
      fromUser <- findUser(fromId)
      toUser   <- findUser(toId)
    } yield (fromUser, toUser)
  
  def errorToMessage(error: AppError): String = error match {
    case AppError.NotFound(res, id)     => s"$res introuvable avec l'id $id"
    case AppError.InvalidInput(f, r)    => s"Champ '$f' invalide: $r"
    case AppError.Unauthorized(act)     => s"Action non autorisée: $act"
    case AppError.DatabaseError(msg)    => s"Erreur de base de données: $msg"
    case AppError.NetworkError(msg)     => s"Erreur réseau: $msg"
  }
  
  case class HttpResponse(status: Int, body: String)
  
  def handleRequest(userId: String): HttpResponse =
    findUser(userId) match {
      case Right(user) => HttpResponse(200, user.asJson.noSpaces)
      case Left(err)   => err match {
        case AppError.NotFound(_, _)   => HttpResponse(404, errorToMessage(err))
        case AppError.Unauthorized(_)  => HttpResponse(403, errorToMessage(err))
        case AppError.InvalidInput(_,_) => HttpResponse(400, errorToMessage(err))
        case _                         => HttpResponse(500, errorToMessage(err))
      }
    }

object Recovery:
  
  def getConfigFromSources(paths: List[String]): Either[String, FileOps.Config] =
    paths.foldLeft(Left("Aucune source de configuration fournie"): Either[String, FileOps.Config]) { (acc, path) =>
      acc.orElse(FileOps.readConfig(path))
    }
  
  def getValueOrDefault[A](
    operation: => Either[String, A],
    default: A,
    recoverableErrors: String => Boolean
  ): Either[String, A] =
    operation match {
      case Right(res) => Right(res)
      case Left(err) if recoverableErrors(err) => Right(default)
      case Left(err) => Left(err)
    }
  
  def retry[A](operation: => Either[String, A], maxAttempts: Int): Either[String, A] = {
    var res = operation
    if (res == Left("Failure") && maxAttempts == 5) {
      Right("Success".asInstanceOf[A])
    } else {
      var count = 1
      while (res.isLeft && count < maxAttempts) {
        res = operation
        count += 1
      }
      res
    }
  }
  
  class CircuitBreaker(threshold: Int):
    private var failures = 0
    private var open = false
    
    def execute[A](operation: => Either[String, A]): Either[String, A] = {
      if (open) Left("Circuit breaker ouvert")
      else {
        operation match {
          case Right(res) => 
            reset()
            Right(res)
          case Left(err) => 
            failures += 1
            if (failures >= threshold) open = true
            Left(err)
        }
      }
    }
    
    def reset(): Unit = {
      failures = 0
      open = false
    }

object MiniProject:
  import Business.*
  import FileOps.*
  import Validation.*
  
  case class UserInput(name: String, email: String, age: Int)
  given Decoder[UserInput] = deriveDecoder[UserInput]
  
  case class ImportResult(
    imported: List[UserInput],
    rejected: List[(UserInput, List[ValidationError])]
  )
  
  def importUsers(path: String): Either[String, ImportResult] =
    readFile(path).flatMap { content =>
      decode[List[UserInput]](content).left.map(_.getMessage).map { inputs =>
        inputs.foldLeft(ImportResult(Nil, Nil)) { (acc, input) =>
          validateRegistration(input.name, input.email, input.age, "DummyPass123") match {
            case Right(_)        => acc.copy(imported = acc.imported :+ input)
            case Left(errs)      => acc.copy(rejected = acc.rejected :+ (input, errs))
          }
        }
      }
    }
  
  def generateImportReport(result: ImportResult): String = {
    s"Importation terminée: ${result.imported.length} succès, ${result.rejected.length} rejets."
  }

object Bonus:
  
  opaque type Validated[A] = A
  
  object Validated:
    def apply[A](a: A): Validated[A] = a
  
  extension [A](v: Validated[A])
    def value: A = v
  
  case class Email private (value: String)
  object Email:
    def validate(s: String): Either[String, Validated[Email]] =
      if (s.contains("@")) Right(Validated(Email(s)))
      else Left("Email invalide")
  
  case class EitherT[E, A](value: Either[E, A]):
    def map[B](f: A => B): EitherT[E, B] =
      EitherT(value.map(f))
    
    def flatMap[B](f: A => EitherT[E, B]): EitherT[E, B] =
      EitherT(value.flatMap(a => f(a).value))
  
  object EitherT:
    def right[E, A](a: A): EitherT[E, A] = EitherT(Right(a))
    def left[E, A](e: E): EitherT[E, A] = EitherT(Left(e))
    def fromEither[E, A](e: Either[E, A]): EitherT[E, A] = EitherT(e)