package etl

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import scala.util.{Try, Using}
import scala.io.Source
import java.io.{File, PrintWriter}

object Pipeline:
  
  def readFile(path: String): Either[String, String] =
    Using(Source.fromFile(path))(_.mkString)
      .toEither
      .left.map(err => s"Erreur de lecture du fichier: ${err.getMessage}")
  
  def parseJson(content: String): Either[String, List[RawSale]] =
    decode[List[RawSale]](content)
      .left.map(err => s"Erreur de parsing JSON: ${err.getMessage}")
  
  def extract(path: String): Either[String, List[RawSale]] =
    for {
      content  <- readFile(path)
      rawSales <- parseJson(content)
    } yield rawSales
  
  def validatePrice(raw: RawSale): Either[ValidationError, Double] =
    raw.price.toDoubleOption
      .filter(_ > 0)
      .toRight(ValidationError(raw.id, "price", s"Le prix doit être un nombre positif (reçu: ${raw.price})"))

  def validateQuantity(raw: RawSale): Either[ValidationError, Int] =
    raw.quantity.toIntOption
      .filter(_ > 0)
      .toRight(ValidationError(raw.id, "quantity", s"La quantité doit être un entier positif (reçu: ${raw.quantity})"))
  
  def validateCategory(raw: RawSale): Either[ValidationError, Category] =
    Category.fromString(raw.category)
      .toRight(ValidationError(raw.id, "category", s"Catégorie inconnue: ${raw.category}"))
  
  def validateRegion(raw: RawSale): Either[ValidationError, Region] =
    Region.fromString(raw.region)
      .toRight(ValidationError(raw.id, "region", s"Région inconnue: ${raw.region}"))
  
  def validateDate(raw: RawSale): Either[ValidationError, SaleDate] =
    SaleDate.parse(raw.date)
      .toRight(ValidationError(raw.id, "date", s"Format de date invalide YYYY-MM-DD (reçu: ${raw.date})"))
  
  def validateSale(raw: RawSale): Either[ValidationError, ValidatedSale] =
    for {
      p <- validatePrice(raw)
      q <- validateQuantity(raw)
      c <- validateCategory(raw)
      r <- validateRegion(raw)
      d <- validateDate(raw)
    } yield ValidatedSale(raw.id, raw.product_name, c, p, q, d, raw.customer_id, r)
  
  case class TransformResult(
    valid: List[ValidatedSale],
    errors: List[ValidationError]
  )
  
  def transform(rawSales: List[RawSale]): TransformResult =
    rawSales.foldLeft(TransformResult(Nil, Nil)) { (acc, raw) =>
      validateSale(raw) match
        case Right(vs) => acc.copy(valid = acc.valid :+ vs)
        case Left(err) => acc.copy(errors = acc.errors :+ err)
    }
  
  def revenue(sale: ValidatedSale): Double =
    sale.price * sale.quantity
  
  def totalRevenue(sales: List[ValidatedSale]): Double =
    sales.map(revenue).sum
  
  def statsByCategory(sales: List[ValidatedSale]): List[CategoryStats] =
    sales.groupBy(_.category).map { (cat, catSales) =>
      val totalSales = catSales.length
      val totalRev   = totalRevenue(catSales)
      val avgPrice   = catSales.map(_.price).sum / totalSales
      val avgQty     = catSales.map(_.quantity).sum.toDouble / totalSales
      CategoryStats(cat, totalSales, totalRev, avgPrice, avgQty)
    }.toList
  
  def statsByRegion(sales: List[ValidatedSale]): List[RegionStats] =
    sales.groupBy(_.region).map { (reg, regSales) =>
      val totalSales = regSales.length
      val totalRev   = totalRevenue(regSales)
      val topCategory = regSales.groupBy(_.category)
        .maxBy((_, catSales) => catSales.length)._1
      RegionStats(reg, totalSales, totalRev, topCategory)
    }.toList
  
  def statsByMonth(sales: List[ValidatedSale]): List[MonthlyStats] =
    sales.groupBy(s => (s.date.year, s.date.month)).map { case ((year, month), monthSales) =>
      MonthlyStats(year, month, monthSales.length, totalRevenue(monthSales))
    }.toList
  
  def buildReport(totalRecords: Int, transformResult: TransformResult): SalesReport =
    SalesReport(
      totalRecords = totalRecords,
      validRecords = transformResult.valid.length,
      invalidRecords = transformResult.errors.length,
      totalRevenue = totalRevenue(transformResult.valid),
      byCategory = statsByCategory(transformResult.valid),
      byRegion = statsByRegion(transformResult.valid),
      byMonth = statsByMonth(transformResult.valid),
      errors = transformResult.errors.map(_.toString)
    )
  
  def reportToJson(report: SalesReport): String =
    report.asJson.spaces2
  
  def writeReport(path: String, report: SalesReport): Either[String, Unit] =
    Try {
      val file = new File(path)
      Option(file.getParentFile).foreach(_.mkdirs())
      val writer = new PrintWriter(file)
      try writer.write(reportToJson(report)) finally writer.close()
    }.toEither.left.map(err => s"Erreur d'écriture du rapport: ${err.getMessage}")
  
  def run(inputPath: String, outputPath: String): Either[String, SalesReport] =
    for {
      rawSales <- extract(inputPath)
      transformRes = transform(rawSales)
      report       = buildReport(rawSales.length, transformRes)
      _        <- writeReport(outputPath, report)
    } yield report

@main def main(args: String*): Unit =
  val inputPath = args.headOption.getOrElse("data/sales.json")
  val outputPath = args.lift(1).getOrElse("output/report.json")
  
  println(s"Starting ETL pipeline...")
  Pipeline.run(inputPath, outputPath) match
    case Right(report) =>
      println(s"Pipeline completed successfully! Written to: $outputPath")
    case Left(error) =>
      println(s"Error: $error")
      sys.exit(1)