package etl

import io.circe.*
import io.circe.generic.semiauto.*

/**
 * Data models for the Mini-ETL
 * 
 * Domain: Sales analysis for an online store
 */

// ============================================
// Raw Data
// ============================================

/**
 * Represents a sale as it arrives in the JSON file
 */
case class RawSale(
  id: String,
  product_name: String,
  category: String,
  price: String,           // String because it may contain errors
  quantity: String,        // String because it may contain errors
  date: String,            // Format: "YYYY-MM-DD"
  customer_id: String,
  region: String
)

object RawSale:
  given Decoder[RawSale] = deriveDecoder[RawSale]
  given Encoder[RawSale] = deriveEncoder[RawSale]


// ============================================
// Validated Data
// ============================================

/**
 * Represents a sale after validation
 */
case class ValidatedSale(
  id: String,
  productName: String,
  category: Category,
  price: Double,
  quantity: Int,
  date: SaleDate,
  customerId: String,
  region: Region
)

/**
 * Product categories
 */
enum Category:
  case Electronics, Clothing, Food, Books, Home, Other

object Category:
  def fromString(s: String): Option[Category] =
    s.toLowerCase match
      case "electronics" | "électronique" => Some(Electronics)
      case "clothing" | "vêtements" => Some(Clothing)
      case "food" | "alimentation" => Some(Food)
      case "books" | "livres" => Some(Books)
      case "home" | "maison" => Some(Home)
      case "other" | "autre" => Some(Other)
      case _ => None
  
  given Encoder[Category] = Encoder.encodeString.contramap(_.toString)

/**
 * Regions
 */
enum Region:
  case North, South, East, West, Central

object Region:
  def fromString(s: String): Option[Region] =
    s.toLowerCase match
      case "north" | "nord" => Some(North)
      case "south" | "sud" => Some(South)
      case "east" | "est" => Some(East)
      case "west" | "ouest" => Some(West)
      case "central" | "centre" => Some(Central)
      case _ => None
  
  given Encoder[Region] = Encoder.encodeString.contramap(_.toString)

/**
 * Sale date
 */
case class SaleDate(year: Int, month: Int, day: Int):
  override def toString: String = f"$year-$month%02d-$day%02d"

object SaleDate:
  def parse(s: String): Option[SaleDate] =
    s.split("-").toList match
      case y :: m :: d :: Nil =>
        for
          year <- y.toIntOption
          month <- m.toIntOption if month >= 1 && month <= 12
          day <- d.toIntOption if day >= 1 && day <= 31
        yield SaleDate(year, month, day)
      case _ => None
  
  given Encoder[SaleDate] = Encoder.encodeString.contramap(_.toString)


// ============================================
// Aggregated Data
// ============================================

/**
 * Statistics by category
 */
case class CategoryStats(
  category: Category,
  totalSales: Int,
  totalRevenue: Double,
  averagePrice: Double,
  averageQuantity: Double
)

object CategoryStats:
  given Encoder[CategoryStats] = deriveEncoder[CategoryStats]

/**
 * Statistics by region
 */
case class RegionStats(
  region: Region,
  totalSales: Int,
  totalRevenue: Double,
  topCategory: Category
)

object RegionStats:
  given Encoder[RegionStats] = deriveEncoder[RegionStats]

/**
 * Monthly statistics
 */
case class MonthlyStats(
  year: Int,
  month: Int,
  totalSales: Int,
  totalRevenue: Double
)

object MonthlyStats:
  given Encoder[MonthlyStats] = deriveEncoder[MonthlyStats]

/**
 * Final report
 */
case class SalesReport(
  totalRecords: Int,
  validRecords: Int,
  invalidRecords: Int,
  totalRevenue: Double,
  byCategory: List[CategoryStats],
  byRegion: List[RegionStats],
  byMonth: List[MonthlyStats],
  errors: List[String]
)

object SalesReport:
  given Encoder[SalesReport] = deriveEncoder[SalesReport]


// ============================================
// Errors
// ============================================

/**
 * Validation errors
 */
case class ValidationError(saleId: String, field: String, message: String):
  override def toString: String = s"[$saleId] $field: $message"
