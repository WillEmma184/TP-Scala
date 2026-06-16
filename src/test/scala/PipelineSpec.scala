package etl

import munit.FunSuite
import java.nio.file.Files

class PipelineSpec extends FunSuite:
  
  // ============================================
  // Tests Extract
  // ============================================
  
  test("1.1 - readFile"):
    val tempFile = Files.createTempFile("test", ".txt")
    Files.writeString(tempFile, "Hello World")
    
    val result = Pipeline.readFile(tempFile.toString)
    assert(result.isRight)
    assertEquals(result.toOption.get, "Hello World")
    
    Files.delete(tempFile)
  
  test("1.2 - parseJson"):
    val json = """[{"id": "S001", "product_name": "Test", "category": "Electronics", "price": "10.0", "quantity": "1", "date": "2024-01-01", "customer_id": "C001", "region": "North"}]"""
    val result = Pipeline.parseJson(json)
    assert(result.isRight)
    assertEquals(result.toOption.get.length, 1)
  
  
  // ============================================
  // Tests Transform
  // ============================================
  
  val validRaw = RawSale("S001", "Test", "Electronics", "99.99", "5", "2024-01-15", "C001", "North")
  
  test("2.1 - validatePrice valid"):
    val result = Pipeline.validatePrice(validRaw)
    assert(result.isRight)
    assertEquals(result.toOption.get, 99.99)
  
  test("2.1 - validatePrice invalid"):
    val invalid = validRaw.copy(price = "not_a_number")
    assert(Pipeline.validatePrice(invalid).isLeft)
  
  test("2.2 - validateQuantity valid"):
    val result = Pipeline.validateQuantity(validRaw)
    assert(result.isRight)
    assertEquals(result.toOption.get, 5)
  
  test("2.2 - validateQuantity invalid"):
    val invalid = validRaw.copy(quantity = "-5")
    assert(Pipeline.validateQuantity(invalid).isLeft)
  
  test("2.3 - validateCategory valid"):
    val result = Pipeline.validateCategory(validRaw)
    assert(result.isRight)
    assertEquals(result.toOption.get, Category.Electronics)
  
  test("2.3 - validateCategory invalid"):
    val invalid = validRaw.copy(category = "Unknown")
    assert(Pipeline.validateCategory(invalid).isLeft)
  
  test("2.4 - validateRegion valid"):
    val result = Pipeline.validateRegion(validRaw)
    assert(result.isRight)
    assertEquals(result.toOption.get, Region.North)
  
  test("2.5 - validateDate valid"):
    val result = Pipeline.validateDate(validRaw)
    assert(result.isRight)
    assertEquals(result.toOption.get, SaleDate(2024, 1, 15))
  
  test("2.6 - validateSale complete"):
    val result = Pipeline.validateSale(validRaw)
    assert(result.isRight)
    val sale = result.toOption.get
    assertEquals(sale.id, "S001")
    assertEquals(sale.price, 99.99)
    assertEquals(sale.quantity, 5)
  
  test("2.7 - transform"):
    val raw = List(
      validRaw,
      validRaw.copy(id = "S002", price = "invalid")
    )
    val result = Pipeline.transform(raw)
    assertEquals(result.valid.length, 1)
    assertEquals(result.errors.length, 1)
  
  
  // ============================================
  // Tests Aggregate
  // ============================================
  
  val sampleSales = List(
    ValidatedSale("S001", "Product A", Category.Electronics, 100.0, 2, SaleDate(2024, 1, 15), "C001", Region.North),
    ValidatedSale("S002", "Product B", Category.Electronics, 50.0, 3, SaleDate(2024, 1, 20), "C002", Region.North),
    ValidatedSale("S003", "Product C", Category.Clothing, 30.0, 5, SaleDate(2024, 2, 1), "C003", Region.South)
  )
  
  test("3.1 - revenue"):
    val sale = sampleSales.head
    assertEquals(Pipeline.revenue(sale), 200.0)
  
  test("3.2 - totalRevenue"):
    val total = Pipeline.totalRevenue(sampleSales)
    assertEquals(total, 500.0)
  
  test("3.3 - statsByCategory"):
    val stats = Pipeline.statsByCategory(sampleSales)
    assert(stats.nonEmpty)
    val electronics = stats.find(_.category == Category.Electronics)
    assert(electronics.isDefined)
    assertEquals(electronics.get.totalSales, 2)
  
  test("3.4 - statsByRegion"):
    val stats = Pipeline.statsByRegion(sampleSales)
    assert(stats.nonEmpty)
    val north = stats.find(_.region == Region.North)
    assert(north.isDefined)
    assertEquals(north.get.totalSales, 2)
  
  test("3.5 - statsByMonth"):
    val stats = Pipeline.statsByMonth(sampleSales)
    assertEquals(stats.length, 2)
  
  
  // ============================================
  // Tests Load
  // ============================================
  
  test("4.1 - buildReport"):
    val transformResult = Pipeline.TransformResult(sampleSales, List())
    val report = Pipeline.buildReport(3, transformResult)
    assertEquals(report.totalRecords, 3)
    assertEquals(report.validRecords, 3)
    assertEquals(report.invalidRecords, 0)
  
  test("4.2 - reportToJson"):
    val transformResult = Pipeline.TransformResult(sampleSales, List())
    val report = Pipeline.buildReport(3, transformResult)
    val json = Pipeline.reportToJson(report)
    assert(json.contains("totalRecords"))
    assert(json.contains("byCategory"))
  
  
  // ============================================
  // Test Complete Pipeline
  // ============================================
  
  test("5 - Complete pipeline"):
    val inputPath = "data/sales.json"
    val tempOutput = Files.createTempFile("report", ".json")
    
    val result = Pipeline.run(inputPath, tempOutput.toString)
    assert(result.isRight, s"Pipeline failed: ${result.left.toOption}")
    
    val report = result.toOption.get
    assertEquals(report.totalRecords, 20)
    assert(report.validRecords > 0)
    assert(report.invalidRecords > 0)
    
    Files.delete(tempOutput)
