# Session 8 - Final Project: Mini-ETL

**Duration**: 4h  
**Date**: June 16, 2025

## Objective

Build a **complete ETL pipeline** that applies all learned concepts:

- Reading JSON files
- Data validation
- Functional transformation
- Aggregation and statistics
- Report generation

## The Project: Sales Analysis

You will create a pipeline that:

1. **Extract**: Reads a JSON file of raw sales
2. **Transform**: Validates and transforms each sale
3. **Aggregate**: Calculates statistics (by category, region, month)
4. **Load**: Generates a JSON report

## Project Structure

```
seance-8/
├── data/
│   └── sales.json           # Input data
├── src/main/scala/etl/
│   ├── Models.scala         # Data models (provided)
│   └── Pipeline.scala       # To complete!
├── src/test/scala/
│   └── PipelineSpec.scala   # Tests
└── output/
    └── report.json          # Generated report
```

## Input Data

```json
{
  "id": "S001",
  "product_name": "Laptop Pro",
  "category": "Electronics",
  "price": "1299.99",
  "quantity": "2",
  "date": "2024-01-15",
  "customer_id": "C001",
  "region": "North"
}
```

**Note**: The data intentionally contains errors to handle!

## Output Report

```json
{
  "totalRecords": 20,
  "validRecords": 15,
  "invalidRecords": 5,
  "totalRevenue": 12500.00,
  "byCategory": [...],
  "byRegion": [...],
  "byMonth": [...],
  "errors": [...]
}
```

## Exercises

### Step 1: Extract (30 min)

1.1. `readFile` - Read a file  
1.2. `parseJson` - Parse JSON to `List[RawSale]`  
1.3. `extract` - Combine both

### Step 2: Transform (90 min)

2.1. `validatePrice` - Validate price  
2.2. `validateQuantity` - Validate quantity  
2.3. `validateCategory` - Validate category  
2.4. `validateRegion` - Validate region  
2.5. `validateDate` - Validate date  
2.6. `validateSale` - Validate a complete sale  
2.7. `transform` - Transform all sales

### Step 3: Aggregate (60 min)

3.1. `revenue` - Calculate a sale's revenue  
3.2. `totalRevenue` - Calculate total revenue  
3.3. `statsByCategory` - Statistics by category  
3.4. `statsByRegion` - Statistics by region  
3.5. `statsByMonth` - Statistics by month

### Step 4: Load (30 min)

4.1. `buildReport` - Build the report  
4.2. `reportToJson` - Convert to JSON  
4.3. `writeReport` - Write the file

### Step 5: Pipeline (30 min)

5. `run` - Execute the complete pipeline

## Commands

```bash
# Compile
sbt compile

# Run tests
sbt test

# Execute the pipeline
sbt "run data/sales.json output/report.json"
```

## Concepts Used

| Concept | Where |
|---------|-------|
| `Either` for errors | Everywhere! |
| `for`-comprehension | Validation, pipeline |
| Pattern matching | Validation, aggregation |
| `map`, `flatMap`, `filter` | Transformation |
| `groupBy`, `foldLeft` | Aggregation |
| Circe | JSON |
| Case classes | Models |
| Enums | Category, Region |

## Grading (indicative)

| Part | Points |
|------|--------|
| Extract | 15% |
| Transform | 40% |
| Aggregate | 25% |
| Load | 10% |
| Complete pipeline | 10% |

## Tips

1. **Start with tests** - Verify each function before moving to the next
2. **Use for-comprehension** - More readable for chaining Either
3. **Don't panic** - Errors are expected in the data
4. **Read the models** - Everything is already defined in `Models.scala`

## Good luck!

---

*FP with Scala - V2 - Session 8 - Final Project*
