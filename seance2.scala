package seance2

object Exercices:
  
  case class Person(name: String, age: Int)
  
  def createPerson(name: String, age: Int): Person =
    Person(name, age)
  
  def getName(person: Person): String =
    person.name
  
  def getAge(person: Person): Int =
    person.age
  
  def isAdult(person: Person): Boolean =
    person.age >= 18
  
  def birthday(person: Person): Person =
    person.copy(age = person.age + 1)
  
  def describe(person: Person): String =
    s"${person.name} is ${person.age} years old"
  
  case class Point(x: Int, y: Int)
  
  def sumCoordinates(point: Point): Int = point match
    case Point(x, y) => x + y
  
  def distanceFromOrigin(point: Point): Double = point match
    case Point(x, y) => math.sqrt(x * x + y * y)
  
  def quadrant(point: Point): String = point match
    case Point(0, 0) => "Origin"
    case Point(x, 0) => "Axis"
    case Point(0, y) => "Axis"
    case Point(x, y) if x > 0 && y > 0 => "I"
    case Point(x, y) if x < 0 && y > 0 => "II"
    case Point(x, y) if x < 0 && y < 0 => "III"
    case Point(x, y) if x > 0 && y < 0 => "IV"
  
  case class Rectangle(bottomLeft: Point, topRight: Point)
  
  def area(rect: Rectangle): Int = rect match
    case Rectangle(Point(x1, y1), Point(x2, y2)) =>
      math.abs(x2 - x1) * math.abs(y2 - y1)
  
  def contains(rect: Rectangle, point: Point): Boolean = (rect, point) match
    case (Rectangle(Point(x1, y1), Point(x2, y2)), Point(px, py)) =>
      px >= x1 && px <= x2 && py >= y1 && py <= y2
  
  enum TrafficLight:
    case Red, Yellow, Green
  
  def nextLight(light: TrafficLight): TrafficLight = light match
    case TrafficLight.Red    => TrafficLight.Green
    case TrafficLight.Green  => TrafficLight.Yellow
    case TrafficLight.Yellow => TrafficLight.Red
  
  def lightAction(light: TrafficLight): String = light match
    case TrafficLight.Red    => "Stop"
    case TrafficLight.Yellow => "Slow down"
    case TrafficLight.Green  => "Go"
  
  sealed trait Shape
  case class Circle(radius: Double) extends Shape
  case class Square(side: Double) extends Shape
  case class RectangleShape(width: Double, height: Double) extends Shape
  
  def shapeArea(shape: Shape): Double = shape match
    case Circle(r)             => math.Pi * r * r
    case Square(s)             => s * s
    case RectangleShape(w, h)  => w * h
  
  def shapePerimeter(shape: Shape): Double = shape match
    case Circle(r)             => 2 * math.Pi * r
    case Square(s)             => 4 * s
    case RectangleShape(w, h)  => 2 * (w + h)
  
  def shapeDescription(shape: Shape): String = shape match
    case Circle(r)             => s"Circle with radius $r"
    case Square(s)             => s"Square with side $s"
    case RectangleShape(w, h)  => s"Rectangle of $w x $h"
  
  enum Expression:
    case Number(value: Int)
    case Addition(left: Expression, right: Expression)
    case Subtraction(left: Expression, right: Expression)
    case Multiplication(left: Expression, right: Expression)
  
  import Expression.*
  
  def evaluate(expr: Expression): Int = expr match
    case Number(v)         => v
    case Addition(l, r)    => evaluate(l) + evaluate(r)
    case Subtraction(l, r) => evaluate(l) - evaluate(r)
    case Multiplication(l, r) => evaluate(l) * evaluate(r)
  
  def exprToString(expr: Expression): String = expr match
    case Number(v)         => v.toString
    case Addition(l, r)    => s"(${exprToString(l)} + ${exprToString(r)})"
    case Subtraction(l, r) => s"(${exprToString(l)} - ${exprToString(r)})"
    case Multiplication(l, r) => s"(${exprToString(l)} * ${exprToString(r)})"
  
  case class Grade(subject: String, score: Int)
  case class Student(name: String, grades: List[Grade])
  
  enum Mention:
    case VeryGood, Good, Satisfactory, Passing, Insufficient
  
  def createStudent(name: String, grades: List[Grade]): Student =
    Student(name, grades)
  
  def isValidGrade(grade: Grade): Boolean =
    grade.score >= 0 && grade.score <= 100
  
  def studentAverage(student: Student): Double =
    if student.grades.isEmpty then 0.0
    else student.grades.map(_.score).sum.toDouble / student.grades.size
  
  def mentionForAverage(average: Double): Mention =
    if average >= 80 then Mention.VeryGood
    else if average >= 70 then Mention.Good
    else if average >= 60 then Mention.Satisfactory
    else if average >= 50 then Mention.Passing
    else Mention.Insufficient
  
  def studentMention(student: Student): Mention =
    mentionForAverage(studentAverage(student))
  
  def bestGrade(student: Student): Option[Grade] =
    student.grades.maxByOption(_.score)
  
  def gradesAboveAverage(student: Student): List[Grade] =
    val avg = studentAverage(student)
    student.grades.filter(_.score >= avg)
  
  def studentReport(student: Student): String =
    val avg = studentAverage(student)
    val mention = studentMention(student)
    val formattedAvg = f"$avg%.2f".replace(",", ".")
    s"Student: ${student.name}\nAverage: $formattedAvg\nMention: $mention"
  
  def betterStudent(s1: Student, s2: Student): String =
    if studentAverage(s2) > studentAverage(s1) then s2.name else s1.name
  
  def sortedGrades(student: Student): List[Grade] =
    student.grades.sortBy(-_.score)
  
  def bySubject(student: Student): Map[String, Int] =
    student.grades.map(g => g.subject -> g.score).toMap