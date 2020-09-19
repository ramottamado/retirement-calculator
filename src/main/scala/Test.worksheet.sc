case class Rectangle(width: Double, height: Double)

def area(r: Rectangle): Double = {
  if (r.width > 5 || r.height > 5) {
    throw new IllegalArgumentException("too big")
  } else {
    r.width * r.height
  }
}

val area1 = area(Rectangle(3, 2))
val area2 = area(Rectangle(4, 2))

val total =
  try {
    area(Rectangle(3, 2)) + area(Rectangle(6, 2))
  } catch {
    case e: IllegalArgumentException => 0
  }

val opt0: Option[Int] = None
// opt0: Option[Int] = None

val opt1: Option[Int] = Some(1)
// opt1: Option[Int] = Some(1)

val list0 = List.empty[String]
list0.headOption
// res0: Option[String] = None
list0.lastOption
// res1: Option[String] = None

val list3 = List("Hello", "World")
list3.headOption
// res2: Option[String] = Some(Hello)
list3.lastOption
// res3: Option[String] = Some(World)

def personDescription(name: String, db: Map[String, Int]): String =
  db.get(name) match {
    case Some(age) => s"$name is $age years old"
    case None => s"$name is not present in db"
  }

val db = Map("John" -> 25, "Rob" -> 40)
personDescription("John", db)
personDescription("Michael", db)

def averageAgeA(name1: String, name2: String, db: Map[String, Int]): Option[Double] = {
  val optOptAvg: Option[Option[Double]] =
    db.get(name1).map(age1 => db.get(name2).map(age2 => (age1 + age2).toDouble / 2))
  optOptAvg.flatten
}

val db1 = Map("John" -> 25, "Rob" -> 40)
averageAgeA("John", "Rob", db1)
averageAgeA("John", "Michael", db1)

def averageAgeB(name1: String, name2: String, db: Map[String, Int]): Option[Double] = {
  db.get(name1).flatMap(age1 => db.get(name2).map(age2 => (age1 + age2).toDouble / 2))
}

db.get("Poo")

averageAgeB("Poo", "Rob", db1)
averageAgeB("John", "Michael", db1)

def getPersonAge(name: String, db: Map[String, Int]): Either[String, Int] =
  db.get(name).toRight(s"$name is not present in db")

def averageAgeC(name1: String, name2: String, db: Map[String, Int]): Either[String, Double] = {
  for {
    age1 <- getPersonAge(name1, db)
    age2 <- getPersonAge(name2, db)
  } yield (age1 + age2).toDouble / 2
}

averageAgeC("John", "Rob", db1)
averageAgeC("John", "Michael", db1)

List(1, 2, 3, 4).foldRight(0) { (x, i) => x + i }

val lol: Either[String, Double] = Left("asd")
val lok: Either[String, Double] = Right(7) // Left("9")

val c = for {
  x <- lol
  y <- lok
} yield {
  x + y
}
