import scala.io.Source

object Day4 {
  def splitArray[T](xs: Array[T], sep: T): List[Array[T]] = {
    var (res, i) = (List[Array[T]](), 0)

    while (i < xs.length) {
      var j = xs.indexOf(sep, i)
      if (j == -1) j = xs.length
      if (j != i) res ::= xs.slice(i, j)
      i = j + 1
    }

    res.reverse
  }
  
  def getValidPassports(passports: List[Array[String]]): List[Map[String, String]] = {
    val mandatory: Array[String] = Array("ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt")
    
    passports
      .map(_.flatMap(line => line.split(" ")
        .map(value => value.split(":"))
        .map(value => (value(0), value(1))))
        .toMap
      )
      .filter(dict => mandatory.forall(key => dict.contains(key)))
  }

  def part1(passports: List[Array[String]]): Unit = {
    print(getValidPassports(passports).length)
  }
  
  def checkYear(value: String, min: Int, max: Int): Boolean = {
    try {
      val nr = value.toInt
      nr >= min && nr <= max
    }
    catch {
      case _ => false
    }
  }
  
  def checkHeight(value: String): Boolean = {
    if (value.endsWith("cm")) {
      val nr = value.substring(0, value.length - 2).toInt
      nr >= 150 && nr <= 193
    }
    else if(value.endsWith("in")) {
      val nr = value.substring(0, value.length - 2).toInt
      
      nr >= 59 && nr <= 76
    }
    else {
      false
    }
  }
  
  def checkHairColor(value: String): Boolean = {
    if (value.startsWith("#") && value.length == 7) {
      value.substring(1).forall(ch => ch.isDigit || (ch >= 'a' && ch <= 'f'))
    }
    else {
      false
    }
  }
  
  def part2(passports: List[Array[String]]): Unit = {
    val validEye = "amb blu brn gry grn hzl oth".split(" ")
    val validPassports: List[Map[String, String]] =
      getValidPassports(passports)
        .filter(passport => 
          checkYear(passport("byr"), 1920, 2002) &&
          checkYear(passport("iyr"), 2010, 2020) &&
          checkYear(passport("eyr"), 2020, 2030) &&
          checkHeight(passport("hgt")) &&
          checkHairColor(passport("hcl")) &&
          validEye.contains(passport("ecl")) &&
          passport("pid").forall(_.isDigit) && passport("pid").length == 9
        )
        
    println(validPassports.length)
  }
  
  def main(args: Array[String]): Unit = {
    val passports: List[Array[String]] = splitArray(Source.fromResource("day4").getLines.map(_.trim).toArray, "")
    
    part2(passports)
  }
}
