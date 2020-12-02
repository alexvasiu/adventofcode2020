import scala.io.Source

case class Password(min: Int, max: Int, ch: Char, password: String) {
  def isValid: Boolean = {
    val count = password.count(c => c == ch)
    min <= count && count <= max
  }
  
  def isValid2: Boolean = {
    (password(min - 1) == ch && password(max - 1) != ch) || (password(min - 1) != ch && password(max - 1) == ch)
  }
}

object Day2 {
  def part1(passwords: Array[Password]): Unit = {
    println(passwords.count(_.isValid))
  }
  
  def part2(passwords: Array[Password]): Unit = {
    println(passwords.count(_.isValid2))
  }
  
  def main(args: Array[String]): Unit = {
    val passwords: Array[Password] = Source.fromResource("day2").getLines().map(Password(_)).toArray
    
    part2(passwords)
  }
}

object Password {
  def apply(line: String): Password = {
    val values: Array[String] = line.trim.split(" ")
    val minMax: Array[String] = values(0).split("-")
    val min: Int = minMax(0).toInt
    val max: Int = minMax(1).toInt
    val ch: Char = values(1)(0)
    
    Password(min, max, ch, values(2))
  }
}