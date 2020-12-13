import scala.io.Source

object Day13 {
  def part1(timestamp: Int, buses: Array[Int]): Unit = {
    val min = buses.filter(_ != -1).map(x => (x, (timestamp + x - 1) / x * x - timestamp)).sortBy(_._2)
    
    println(min(0)._1 * min(0)._2)
  }
  
  def gcd(a: Long, b: Long, x0: Long, x1: Long): Long = if (a > 1) then gcd(b, a % b, x1 - (a / b) * x0, x0) else x1
  
  def invMod(a: Long, b: Long): Long = {
    if (b == 1) {
      1
    }
    else {
      val x1 = gcd(a, b, 0, 1)

      if (x1 < 0) {
        x1 + b
      }
      else {
        x1
      }
    }
  }

  def chineseRemainder(modulos: Array[Long], reminders: Array[Long]): Long = {
    val product = modulos.product

    def iter(modulos: Array[Long], reminders: Array[Long], sm: Long): Long = {
      if (modulos.nonEmpty) {
        val p = product / modulos.head

        iter(modulos.tail, reminders.tail, sm + reminders.head * invMod(p, modulos.head) * p)
      }
      else {
        sm
      }
    }

    iter(modulos, reminders, 0) % product
  }
  
  def part2(buses: Array[Int]): Unit = {
    val modulos: Array[Long] = buses.filter(_ != -1).map(_.toLong)
    var reminders: Array[Long] = buses.zipWithIndex.filter(x => x._1 != -1).map(x => x._1 - x._2).map(_.toLong)
    reminders(0) = 0

    println(chineseRemainder(modulos, reminders))
  }
  
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day13").getLines.toArray
    val timestamp: Int = input(0).toInt
    val buses: Array[Int] = input(1).split(",").map(x => {
      if (x.forall(_.isDigit)) {
        x.toInt
      }
      else {
       -1
      }
    }).toArray
    
    part2(buses)
  }
}
