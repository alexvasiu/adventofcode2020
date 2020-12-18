import scala.io.Source
import scala.collection.mutable.Stack

object Day18 {
  def precedence1(op: Char): Int = if (op == '(') then 0 else 1

  def precedence2(op: Char): Int = if (op == '+' || op == '-') then 2 else precedence1(op)

  def apply(a: Long, b: Long, op: Char): Long = {
    op match {
      case '+' => b + a
      case '-' => b - a
      case '*' => b * a
      case '/' => b / a
    }
  }

  def evalExpression(expr: String, precedence: Char => Int): Long = {
    var values: Stack[Long] = Stack()
    var ops: Stack[Char] = Stack()
    var i: Int = 0

    while (i < expr.length) {
      if (expr(i) != ' ') {
        if (expr(i) == '(') {
          ops.push('(')
        }
        else if (expr(i).isDigit) {
          var value: Long = 0

          while (i < expr.length && expr(i).isDigit) {
            value = value * 10 + expr(i).toString.toLong
            i += 1
          }

          values.push(value)
          i -= 1
        }
        else if (expr(i) == ')') {
          while (ops.top != '(') {
            values.push(apply(values.pop, values.pop, ops.pop))
          }

          ops.pop
        }
        else {
          while (ops.nonEmpty && precedence(ops.top) >= precedence(expr(i))) {
            values.push(apply(values.pop, values.pop, ops.pop))
          }

          ops.push(expr(i))
        }
      }
      i += 1
    }

    while (ops.nonEmpty) {
      values.push(apply(values.pop, values.pop, ops.pop))
    }

    values.top
  }

  def part1(expressions: Array[String]): Unit = {
    println(expressions.map(evalExpression(_, precedence1)).sum)
  }

  def part2(expressions: Array[String]): Unit = {
    println(expressions.map(evalExpression(_, precedence2)).sum)
  }

  def main(args: Array[String]): Unit = {
    val expressions: Array[String] = Source.fromResource("day18").getLines.map(_.trim).toArray

    part2(expressions)
  }
}