import scala.io.Source

object Day17 {
  val directions: Array[Int] = Array(-1, 0, 1)
  
  def neighbours(x: Int, y: Int, z: Int): Set[(Int, Int, Int)] = {
    var n: Set[(Int, Int, Int)] = Set()
    
    for (dx <- directions) {
      for (dy <- directions) {
        for (dz <- directions) {
          if (!(dx == 0 && dy == 0 && dz == 0)) {
            n += (x + dx, y + dy, z + dz)
          }
        }
      }
    }
    
    n
  }
  
  def neighbours(x: Int, y: Int, z: Int, w: Int): Set[(Int, Int, Int, Int)] = {
    var n: Set[(Int, Int, Int, Int)] = Set()

    for (dx <- directions) {
      for (dy <- directions) {
        for (dz <- directions) {
          for (dw <- directions) {
            if (!(dx == 0 && dy == 0 && dz == 0 && dw == 0)) {
              n += (x + dx, y + dy, z + dz, w + dw)
            }
          }
        }
      }
    }

    n
  } 
  
  def neighbours[T](element: T): Set[T] = {
    if (element.isInstanceOf[(Int, Int, Int)]) {
      var value = element.asInstanceOf[(Int, Int, Int)]
      neighbours(value._1, value._2, value._3).asInstanceOf[Set[T]]
    }
    else {
      var value = element.asInstanceOf[(Int, Int, Int, Int)]
      neighbours(value._1, value._2, value._3, value._4).asInstanceOf[Set[T]]
    }
  }
  
  def countn[T](cubes: Set[T], element: T): Int = {
    neighbours(element).count(cubes.contains(_))
  }
  
  def step[T](cubes: Set[T]): Set[T] = {
    var newCubes: Set[T] = Set()
    
    for (p <- cubes) {
      for (neighbour <- neighbours(p)) {
        if (!cubes.contains(neighbour) && countn(cubes, neighbour) == 3) {
          newCubes += neighbour
        }
      }

      if (Array(2, 3).contains(countn(cubes, p))) {
        newCubes += p
      }
    }

    newCubes
  }
  
  def getCubes[T](values: Array[Array[Char]], dim: Int): Set[T] = {
    var cubes: Set[T] = Set()

    for (i <- 0 until values.length) {
      for (j <- 0 until values(0).length) {
        if (values(i)(j) == '#') {
          cubes += (if (dim == 3) then (i, j, 0) else (i, j, 0, 0)).asInstanceOf[T]
        }
      }
    }
    
    cubes
  }
  
  def part[T](values: Array[Array[Char]], dim: Int): Unit = {
    var cubes: Set[T] = getCubes(values, dim)
    
    for (i <- 0 until 6) {
      cubes = step(cubes)
    }
    
    println(cubes.size)
  }
  
  def main(args: Array[String]): Unit = {
    val values: Array[Array[Char]] = Source.fromResource("day17").getLines.map(_.toArray).toArray
    
    part(values, 3)
    part(values, 4)
  }
}