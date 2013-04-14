package data.general

import scalaz.Equal

case class Point( x: Int, y: Int )

object Point {
  implicit val pointEq = Equal.equalA[Point]
}
