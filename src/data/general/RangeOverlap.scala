package data.general

object RangeOverlap {

  class RangeOverlap( r1 : Range )  {
    def overlapsWith (r2: Range) : Boolean = {
      r1.exists( (n : Int) => r2.contains(n) )
    }
  }

  implicit def range2Overlap( r: Range ) = new RangeOverlap( r )
}