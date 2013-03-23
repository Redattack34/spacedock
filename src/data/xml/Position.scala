package data.xml

import com.codecommit.antixml._

case class Position(x: Int, y: Int)

object Position {

  def positions(e: Elem) : Seq[Position] = for {
    pos <- e \ 'Position
    x <- pos \ 'X \ text
    y <- pos \ 'Y \ text
  } yield Position(x.toInt, y.toInt)

  def toXml( pos: Position ) : Elem = {
    val x = Elem(None, "X", Attributes.empty, Map.empty, Group(Text(pos.x.toString)))
    val y = Elem(None, "Y", Attributes.empty, Map.empty, Group(Text(pos.y.toString)))
    Elem(None, "Position", Attributes.empty, Map.empty,  Group(x, y))
  }
}