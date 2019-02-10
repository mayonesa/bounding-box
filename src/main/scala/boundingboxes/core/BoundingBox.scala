package boundingboxes.core

import boundingboxes.general.Point
import math.{ max, min }

private[core] class BoundingBox(x: Int, y: Int) {
  private val p =  Point(x, y)
  private var topLeft = p
  private var bottomRight = p
  private var asterisks = Set(p)
  
  private[core] def addAsterisk(x: Int, y: Int) = {
		asterisks = asterisks + Point(x, y)
		if (x < topLeft.x) {
		  topLeft = Point(x, min(y, topLeft.y))
		} else if (y < topLeft.y) {
		  topLeft = Point(topLeft.x, y)
		}
		if (x > bottomRight.x) {
		  bottomRight = Point(x, max(y, bottomRight.y))
		} else if (y > bottomRight.y) {
		  bottomRight = Point(bottomRight.x, y)
    }
  }
  private[core] def area = (bottomRight.x - topLeft.x + 1) * (bottomRight.y - topLeft.y + 1)
  private[core] def absorb(absorbed: BoundingBox) =
    absorbed.asterisks.foreach { p =>
      addAsterisk(p.x, p.y)
    }
  private[core] def is2d = bottomRight.x - topLeft.x > 0 && bottomRight.y - topLeft.y > 0
  private[core] def overlaps(that: BoundingBox) = {
    def between(coord: Point => Int) = {
      def in(ends: BoundingBox, mid: BoundingBox) = {
        val midCoord = coord(mid.topLeft)
        coord(ends.topLeft) <= midCoord && midCoord <= coord(ends.bottomRight)
      }
      in(this, that) || in(that, this)
    }
    between(_.x) && between(_.y)
  }
  override def toString = topLeft.toString + bottomRight.toString
}