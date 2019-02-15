package boundingboxes.core

import boundingboxes.general.Point
import math.{ max, min }
import BoundingBox._

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
  private[core] def area = length(xf) * length(yf)
  private[core] def absorb(absorbed: BoundingBox) =
    absorbed.asterisks.foreach { p =>
      addAsterisk(p.x, p.y)
    }
  private[core] def is2d = hasDepth(xf) && hasDepth(yf)
  private[core] def overlaps(that: BoundingBox) = {
    def between(coord: Point => Int) = {
     def le(l: Point, r: Point) = coord(l) <= coord(r)
     def in(ends: BoundingBox, mid: BoundingBox) = {
        val mtl = mid.topLeft
        le(ends.topLeft, mtl) && le(mtl, ends.bottomRight)
      }
      in(this, that) || in(that, this)
    }
    between(xf) && between(yf)
  }
  private def hasDepth(coord: Point => Int) = coord(bottomRight) - coord(topLeft) > 0
  private def length(coord: Point => Int) = coord(bottomRight) - coord(topLeft) + 1
  override def toString = topLeft.toString + bottomRight.toString
}

private object BoundingBox {
  private val xf: Point => Int = _.x
  private val yf: Point => Int = _.y
}