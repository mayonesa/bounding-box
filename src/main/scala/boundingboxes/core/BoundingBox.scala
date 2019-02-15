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
    def f(newPoint: Point => Unit, corner: Point, comp: (Int, Int) => Boolean, minMax: (Int, Int) => Int) = {
      val cx = corner.x
      val cy = corner.y
      if (comp(x, cx)) {
        newPoint(Point(x, minMax(y, cy)))
      } else if (comp(y, cy)) {
        newPoint(Point(cx, y))
      }
    }
    f(topLeft = _, topLeft, _ < _, min)
    f(bottomRight = _, bottomRight, _ > _, max)
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