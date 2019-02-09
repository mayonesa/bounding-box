package boundingboxes.general

case class Point(x: Int, y: Int) {
  override def toString: String = s"($x,$y)"
}