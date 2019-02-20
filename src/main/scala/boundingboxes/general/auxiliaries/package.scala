package boundingboxes.general

package object auxiliaries {
  implicit class RichVector[A](v: Vector[A]) {
    def removeAt[A](i: Int) = v.patch(i, Vector.empty, 1)
  }
}