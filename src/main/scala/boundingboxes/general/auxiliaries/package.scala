package boundingboxes.general

package object auxiliaries {
  def removeAt[A](i: Int, v: Vector[A]) = v.patch(i, Vector.empty, 1)
}