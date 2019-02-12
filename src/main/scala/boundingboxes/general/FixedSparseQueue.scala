package boundingboxes.general

import annotation.tailrec

trait FixedSparseQueue[A] {
  def enqueue: FixedSparseQueue[A]
  def enqueue(a: A): FixedSparseQueue[A]
  def replacedContigousBackBy(a: A, limit: Int): FixedSparseQueue[A]
  def head: Option[A]
  def last: Option[A]
  def depth: Int
}

object FixedSparseQueue {
  def apply[A](depth: Int): FixedSparseQueue[A] = FixedSparseQueue(Map.empty, 0, depth)
  private def apply[A](m: Map[Int, A], i: Int, depth: Int): FixedSparseQueue[A] =
    new FixedSparseQueueImpl(m, i, depth)

  private class FixedSparseQueueImpl[A](m: Map[Int, A], i: Int, val depth: Int) extends FixedSparseQueue[A] {
    def enqueue: FixedSparseQueue[A] = fsq(m - nextI, nextI)
    def enqueue(a: A): FixedSparseQueue[A] = fsq(m - nextI + (i -> a), i + 1)
    def replacedContigousBackBy(a: A, limit: Int): FixedSparseQueue[A] = {
      @tailrec
      def loop(i0: Int, m0: Map[Int, A]): Map[Int, A] = {
        if (m0.contains(i0)) loop(i0 - 1, m0.updated(i0, a))
        else m0
      }
      fsq(loop(i, m), i)
    }
    def head: Option[A] = m.get(i)
    def last: Option[A] = m.get(nextI)

    private def nextI = if (i == depth) 0 else i
    private def fsq(m0: Map[Int, A], i: Int) = FixedSparseQueue(m0, i, depth)
  }
}