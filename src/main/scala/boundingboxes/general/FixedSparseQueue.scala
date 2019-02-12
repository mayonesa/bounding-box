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
    def enqueue: FixedSparseQueue[A] = fsq(m - i, nextI)
    def enqueue(a: A): FixedSparseQueue[A] = fsq(m + (i -> a), nextI)
    def replacedContigousBackBy(a: A, limit: Int): FixedSparseQueue[A] = {
      @tailrec
      def loop(i0: Int, m0: Map[Int, A]): Map[Int, A] = {
        val nIters = circ((i - i0) + depth)
        if (nIters <= limit && m0.contains(i0)) loop(prev(i0), m0.updated(i0, a))
        else m0
      }
      fsq(loop(prev(i), m), i)
    }
    def head: Option[A] = m.get(circ(i - 1))
    def last: Option[A] = m.get(i)

    private def prev(i0: Int) = circ(i0 - 1)
    private def nextI = circ(i + 1)
    private def circ(i0: Int) = i0 % depth
    private def fsq(m0: Map[Int, A], i: Int) = FixedSparseQueue(m0, i, depth)
  }
}