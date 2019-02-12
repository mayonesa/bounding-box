package boundingboxes.core

import annotation.tailrec
import boundingboxes.general.FixedSparseQueue

trait BoxHistory {
  def :+ : BoxHistory
  def :+ (b: BoundingBox): BoxHistory
  def newBox: (BoundingBox, BoxHistory)
  def replacedContiguousBackBoxesWith(b: BoundingBox): BoxHistory
  def up: Option[BoundingBox]
  def back: Option[BoundingBox]
}

object BoxHistory {
  def apply(depth: Int): BoxHistory = BoxHistory(FixedSparseQueue(depth), 1, 1)
  private def apply(q: FixedSparseQueue[BoundingBox], i: Int, j: Int) = new BoxHistoryImpl(q, i,  j)
  
  private class BoxHistoryImpl(q: FixedSparseQueue[BoundingBox], i: Int, j: Int) extends BoxHistory {
    def :+ : BoxHistory = bh(q.enqueue, j + 1)
    def :+ (b: BoundingBox): BoxHistory = {
      b.addAsterisk(i, j)
      bh(q.enqueue(b), j + 1)
    }
    def newBox: (BoundingBox, BoxHistory) = {
      val nb = new BoundingBox(i, j)
      (nb, bh(q.enqueue(nb), j + 1))
    }
    def replacedContiguousBackBoxesWith(b: BoundingBox): BoxHistory = bh(q.replacedContigousBackBy(b, j), j)
    def up: Option[BoundingBox] = q.head
    def back: Option[BoundingBox] = q.last

    private def bh(q0: FixedSparseQueue[BoundingBox], j0: Int) =
      if (j0 > q.depth) BoxHistory(q0, i + 1, 1)
      else BoxHistory(q0, i, j0)
  }
}