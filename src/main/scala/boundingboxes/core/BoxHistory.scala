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
  def apply(depth: Int): BoxHistory = BoxHistory(FixedSparseQueue(depth), 0, 0)
  private def apply(q: FixedSparseQueue[BoundingBox], i: Int, j: Int) =
    new BoxHistoryImpl(q, i, if (j > q.depth) 1 else j)
  
  private class BoxHistoryImpl(q: FixedSparseQueue[BoundingBox], i: Int, j: Int) extends BoxHistory {
    def :+ : BoxHistory = BoxHistory(q.enqueue, i, j + 1)
    def :+ (b: BoundingBox): BoxHistory = {
      b.addAsterisk(i, j)
      BoxHistory(q.enqueue(b), i, j + 1)
    }
    def newBox: (BoundingBox, BoxHistory) = {
    		val nb = new BoundingBox(i, j)
  			(nb, BoxHistory(q.enqueue(nb), i, j + 1))
    }
    def replacedContiguousBackBoxesWith(b: BoundingBox): BoxHistory =
      BoxHistory(q.replacedContigousBackBy(b, j), i, j)
    def up: Option[BoundingBox] = q.head
    def back: Option[BoundingBox] = q.last
  }
}