package boundingboxes.core

import annotation.tailrec

trait BoxHistory {
  def :+ : BoxHistory
  def :+ (b: BoundingBox): BoxHistory
  def newBox: (BoundingBox, BoxHistory)
  def replacedContiguousBackBoxesWith(b: BoundingBox): BoxHistory
  def appendRow: BoxHistory
  def up: Option[BoundingBox]
  def back: Option[BoundingBox]
}

object BoxHistory {
  def empty[A]: BoxHistory = new BoxHistoryImpl(Map.empty, Map.empty, 0, 0)
  
  private class BoxHistoryImpl(priorRow: Map[Int, BoundingBox], currentRow: Map[Int, BoundingBox], i: Int, j: Int) extends BoxHistory {
    def :+ : BoxHistory = new BoxHistoryImpl(priorRow, currentRow, i, j + 1)
    def :+ (b: BoundingBox): BoxHistory = {
      b.addAsterisk(i, j)
      new BoxHistoryImpl(priorRow, currentRow + (j -> b), i, j + 1)
    }
    def replacedContiguousBackBoxesWith(b: BoundingBox): BoxHistory = {
      @tailrec
      def loop(j0: Int, row: Map[Int, BoundingBox]): Map[Int, BoundingBox] = {
        if (row.contains(j0)) loop(j0 - 1, row.updated(j0, b))
        else row
      }
      new BoxHistoryImpl(priorRow, loop(j - 1, currentRow), i, j)
    }
    def newBox: (BoundingBox, BoxHistory) = {
      val nb = new BoundingBox(i, j)
      (nb, new BoxHistoryImpl(priorRow, currentRow + (j -> nb), i, j + 1))
    }
    def appendRow: BoxHistory = new BoxHistoryImpl(currentRow, Map.empty, i + 1, 1)
    def up: Option[BoundingBox] = priorRow.get(j)
    def back: Option[BoundingBox] = currentRow.get(j - 1)
  }
}