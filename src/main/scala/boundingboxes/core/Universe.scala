package boundingboxes.core

import annotation.tailrec
import language.postfixOps
import boundingboxes.general.auxiliaries.removeAt

trait Universe {
  def appendDash: Universe
  def appendAsterisk: Universe
  def appendRow: Universe
  def largestNonoverlappingBoxes: Set[BoundingBox]
}

object Universe {
  def empty: Universe = Universe(BoxHistory.empty, Set.empty)
  private def apply(h: BoxHistory, bs: Set[BoundingBox]) = new UniverseImpl(h, bs)

  private class UniverseImpl(h: BoxHistory, bs: Set[BoundingBox]) extends Universe {
    def appendDash: Universe = Universe(h :+, bs)
    def appendAsterisk: Universe = {
      val upOpt = h.up
      h.back match {
        case Some(backBox) =>
          lazy val useBack = Universe(h :+ backBox, bs)
          upOpt match {
            case Some(upBox) =>
              if (backBox == upBox) useBack
              else Universe(merge(backBox, upBox) :+ upBox, bs - backBox)
            case None => useBack
          }
        case None =>
          upOpt match {
            case Some(upBox) => Universe(h :+ upBox, bs)
            case None =>
              val (newBox, newHist) = h.newBox
              Universe(newHist, bs + newBox)
          }
      }
    }
    def appendRow: Universe = Universe(h.appendRow, bs)
    def largestNonoverlappingBoxes: Set[BoundingBox] = largests(nonoverlapping(real))
    override def toString = bs.mkString("\n")

    private def real = bs.filter(_.is2d)
    private def merge(absorbed: BoundingBox, into: BoundingBox) = {
      into.absorb(absorbed)
      h.replacedContiguousBackBoxesWith(into)
    }
  }

  private def nonoverlapping(boxes: Set[BoundingBox]) = {
    def loop1(bs: Vector[BoundingBox], overlaps: Set[BoundingBox]): Set[BoundingBox] =
      if (bs.isEmpty) overlaps
      else {
        val primary = bs.head
        val secondaries = bs.tail
        @tailrec
        def loop0(j: Int): Set[BoundingBox] =
          if (j == secondaries.length) loop1(secondaries, overlaps)
          else {
            val secondary = secondaries(j)
            if (secondary.overlaps(primary)) 
              loop1(removeAt(j, secondaries), overlaps + primary + secondary)
            else loop0(j + 1)
          }
        loop0(0)
      }
    val overlapping = loop1(boxes.toVector, Set.empty)
    boxes -- overlapping
  }
  private def largests(boxes: Set[BoundingBox]) =
    boxes.foldLeft(Set.empty[BoundingBox]) { (acc, b) =>
      if (acc.isEmpty || b.area > acc.head.area) Set(b)
      else if (b.area < acc.head.area) acc
      else acc + b
    }
}