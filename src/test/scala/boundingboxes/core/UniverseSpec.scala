package boundingboxes.core

import org.scalatest.FlatSpec

class UniverseSpec extends FlatSpec {
  "sample provided" should "return top-left bounding box" in {
    val row1 = Universe.empty.appendRow.appendAsterisk.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk
    val row12 = row1.appendRow.appendDash.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk
    val bug = row12.appendAsterisk.appendAsterisk.appendDash
    val row123 = bug.appendRow.appendDash.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk
    val row1234 = row123.appendRow.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash
    val expBox = new BoundingBox(1, 1)
    expBox.addAsterisk(1, 2)
    expBox.addAsterisk(2, 2)
    assert(row1234.largestNonoverlappingBoxes.map(_.toString) === Set(expBox.toString))
  }
  "single asterisks" should "have no effect" in {
    val row1 = Universe.empty.appendRow.appendAsterisk.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk
    val row12 = row1.appendRow.appendDash.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash
    val row123 = row12.appendRow.appendDash.appendDash.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk
    val row1234 = row123.appendRow.appendDash.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash
    val expBox1 = new BoundingBox(1, 1)
    expBox1.addAsterisk(1, 2)
    expBox1.addAsterisk(2, 2)
    assert(row1234.largestNonoverlappingBoxes.map(_.toString) === Set(expBox1.toString))
  }
  "multiple largests" should "be multiple" in {
    val row1 = Universe.empty.appendRow.appendAsterisk.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk
    val row12 = row1.appendRow.appendDash.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash
    val row123 = row12.appendRow.appendDash.appendDash.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk
    val row1234 = row123.appendRow.appendDash.appendAsterisk.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash
    val expBox1 = new BoundingBox(1, 1)
    expBox1.addAsterisk(1, 2)
    expBox1.addAsterisk(2, 2)
    val expBox2 = new BoundingBox(3, 3)
    expBox2.addAsterisk(4, 2)
    expBox2.addAsterisk(4, 3)
    assert(row1234.largestNonoverlappingBoxes.map(_.toString) === Set(expBox1.toString, expBox2.toString))
  }
  "0 nonoverlappings" should "come back empty" in {
    val row1 = Universe.empty.appendRow.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk
    val row12 = row1.appendRow.appendDash.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash
    val row123 = row12.appendRow.appendDash.appendDash.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk
    val row1234 = row123.appendRow.appendDash.appendDash.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash
    assert(row1234.largestNonoverlappingBoxes.isEmpty)
  }
  "tear design" should "capture box" in {
    val row1 = Universe.empty.appendRow.appendDash.appendAsterisk.appendAsterisk.appendAsterisk
    val row12 = row1.appendRow.appendDash.appendAsterisk.appendDash.appendAsterisk
    val expBox = new BoundingBox(1, 2)
    expBox.addAsterisk(1, 3)
    expBox.addAsterisk(1, 4)
    expBox.addAsterisk(2, 2)
    expBox.addAsterisk(2, 4)
    assert(row12.largestNonoverlappingBoxes.head.toString === expBox.toString)
  }
  "adjacent boxes" should "not overlap" in {
    val row1 = Universe.empty.appendRow.appendAsterisk.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk
    val row12 = row1.appendRow.appendDash.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash
    val row123 = row12.appendRow.appendDash.appendDash.appendAsterisk.appendAsterisk.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash.appendAsterisk.appendAsterisk
    val row1234 = row123.appendRow.appendDash.appendAsterisk.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendDash
    val expBox = new BoundingBox(3, 3)
    expBox.addAsterisk(4, 2)
    expBox.addAsterisk(4, 3)
    expBox.addAsterisk(3, 4)
    assert(row1234.largestNonoverlappingBoxes.map(_.toString) === Set(expBox.toString))
  }
  "long line and dip" should "not show up and be complete, respectively" in {
    val row1 = Universe.empty.appendRow.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendAsterisk.appendAsterisk.appendDash.appendDash
    val row12 = row1.appendRow.appendDash.appendAsterisk.appendDash.appendDash.appendDash.appendDash.appendAsterisk.appendAsterisk.appendAsterisk.appendDash.appendAsterisk.appendDash
    val row123 = row12.appendRow.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash.appendDash
    val row1234 = row123.appendRow.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk.appendAsterisk
    val expBox = new BoundingBox(1, 5)
    expBox.addAsterisk(1, 6)
    expBox.addAsterisk(1, 7)
    expBox.addAsterisk(1, 9)
    expBox.addAsterisk(1, 10)
    expBox.addAsterisk(2, 7)
    expBox.addAsterisk(2, 8)
    expBox.addAsterisk(2, 9)
    assert(row1234.largestNonoverlappingBoxes.map(_.toString) === Set(expBox.toString))
  }
}