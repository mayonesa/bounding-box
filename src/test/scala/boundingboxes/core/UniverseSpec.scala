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
}