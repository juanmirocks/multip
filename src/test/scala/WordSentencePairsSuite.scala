package multip

import org.scalatest.FunSuite

class RawSentencePairSuite extends FunSuite {
  import RawSentencePair._

  test("getBestCommonSubpart -- default by max length") {
    assert(Array("A") === getBestCommonSubpart(Array(Array("A"))))
    assert(Array("A", "B") === getBestCommonSubpart(Array(Array("A"), Array("A", "B"))))
    assert(Array("Ax", "Bx") === getBestCommonSubpart(Array(Array("A"), Array("A", "B"), Array("Ax", "Bx"))))
  }

  test("findCommonSubparts -- max length 2") {
    assert(Set(Seq("A"), Seq("B"), Seq("A", "B"), Seq("C"), Seq("B", "C"), Seq("D")) ===
      findCommonSubparts(
        Array("A", "B", "C", "D"),
        Array("X", "A", "B", "C", "X", "D", "X")
      ).map(_.toSeq)
    )

    assert(Set(Seq("A"), Seq("B"), Seq("A", "B"), Seq("C"), Seq("B", "C"), Seq("D"), Seq("X")) ===
      findCommonSubparts(
        Array("X", "A", "B", "C", "D"),
        Array("A", "B", "C", "X", "D", "X")
      ).map(_.toSeq)
    )
  }

  test("findCommonSubparts with empty parts") {
    assert(Set(Seq("A"), Seq("B"), Seq("C"), Seq("B", "C"), Seq("D")) ===
      findCommonSubparts(
        Array("A", "B", "C", "D"),
        Array("X", "A", "", "B", "C", "X", "D", "X")
      ).map(_.toSeq)
    )
  }

  test("findCommonSubparts -- stopwords") {
    assert(Set(Seq("A"), Seq("A", "B"), Seq("B"), Seq("B", "C"), Seq("C")) ===
      findCommonSubparts(Array("A", "B", "C"), Array("A", "B", "C"), stopWords = Set()).map(_.toSeq)
    )
    assert(Set(Seq("A"), Seq("C")) ===
      findCommonSubparts(Array("A", "B", "C"), Array("A", "B", "C"), stopWords = Set("B")).map(_.toSeq)
    )
  }
}
