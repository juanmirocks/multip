package multip;

import scala.collection.mutable.HashMap

object Vocab {

  //Unknown, so called in original code
  val UNK_INT = -1
  val UNK_STR = "-UNK-"

}

// Vocabulary class maps strings to integers for efficiency
class Vocab(minCount: Int = 1) {

  private val string2Int   = new HashMap[String, Int]
  private val string2Count = new HashMap[String, Int]
  private val int2String   = new HashMap[Int, String]
  private var nextInt = 0
  private var locked = false

  def apply(s: String): Int = {
    if (locked) {
      string2Int.get(s) match {
        case Some(i) if string2Count(s) >= minCount => i
        case _ => Vocab.UNK_INT
      }
    }
    else {
      string2Int.get(s) match {
        case Some(i) => {
          string2Count(s) += 1
          i
        }
        case None => {
          val ret = nextInt
          string2Int   += s -> ret
          int2String   += nextInt -> s
          string2Count += s -> 1
          nextInt += 1
          ret
        }
      }
    }
  }

  def apply(i: Int): String = {
    int2String.get(i) match {
      case None => Vocab.UNK_STR
      case Some(x) => x
    }
  }

  def lock(): Vocab = {
    locked = true
    this
  }

  def isLocked(): Boolean =
    locked

  def getMinCountVocab: Vocab = {
    //locked = true

    val newVocab = new Vocab(minCount)
    for ((word, count) <- string2Count) {
      if (count >= minCount) {
        newVocab(word)
        newVocab.string2Count(word) = count
      }
    }

    newVocab.locked = true

    newVocab
  }

  def size() = nextInt
}
