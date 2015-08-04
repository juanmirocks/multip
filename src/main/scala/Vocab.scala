package multip;

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

// Vocabulary class maps strings to integers for efficiency
class Vocab(minCount: Int = 1) {

  var string2Int   = new HashMap[String, Int]
  var string2Count = new HashMap[String, Int]
  var int2String   = new HashMap[Int, String]
  private val unk = -1
  private var nextInt = 0
  private var locked = false

  def apply(s: String): Int = {

    //if(!string2Int.contains(s) && locked) {
    if((!string2Int.contains(s) || string2Count(s) < minCount) && locked) {
      return -1  //UNK is -1
    }
    else if(!string2Int.contains(s)) {
      string2Int   += s -> nextInt
      int2String   += nextInt -> s
      string2Count += s -> 0
      nextInt += 1
    }
    string2Count(s) += 1
    return string2Int(s)
  }

  def apply(i: Int): String = {
    int2String.get(i) match {
      case None => "-UNK-"
      case Some(x) => x
    }
  }

  def lock: Vocab = {
    locked = true
    this
  }

  def getMinCountVocab = {
    //locked = true

    val newVocab = new Vocab(minCount)
    for ((word, count) <- string2Count) {
      if (count >= minCount) {
        newVocab(word)
        newVocab.string2Count(word) = count
      }
    }

    newVocab.locked = true

    //this
    newVocab
  }

  def size() = nextInt
}
