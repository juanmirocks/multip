package multip;


import scala.util.Random;

import breeze.linalg._
import breeze.math._
import breeze.numerics._


// This Eval(uation) object is mainly used for monitoring the performance of trained model at each iteration.
// The final evaluation of the system performance (that reproduces the result we reported in our TACL paper)
// uses the EvalIterations function in Main.scala; not this Eval object.
object Eval {

  // Evaluating only on binary outputs, using precision / recall / F1
  def aggregateEval(params: Seq[Parameters], evals: Seq[SentPairsData], useAveragedParameters: Boolean) {
    require(params.size == evals.size && params.nonEmpty)

    if (Constants.TIMING) {
      Utils.Timer.start("AggregateEval")
    }

    var totalPositiveParaphrases = 0.0	//For computing fn
    var sortedPredictions = List[Prediction]()

    params.zip(evals).foreach { case (param, eval) =>
      for (ep <- Random.shuffle(eval.data.toList)) {
        val predicted = param.inferAll(ep, useAveragedParameters)

        val goldlabel = ep.rel(param.data.IS_PARAPHRASE)
        val prediction = predicted.rel(param.data.IS_PARAPHRASE)

        if (goldlabel == 1.0) {
          totalPositiveParaphrases += 1.0
        }

        if (goldlabel == 1.0 && prediction == 1.0) {
          sortedPredictions ::= new Prediction(max(predicted.zScore(predicted.z :== param.data.IS_PARAPHRASE)), true)
        }
        else if (goldlabel == 0.0 && prediction == 1.0) {
          sortedPredictions ::= new Prediction(max(predicted.zScore(predicted.z :== param.data.IS_PARAPHRASE)), false)
        }
      }
    }

    println("# of sentence pairs: " + evals.foldLeft(0) { (c, e) => c + e.data.size })
    printPR(sortedPredictions, totalPositiveParaphrases)

    if (Constants.TIMING) {
      Utils.Timer.stop("AggregateEval")
    }
  }

  def aggregateEval(param: Parameters, eval: SentPairsData, useAveragedParameters: Boolean) {
    aggregateEval(List(param), List(eval), useAveragedParameters)
  }

  //This evaluation try to use 3 different points to characterize the Precision/Recall curve:
  //   max F1 point
  //   max Precision point (and recall > 0.05)
  //   max Recall point (and precision > 0.5
  //This function is mainly used to monitor the training process, showing the model performance at each iteration
  def printPR(sortedPredictions: List[Prediction], maxResults: Double) {
    var tp, fp, fn = 0.0

    var maxF,  maxFp, maxFr = 0.0
    var maxP,  maxPr, maxPf = 0.0
    var maxRp, maxR,  maxRf = 0.0

    for (prediction <- sortedPredictions.sortBy(-_.score)) {
      if (prediction.correct) {
        tp += 1.0
      } else {
        fp += 1.0
      }

      fn = maxResults - tp
      val p = tp / (tp + fp)
      val r = tp / (tp + fn)
      val f = 2 * p * r / (p + r)

      if (f > maxF) {
        maxF  = f
        maxFp = p
        maxFr = r
      }

      if (r > 0.05 && p > maxP) {
        maxP  = p
        maxPr = r
        maxPf = f
      }

      if (r > maxR && p > 0.5) {
        maxR  = r
        maxRp = p
        maxRf = f
      }
    }

    fn = maxResults - tp
    val p = tp / (tp + fp)
    val r = tp / (tp + fn)
    val f = 2 * p * r / (p + r)

    println("# of _actual_ paraphrases: " + maxResults.toInt)
    println("# of _predicted_ paraphrases: " + sortedPredictions.length)
    println("normal; P:" + p     + "\tR:" + r     + "\tF:" + f)
    println("max. F; P:" + maxFp + "\tR:" + maxFr + "\tF:" + maxF)
    println("max. P; P:" + maxP  + "\tR:" + maxPr + "\tF:" + maxPf)
    println("max. R; P:" + maxRp + "\tR:" + maxR  + "\tF:" + maxRf)
  }

  class Prediction(val score: Double, val correct: Boolean, val rel: String, val annotated_sentence: String) {

    def this(score: Double, correct: Boolean) = this(score, correct, null, null)

  }

}
