package multip

import multip.feature._
import scala.math.exp

import edu.washington.cs.knowitall.morpha._
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import collection.immutable.ListMap

import breeze.linalg._
import breeze.numerics._
import java.io.File

object Constants {
  var DEBUG = false
  var TIMING = true
}

object Main extends App {

  case class Config(
    numIter: Int = 50,
    runTests: Set[String] = Set[String](),
    trainFile: File = null,
    trainUseExpert: Boolean = false,
    evalFile: File = null,
    evalUseExpert: Boolean = true,
    numCV: Int = 10,
    useAveragedParameters: Boolean = true
  )

  val parser = new scopt.OptionParser[Config]("multip") {
    head("multip", "0.2")
    opt[Int]('i', "iterations") action { (x, c) => c.copy(numIter = x) } text("# iterations for training")
    opt[Set[String]]('t', "tests")(scopt.Read.reads(_.split(",").toSet)) action { (x, c) => c.copy(runTests = x) } text("final tests to run as in original multip code")
    opt[File]("trainFile") required() valueName("<file>") action { (x, c) => c.copy(trainFile = x) }
    opt[Boolean]("trainUseExpert") action { (x, c) => c.copy(trainUseExpert = x) }
    opt[File]("evalFile") valueName("<file>") action { (x, c) => c.copy(evalFile = x) } text("OPTIONAL if not given, CV training is performed")
    opt[Boolean]("evalUseExpert") action { (x, c) => c.copy(evalUseExpert = x) }
    opt[Int]("cv") action { (x, c) => c.copy(numCV = x) } text("# of CVs")
    opt[Boolean]("useAveragedParameters") action { (x, c) => c.copy(useAveragedParameters = x) }
  }

  val config: Config = parser.parse(args, Config()) match {
    case Some(config) => config
    case None => System.exit(-1); ???
  }

  val (datas, isCV) = {
    val rawTrain = Data.readPitFile(config.trainFile, useExpert = config.trainUseExpert)
    if (config.evalFile == null) {
      (Data.createCV(rawTrain, randomOrder = true), true)
    }
    else {
      val rawEval = Data.readPitFile(config.evalFile, useExpert = config.evalUseExpert)
      (Iterable(Data.createOne(rawTrain, rawEval)), false)
    }
  }

  val evalCV = new Eval()

  var cv = 0
  datas.foreach { case data =>
    cv += 1
    val model = new MultiP(data.training)

    println("\n\n\n---------------------------------------------------------------------------------\n")
    println(s"Size of training data: ${data.training.data.size}")
    println(s"Size of evaluation data: ${data.evaluation.data.size}\n")

    model.train(config.numIter) { itr =>
      println(s"****************** iteration  ${if (isCV) (cv+".") else ""}${itr} ******************")
      val eval = new Eval()
      eval.aggregateEval(model, data.evaluation, config.useAveragedParameters)
      eval.printPR()
    }

    if (isCV) {
      evalCV.aggregateEval(model, data.evaluation, config.useAveragedParameters)
    }

    runTests(model, data)
  }

  if (isCV) {
    println(s"\n\n\n${config.numCV}-CV Results----------------------------------------------------------------------\n")
    evalCV.printPR()
  }

  //---------------------------------------------------------------------------------

  def runTests(model: Parameters, data: Data): Unit = {

    ///  Testing #0   showing the weights of each feature   ///
    if (config.runTests.contains("0")) {
      model.printTheta
    }

    ///  Testing #1   showing the model outputs for each input test sentence pair   ///

    // use the parameters averaged from all iterations
    val useAveragedParameters = true
    // or use only the parameter of the last iteration
    //val useAveragedParameters = false


    val OUTPUT_TOP_PHRASE_PAIR = 10

    //val datadata = model.data    // uncomment if want to do close test (test on training data)
    val datadata = data.evaluation // uncomment if want to do open test (test on the test data)

    var sysoutputs:Map[VectorSentencePair, Double] = Map()

    var totalgoldpos = 0.0

    var sysscores = new ArrayBuffer[Double]()

    for (j <- 0 until datadata.data.length) {
      val datapoint:VectorSentencePair = datadata.data(j)

      val (yespairscores, nopairscores ) = model.inferAllDebug(datapoint, useAveragedParameters)
      var output = ""

      val predicted = model.inferAll(datapoint, useAveragedParameters)
      val r = model.data.IS_PARAPHRASE
      val prediction = predicted.rel(r)
      var score = 0.0

      val goldlabel = datapoint.isParaphrase

      if (goldlabel == true) {
        totalgoldpos += 1.0
      }

      if (config.runTests.contains("1")) {
        if (prediction == 1.0) {
          print("SYS = YESPARA | " )
        } else {
          print("SYS = NOTPARA | " )
        }

        print(datapoint.toString)
      }

      //for (k <- 0 until OUTPUT_TOP_PHRASE_PAIR) {
      for (k <- 0 until datapoint.features.length) {
        val top = argmax(yespairscores)

        if (k == 0) {
          score = exp(yespairscores(top))
        }

        output += "WordPair #" + top + " : " + exp(yespairscores(top)) + " | "  + exp(nopairscores(top)) + " | " + datadata.wordVocab(datapoint.w1ids(top)) + " | " + datadata.wordVocab(datapoint.w2ids(top)) + " | "

        val strfeatures = Utils.bin2int(datapoint.features(top).toArray).map((f) => model.data.featureVocab(f))
        output += strfeatures.mkString(" ")
        output += "\n"
        yespairscores(top) = Double.NegativeInfinity
      }

      sysoutputs += ( datapoint -> score)
      if (config.runTests.contains("1")) {
        println(output)
      }

      sysscores += score

    }

    ///  Testing #2   showing the system outputs in SemEval 2015 PIT shared task format   ///
    if (config.runTests.contains("2")) {
      val dff = new java.text.DecimalFormat("0.0000", new java.text.DecimalFormatSymbols(java.util.Locale.ENGLISH))
      val sysscorearray = sysscores.toArray
      for (j <- 0 until datadata.data.length) {
        val sysscore = sysscorearray(j)
        val datapoint: VectorSentencePair = datadata.data(j)

        val predicted = model.inferAll(datapoint, useAveragedParameters)
        val goldlabel = datapoint.isParaphrase
        val isParaphrase = predicted.rel(model.data.IS_PARAPHRASE) == 1.0 // sysscore > 0.0001d (original code had this but was likely a bug)

        if (isParaphrase) {
          println("true\t" + dff.format(sysscore))// + "\t" + datapoint.origsent + "\t" + datapoint.candsent)
        } else {
          println("false\t" + dff.format(sysscore))// + "\t" + datapoint.origsent + "\t" + datapoint.candsent)
        }
      }
    }


    ///  Testing #3   model performance aggregated on the test dataset   ///
    ///               showing Precision/Recall curve, and max F1 point, and PINC scores etc  ///
    if (config.runTests.contains("3")) {

      // PRINT PR Curve with PINC score
      var tp = 0.0
      var fp = 0.0
      //var tn = 0.0
      var fn = 0.0
      var totalpinc = 0.0



      println("size of test data (count only unique) = " + sysoutputs.toList.length)
      println( "RANK\t\tPRECIS.\tRECALL\tF1\tHIT-PINC|||\tPINC\tMultiP\tSENT1\tSENT2")

      val sortedoutoputs = ListMap(sysoutputs.toList.sortBy{-_._2}:_*)
      var i = 0

      var maxfscore = 0.0
      var maxfoutput = ""

      val df = new java.text.DecimalFormat("#.###")

      for ((paradata, parascore) <- sortedoutoputs) {

        var strhit = "HIT"

        val sent1 = paradata.origsent
        val sent2 = paradata.candsent

        if (sent1 != sent2) {
          i += 1

          val predicted = model.inferAll(paradata, useAveragedParameters)
          val prediction = predicted.rel(model.data.IS_PARAPHRASE)

          val goldlabel = paradata.isParaphrase

          val pincscore = PINC.getRawScore(sent1, sent2)

          if (goldlabel == true) {
            tp += 1
            totalpinc += pincscore
          } else {
            fp += 1
            strhit = "ERR"
          }


          val precision = tp / (tp + fp)
          val recall = tp / totalgoldpos
          val fscore = 2 * precision * recall / (precision + recall)

          val avgpinc = totalpinc / tp

          var output = i + "\t" + strhit + "\t" + df.format(precision) + "\t" + df.format(recall) + "\t" + df.format(fscore) + "\t" + df.format(avgpinc) + "\t|||\t"
          output += df.format(pincscore) + "\t"+ df.format(parascore) + "\t" + sent1 + "\t" + sent2
          println(output)

          if (fscore > maxfscore) {
            maxfscore  = fscore
            maxfoutput = output
          }
        }
      }

      println()
      println ("MAX" + maxfoutput)
    }
  }
}
