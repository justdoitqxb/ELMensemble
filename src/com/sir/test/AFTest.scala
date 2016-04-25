package com.sir.test
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import com.sir.util.ClassedPoint
import com.sir.config.Strategy
import com.sir.config.StrategyType
import com.sir.config.StrategyType._
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.elmensemble.ELMBagging
import com.sir.config.ClassifierType
import com.sir.config.ClassifierType._
import com.sir.config.CombinationType
import com.sir.config.CombinationType._
import com.sir.analysis.ErrorEstimation
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.tree.RandomForest

object AFTest {
  def main(args: Array[String]): Unit = {
    val sc = new SparkContext("local", "AFTest")
    val validataionData = sc.textFile("C://Users//bsn//Scala//ELM_ensemble//validationSet70//*.data")
    val validation = validataionData.map { ClassedPoint.parse }
    val numClasses = 2
    val splits = validation.randomSplit(Array(0.95, 0.05))
    val (trainData, testData) = (splits(0), splits(1))
//    val l0 = trainData.filter { _.label == 0.0 }.count()
//    val l1 =trainData.filter { _.label == 1.0 }.count()
//    val total = trainData.count
//    println(total)
//    println(l0)
//    println(l1)
    val numFlocks: Int  = 3
    val numSamplesPerNode: Int = 1000
    val flag  = StrategyType.ELMEnsemble
    val elmType = ELMType.Classification
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses, classifierType = ClassifierType.KernelELM)
    val model = ELMBagging.trainClassifier(trainData, numFlocks, numSamplesPerNode, 0.8, CombinationType.WeightVote, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = model.predict(x.features)
      (predict, x.label)
    }
    ErrorEstimation.estimateError(labelAndPreds)
    model.flocks.foreach( p => println(p.weight))
    sc.stop() 
  }
}