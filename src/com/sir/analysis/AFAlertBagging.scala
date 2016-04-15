package com.sir.analysis

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
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
import com.sir.util.ClassedPoint
//import org.apache.spark.mllib.tree.RandomForest
//import org.apache.spark.mllib.tree.model.RandomForestModel
//import org.apache.spark.mllib.util.MLUtils
//import org.apache.spark.mllib.regression.LabeledPoint
//import org.apache.spark.mllib.linalg.Vector
//import org.apache.spark.mllib.linalg.Vectors

object AFAlertBagging {
  def main(args: Array[String]): Unit = {
    if (args.length < 5) { 
      println("Usage: [training file] [validation file] [number flocks] [num examples per flock] [classifier type]") 
      System.exit(1) 
    } 
    val conf = new SparkConf().setAppName("AFAlertBagging").setMaster("spark://Master:7077")
    val sc = new SparkContext(conf)
    val trainingData = sc.textFile("hdfs://172.17.0.2:9000" + args(0))
    val training = trainingData.map{ x => ClassedPoint.parse(x, true)}
    val validataionData = sc.textFile("hdfs://172.17.0.2:9000" + args(1))
    val validation = validataionData.map { ClassedPoint.parse }
         
    val splits = validation.randomSplit(Array(0.8, 0.2))
    val (trainDataPart, testData) = (splits(0), splits(1))
    val trainData = training ++ trainDataPart
    val numClasses = 2
    val numFlocks: Int = args(2).toInt
    val numSamplesPerNode: Int = args(3).toInt
    val flag  = StrategyType.ELMEnsemble
    val elmType = ELMType.Classification
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses, classifierType = ClassifierType.formString(args(4)))
    val model = ELMBagging.trainClassifier(trainData, numFlocks, numSamplesPerNode, 0.6, CombinationType.Vote, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = model.predict(x.features)
      (predict, x.label)
    }

//    //与随机森林，结果对比
//    val categoricalFeaturesInfo = Map[Int, Int]()
//    val numTrees = args(2).toInt
//    val featureSubsetStrategy = "auto" 
//    val impurity = "gini"
//    val maxDepth = 10
//    val maxBins = 32
//    val RFTrainData = trainData.map { x => LabeledPoint(x.label,Vectors.dense(x.features)) }
//    val rfmodel = RandomForest.trainClassifier(RFTrainData, numClasses, categoricalFeaturesInfo,
//      numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)
//    val testingData = testData.map{ x => LabeledPoint(x.label,Vectors.dense(x.features)) }
//    val lp = testingData.map { point =>
//       val prediction = rfmodel.predict(point.features)
//       (point.label, prediction)
//    }
//    ErrorEstimation.estimateError(lp)
    val pos = validation.filter { _.label == 0.0 }.count()
    val neg = validation.filter { _.label == 1.0 }.count()
    ErrorEstimation.estimateError(labelAndPreds)
    println(validation.count)
    println(pos)
    println(neg)
    sc.stop() 
    sc.stop() 
  }
}