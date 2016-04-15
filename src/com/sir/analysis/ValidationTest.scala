package com.sir.analysis

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
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

object ValidationTest {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("ValidationTest").setMaster("spark://Master:7077")
    val sc = new SparkContext(conf)
    val trainingData = sc.textFile("hdfs://172.17.0.2:9000/af/validation70/")
    val training = trainingData.map{ ClassedPoint.parse}
    val validataionData = sc.textFile("hdfs://172.17.0.2:9000/af/validation70/")
    val validation = validataionData.map { ClassedPoint.parse }
         
    val splits = validation.randomSplit(Array(0.8, 0.2))
    val (trainData, testData) = (splits(0), splits(1))
    //val trainData = training ++ trainDataPart
    val numClasses = 2
    val numFlocks: Int = 10
    val numSamplesPerNode: Int = 1000
    val flag  = StrategyType.ELMEnsemble
    val elmType = ELMType.Classification
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses, classifierType = ClassifierType.ELM)
    val model = ELMBagging.trainClassifier(trainData, numFlocks, numSamplesPerNode, 0.8, CombinationType.WeightVote, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = model.predict(x.features)
      (predict, x.label)
    }
    
//    //与随机森林，结果对比
//    val categoricalFeaturesInfo = Map[Int, Int]()
//    val numTrees = 10
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
    ErrorEstimation.estimateError(labelAndPreds)
    sc.stop() 
  }
}