package com.sir.test

import org.apache.spark.SparkContext
import com.sir.config.Strategy
import com.sir.config.StrategyType
import com.sir.config.ELMType
import com.sir.elm.ELM
import com.sir.config.ActivationFuncType
import com.sir.config.ActivationFuncType._
import com.sir.analysis.ErrorEstimation
import org.apache.spark.mllib.tree.RandomForest
import org.apache.spark.mllib.tree.model.RandomForestModel
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.Vector
import org.apache.spark.mllib.linalg.Vectors

object ELMTest {
  def main(args: Array[String]): Unit = {
    val sc = new SparkContext("local","ELMTest")
    val numClasses = 2
    val data = DataGenerator.generate(5000, 100, numClasses, sc)
    val splits = data.randomSplit(Array(0.8, 0.2))
    val (trainData, testData) = (splits(0), splits(1))
    println(trainData.count())
    println(testData.count())
    val flag  = StrategyType.ELM
    val elmType = ELMType.Classification
    val actFunc = ActivationFuncType.fromString("sigmoid")
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses, activationFunc = actFunc)
    val elmModel = ELM.trainClassifier(trainData, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = elmModel.predict(x.features)
      (predict, x.label)
    }
    ErrorEstimation.estimateError(labelAndPreds)
    //labelAndPreds.collect().foreach(println)
    //与随机森林，结果对比
//    val categoricalFeaturesInfo = Map[Int, Int]()
//    val numTrees = 2 
//    val featureSubsetStrategy = "auto" 
//    val impurity = "gini"
//    val maxDepth = 10
//    val maxBins = 32
//    val trainingData = trainData.map { x => LabeledPoint(x.label,Vectors.dense(x.features)) }
//    val model = RandomForest.trainClassifier(trainingData, numClasses, categoricalFeaturesInfo,
//      numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)
//    val testingData = testData.map{ x => LabeledPoint(x.label,Vectors.dense(x.features)) }
//    val lp = testingData.map { point =>
//       val prediction = model.predict(point.features)
//       (point.label, prediction)
//    }
//    ErrorEstimation.estimateError(lp)
//    labelAndPreds.unpersist()
//    trainData.unpersist()
//    testData.unpersist()
//    data.unpersist()
    sc.stop() 
  }
}