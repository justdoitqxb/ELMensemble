package com.sir.test

import org.apache.spark.SparkContext
import com.sir.config.Strategy
import com.sir.config.StrategyType
import com.sir.config.ELMType
import com.sir.elm.ELM
import com.sir.analysis.ErrorEstimation

import org.apache.spark.mllib.tree.RandomForest
import org.apache.spark.mllib.tree.model.RandomForestModel
import org.apache.spark.mllib.util.MLUtils

object ELMTest {
  def main(args: Array[String]): Unit = {
    val sc = new SparkContext("local","ELMTest")
    val numClasses = 2
    val data = DataGenerator.generate(100, 10, numClasses, sc)
    val splits = data.randomSplit(Array(0.8, 0.2))
    val (trainData, testData) = (splits(0), splits(1))
    println(trainData.count())
    println(testData.count())
    val flag  = StrategyType.ELM
    val elmType = ELMType.Classification
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses)
    val elmModel = ELM.trainClassifier(trainData, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = elmModel.predict(x.features)
      (predict, x)
    }
    ErrorEstimation.estimateError(labelAndPreds)
    
    val categoricalFeaturesInfo = Map[Int, Int]()
    val numTrees = 2 
    val featureSubsetStrategy = "auto" 
    val impurity = "gini"
    val maxDepth = 10
    val maxBins = 32
    val model = RandomForest.trainClassifier(trainData, numClasses, categoricalFeaturesInfo,
      numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)

    val lp = testData.map { point =>
       val prediction = model.predict(point.features)
       (point.label, prediction)
    }
    val testErr = lp.filter(r => r._1 != r._2).count.toDouble / testData.count()
    println("Test Error = " + testErr)
//    labelAndPreds.unpersist()
//    trainData.unpersist()
//    testData.unpersist()
//    data.unpersist()
    sc.stop() 
  }
}