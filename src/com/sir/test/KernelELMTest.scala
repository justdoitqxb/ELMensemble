package com.sir.test

import org.apache.spark.SparkContext
import com.sir.config.Strategy
import com.sir.config.StrategyType
import com.sir.config.StrategyType._
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.elm.KernelELM
import com.sir.analysis.ErrorEstimation
import org.apache.spark.mllib.tree.RandomForest
import org.apache.spark.mllib.tree.model.RandomForestModel
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.Vector
import org.apache.spark.mllib.linalg.Vectors

object KernelELMTest {
  def main(args: Array[String]): Unit = {
    val sc = new SparkContext("local","ELMTest")
    val numClasses = 2
    val data = DataGenerator.generate(500, 100, numClasses, sc)
    val splits = data.randomSplit(Array(0.8, 0.2))
    val (trainData, testData) = (splits(0), splits(1))
    println(trainData.count())
    println(testData.count())
    val flag  = StrategyType.KernelELM
    val elmType = ELMType.Classification
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses)
    val model = KernelELM.trainClassifier(trainData, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = model.predict(x.features)
      (predict, x.label)
    }
    ErrorEstimation.estimateError(labelAndPreds)
    //与随机森林，结果对比
    val categoricalFeaturesInfo = Map[Int, Int]()
    val numTrees = 10
    val featureSubsetStrategy = "auto" 
    val impurity = "gini"
    val maxDepth = 10
    val maxBins = 32
    val trainingData = trainData.map { x => LabeledPoint(x.label,Vectors.dense(x.features)) }
    val rfmodel = RandomForest.trainClassifier(trainingData, numClasses, categoricalFeaturesInfo,
      numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)
    val testingData = testData.map{ x => LabeledPoint(x.label,Vectors.dense(x.features)) }
    val lp = testingData.map { point =>
       val prediction = rfmodel.predict(point.features)
       (point.label, prediction)
    }
    ErrorEstimation.estimateError(lp)
    labelAndPreds.unpersist()
    trainData.unpersist()
    testData.unpersist()
    trainingData.unpersist()
    testingData.unpersist()
    data.unpersist()
    sc.stop() 
  }
}