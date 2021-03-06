package com.sir.test

import org.apache.spark.SparkContext
import com.sir.config.Strategy
import com.sir.config.StrategyType
import com.sir.config.StrategyType._
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.elmensemble.ELMBagging
import com.sir.analysis.ErrorEstimation
import com.sir.config.ClassifierType
import com.sir.config.ClassifierType._
import com.sir.config.CombinationType
import com.sir.config.CombinationType._
import org.apache.spark.mllib.tree.RandomForest
import org.apache.spark.mllib.tree.model.RandomForestModel
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.Vector
import org.apache.spark.mllib.linalg.Vectors


object BaggingTest {
  def main(args: Array[String]): Unit = {
    val sc = new SparkContext("local","ELMTest")
    val numClasses = 2
    val data = DataGenerator.generate(5000, 100, numClasses, sc)
    val splits = data.randomSplit(Array(0.8, 0.2))
    val (trainData, testData) = (splits(0), splits(1))
    println(trainData.count())
    println(testData.count())
    val numFlocks: Int  = 3
    val numSamplesPerNode: Int = 400
    val flag  = StrategyType.ELMEnsemble
    val elmType = ELMType.Classification
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses, classifierType = ClassifierType.KernelELM)
    val model = ELMBagging.trainClassifier(trainData, numFlocks, numSamplesPerNode, 0.6, CombinationType.WeightVote, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = model.predict(x.features)
      (predict, x.label)
    }
    ErrorEstimation.estimateError(labelAndPreds)
    model.flocks.foreach( p => println(p.weight))
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