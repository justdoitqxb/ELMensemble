package com.sir.test

import org.apache.spark.SparkContext
import com.sir.config.Strategy
import com.sir.config.StrategyType
import com.sir.config.ELMType
import com.sir.elm.ELM
import com.sir.analysis.ErrorEstimation

object ELMTest {
  def main(args: Array[String]): Unit = {
    val sc = new SparkContext("local","ELMTest")
    val numClasses = 2
    val data = DataGenerator.generate(2000, 10, numClasses, sc)
    val splits = data.randomSplit(Array(0.8, 0.2))
    val (trainData, testData) = (splits(0), splits(1))
    val flag  = StrategyType.ELM
    val elmType = ELMType.Classification
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses)
    val elmModel = ELM.trainClassifier(trainData, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = elmModel.predict(x.features)
      (predict, x)
    }
    ErrorEstimation.estimateError(labelAndPreds)
    labelAndPreds.unpersist()
    trainData.unpersist()
    testData.unpersist()
    data.unpersist()
    sc.stop()
    
  }
}