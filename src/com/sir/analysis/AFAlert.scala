package com.sir.analysis

import org.apache.spark.SparkContext
import com.sir.config.Strategy
import com.sir.config.StrategyType
import com.sir.config.ELMType
import com.sir.elm.ELM
import com.sir.config.ActivationFuncType
import com.sir.config.ActivationFuncType._
import com.sir.util.ClassedPoint

object AFAlert {
  def main(args: Array[String]): Unit = {
    val sc = new SparkContext("local","AF_Alert")
    val samplesData = sc.textFile("C://Users//bsn//Scala//ELM_ensemble//data//16773.data")
    val data = samplesData.map {ClassedPoint.parse}

    val numClasses = 2
    val splits = data.randomSplit(Array(0.8, 0.2))
    val (trainData, testData) = (splits(0), splits(1))
    println(trainData.count())
    println(testData.count())
    val flag  = StrategyType.ELM
    val elmType = ELMType.Classification
    val actFunc = ActivationFuncType.fromString("sin")
    val strategy = Strategy.generateStrategy(flag, elmType, numClasses, activationFunc = actFunc)
    val elmModel = ELM.trainClassifier(trainData, strategy, sc)
    val labelAndPreds = testData.map{x =>
      val predict = elmModel.predict(x.features)
      (predict, x.label)
    }
    ErrorEstimation.estimateError(labelAndPreds)
    
    //testData.collect().foreach { println }
    
    labelAndPreds.unpersist()
    trainData.unpersist()
    testData.unpersist()
    data.unpersist()
    samplesData.unpersist()
    sc.stop()
  }
}