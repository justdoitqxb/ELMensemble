package com.sir.elmensemble

import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import scala.collection.mutable.ArrayBuffer
import com.sir.util.ClassedPoint
import com.sir.util.TimeTracker
import com.sir.util.Predictor
import com.sir.util.Splitter
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.config.ClassifierType
import com.sir.config.ClassifierType._
import com.sir.activefunc.ActivationFunc
import com.sir.config.Strategy
import com.sir.model.ELMStackingModel
import com.sir.elm.ELM
import com.sir.elm.KernelELM
import com.sir.elm.ELMMatrix
import com.sir.config.CombinationType
import com.sir.config.CombinationType._

/**
 * Generic Predictor provides predict.
 * Created by Qin on 2015. 12. 15..
 */
class ELMStacking (
    val strategy: Strategy, 
    val numFlocks: Int,
    val numSamplesPerNode: Int,
    val elmPerKelm: Double,
    val sc: SparkContext){
  /** 
   * Method to train a ELM over an RDD 
   * @param input Training data: RDD of [ClassedPoint]. 
   * @return ELMModel that can be used for prediction. 
   */ 
  def run(input1: RDD[ClassedPoint], input2: RDD[ClassedPoint]): ELMStackingModel = { 
    strategy.assertValid
    val timer = new TimeTracker() 
    timer.start("total") 
    val flocks: Array[Predictor] = Array.fill[Predictor](numFlocks)(buildBaseClassifier(input1))
    val filterFlocks = flocks.filter { predictor => predictor.weight > 0.5 }
    val tier2Weight = calTier2Weight(filterFlocks, input2) 
    timer.stop("total") 
    println("Ensemble models training time: " + timer.toString())
    new ELMStackingModel(ELMType.Classification, filterFlocks, tier2Weight)
  } 
  
  private def buildBaseClassifier(input: RDD[ClassedPoint]): Predictor = {
    val (classifierType, childStrategy) = Strategy.generateChildStrategy(strategy, elmPerKelm)
    val numSamples = java.lang.Math.min(numSamplesPerNode, 20000)
    val trainSet = Splitter.bootstrapSampling(input, numSamples)
    println("Number Examples: " + trainSet.count())
    classifierType match {
      case ClassifierType.ELM => ELM.trainClassifier(trainSet, childStrategy, sc)
      case ClassifierType.KernelELM => KernelELM.trainClassifier(trainSet, childStrategy, sc)
      case _ => throw new IllegalArgumentException(s"Given unsupported parameter")
    }
  }
  
  private def calTier2Weight(flocks: Array[Predictor], input2: RDD[ClassedPoint]): ELMMatrix = {
    println("Calculate the weight of tier 2...")
    val numSamples = java.lang.Math.min(input2.count().toInt, 30000)
    val trainSet = if(numSamples == 100000) Splitter.bootstrapSampling(input2, numSamples) else input2
    val tier2TrainSet = trainSet.map { sample => 
      val newFeatures = ArrayBuffer[Double]()
      for(i <- 0 until flocks.length){
        newFeatures ++= flocks.apply(i).calOutput(sample.features)
      }
      ClassedPoint(sample.label, newFeatures.toArray)
    }
    val tmp = tier2TrainSet.collect()
    val numFeatures = tmp.apply(0).features.length
    val numClasses = numFeatures / flocks.length
    val features = new ELMMatrix(numSamples, numFeatures)
    val target = new ELMMatrix(numSamples, numClasses)
    for(i <- 0 until numSamples){
      val example = tmp.apply(i)
      for(j <- 0 until numFeatures){
        features.set(i, j, example.features.apply(j))
      }
      target.set(i, example.label.toInt, 1.0)
    } 
    val pinvH = ELMMatrix.pinv(features, sc)
    pinvH * target
  }
}

object ELMStacking{
    /**
  * Method to train model for binary or multiclass classification.
  *
  * @param trainSet--Training dataset: RDD of [[com.sir.util.ClassedPoint]].
  * @param strategy:
	* 			 Elm_Type--regression for (both binary and multi-classes);classification
	* 			 numberofHiddenNode--Number of hidden neurons assigned to the ELM
  * 			 activationFunc -- Type of activation function:
  * @return ELM model that can be used for prediction
  */
  def trainClassifier(
    tier1TrainSet: RDD[ClassedPoint],
    tier2TrainSet: RDD[ClassedPoint],
    numFlocks: Int,
    numSamplesPerNode: Int,
    elmPerKelm: Double,
    strategy: Strategy, 
    sc: SparkContext): ELMStackingModel = {
      new ELMStacking(strategy, numFlocks, numSamplesPerNode, elmPerKelm, sc).run(tier1TrainSet, tier2TrainSet)
  }
  
//  def trainRegressor(
//    trainSet: RDD[ClassedPoint],
//    strategy: Strategy): ELMModel = {
//      new ELM(strategy).run(trainSet)
//  }
}