package com.sir.elm

import org.apache.spark.rdd.RDD
import com.sir.util.ClassedPoint
import com.sir.util.TimeTracker
import com.sir.model.ELMModel
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.config.ELMMeta
import com.sir.config.Strategy
import org.apache.spark.SparkContext
import com.sir.activefunc.ActivationFunc
/**
 * Generic Predictor provides predict.
 * Created by Qin on 2015. 12. 15..
 */
class ELM(val strategy: Strategy, sc: SparkContext){
  /** 
   * Method to train a ELM over an RDD 
   * 
   * @param input Training data: RDD of [ClassedPoint]. 
   * @return ELMModel that can be used for prediction. 
   */ 
  def run(input: RDD[ClassedPoint]): ELMModel = { 
    strategy.assertValid
    val timer = new TimeTracker() 
    timer.start("elm") 
    val elmMeta = ELMMeta.buildMeta(input, strategy)
    val beta = calBeta(input, elmMeta)
    timer.stop("elm") 
    println("Training time: " + timer.toString())
    new ELMModel(elmMeta.flag, elmMeta.WAug, beta, elmMeta.activationFunc, 1.0) //???Generate training accuracy
  } 
  
  private def calBeta(input: RDD[ClassedPoint], elmMeta: ELMMeta): ELMMatrix = elmMeta.flag match{
      case ELMType.Classification => 
        val (features, target) = reBuildData(input, elmMeta)
        val HActive = ActivationFunc.calActiveFunc(elmMeta.activationFunc, features * elmMeta.WAug)
        val pinvH = ELMMatrix.pinv(HActive, sc)
        pinvH * target
      case ELMType.Regression => throw new IllegalArgumentException(s"Not support for regression now")
  }

  private def reBuildData(input: RDD[ClassedPoint], elmMeta: ELMMeta): (ELMMatrix, ELMMatrix) = {
    val features = new ELMMatrix(elmMeta.numExamples, elmMeta.numFeatures + 1)
    val target = new ELMMatrix(elmMeta.numExamples, elmMeta.numClasses)
    val tmp = input.collect()
    for(i <- 0 until elmMeta.numExamples){
      val example = tmp.apply(i)
      for(j <- 0 until elmMeta.numFeatures){
        features.set(i, j, example.features.apply(j))
        target.set(i, example.label.toInt, 1.0)
      }
      features.set(i, elmMeta.numFeatures, 1.0)
    }   
    (features, target)
  }
    
}

object ELM extends Serializable {
    /**
  * Method to train a ELM model for binary or multiclass classification.
  *
  * @param trainSet--Training dataset: RDD of [[com.sir.util.ClassedPoint]].
  * @param strategy:
	* 			 Elm_Type--regression for (both binary and multi-classes);classification
	* 			 numberofHiddenNode--Number of hidden neurons assigned to the ELM
  * 			 activationFunc -- Type of activation function:
  * @return ELM model that can be used for prediction
  */
  def trainClassifier(
    trainSet: RDD[ClassedPoint],
    strategy: Strategy, sc: SparkContext): ELMModel = {
      new ELM(strategy, sc).run(trainSet)
  }
  
//  def trainRegressor(
//    trainSet: RDD[ClassedPoint],
//    strategy: Strategy): ELMModel = {
//      new ELM(strategy).run(trainSet)
//  }
}