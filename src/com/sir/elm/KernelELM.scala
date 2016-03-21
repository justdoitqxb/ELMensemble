package com.sir.elm

import org.apache.spark.rdd.RDD
import com.sir.util.ClassedPoint
import com.sir.util.TimeTracker
import com.sir.model.KernelELMModel
import com.sir.config.ELMType
import com.sir.config.ELMType._
import org.apache.spark.SparkContext
import com.sir.activefunc.ActivationFunc
import com.sir.kernel.Kernel
import com.sir.config.KernelELMMeta
import com.sir.config.Strategy

/**
 * Generic Predictor provides predict.
 * Created by Qin on 2016. 3. 7..
 */
class KernelELM(val strategy: Strategy, sc: SparkContext){
  /** 
   * Method to train a ELM over an RDD 
   * 
   * @param input Training data: RDD of [ClassedPoint]. 
   * @return ELMModel that can be used for prediction. 
   */ 
  def run(input: RDD[ClassedPoint]): KernelELMModel = { 
    strategy.assertValid
    val timer = new TimeTracker() 
    timer.start("kernelelm") 
    val kernelELMMeta = KernelELMMeta.buildKernelMeta(input, strategy)
    val (features, target) = reBuildData(input, kernelELMMeta)
    val beta = calBeta(features, target, kernelELMMeta)
    timer.stop("kernelelm") 
    println("Training time: " + timer.toString())
    val model = new KernelELMModel(kernelELMMeta.flag, features, kernelELMMeta.kernelType, beta)
    model.SetTainingAccuracy(input)
    model
  } 
  
  private def calBeta(features: ELMMatrix, target: ELMMatrix, kernelELMMeta: KernelELMMeta): ELMMatrix = kernelELMMeta.flag match{
      case ELMType.Classification =>         
        val kernelMat = Kernel.calKernel(kernelELMMeta.kernelType, features, features)
        val regularization = new SparserELMMatrix(kernelELMMeta.numExamples, kernelELMMeta.numExamples).speye / kernelELMMeta.regularizationCoefficient
        val pinvKernelMat = ELMMatrix.pinv(kernelMat + regularization , sc)
        pinvKernelMat * target
      case ELMType.Regression => throw new IllegalArgumentException(s"Not support for regression now")
  }

  private def reBuildData(input: RDD[ClassedPoint], kernelELMMeta: KernelELMMeta): (ELMMatrix, ELMMatrix) = {
    val features = new ELMMatrix(kernelELMMeta.numExamples, kernelELMMeta.numFeatures)
    val target = new ELMMatrix(kernelELMMeta.numExamples, kernelELMMeta.numClasses)
    val tmp = input.collect()
    for(i <- 0 until kernelELMMeta.numExamples){
      val example = tmp.apply(i)
      for(j <- 0 until kernelELMMeta.numFeatures){
        features.set(i, j, example.features.apply(j))
        target.set(i, example.label.toInt, 1.0)
      }
    }   
    (features, target)
  }   
}

object KernelELM extends Serializable {
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
    strategy: Strategy, sc: SparkContext): KernelELMModel = {
      new KernelELM(strategy, sc).run(trainSet)
  }
  
//  def trainRegressor(
//    trainSet: RDD[ClassedPoint],
//    strategy: Strategy): ELMModel = {
//      new ELM(strategy).run(trainSet)
//  }
}