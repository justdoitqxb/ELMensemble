package com.sir.elmensemble

import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import com.sir.util.ClassedPoint
import com.sir.util.TimeTracker
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.activefunc.ActivationFunc
import com.sir.elm.Strategy

/**
 * Generic Predictor provides predict.
 * Created by Qin on 2015. 12. 15..
 */
class ELMEnsemble (val strategy: Strategy, sc: SparkContext){
  /** 
   * Method to train a ELM over an RDD 
   * 
   * @param input Training data: RDD of [ClassedPoint]. 
   * @return ELMModel that can be used for prediction. 
   */ 
  def run(input: RDD[ClassedPoint]): ELMEnsembleModel = { 
    strategy.assertValid
    val timer = new TimeTracker() 
    timer.start("total") 
//    val trees = Seq.fill(20)(TreeBuilder.build(data, maxDepth))
//    RandomForests(trees.toList)

    
    timer.stop("total") 
  } 
}

object ELMEnsemble extends {
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
    trainSet: RDD[ClassedPoint],
    strategy: Strategy, sc: SparkContext): ELMEnsembleModel = {
      new ELMEnsemble(strategy, sc).run(trainSet)
  }
  
//  def trainRegressor(
//    trainSet: RDD[ClassedPoint],
//    strategy: Strategy): ELMModel = {
//      new ELM(strategy).run(trainSet)
//  }
}