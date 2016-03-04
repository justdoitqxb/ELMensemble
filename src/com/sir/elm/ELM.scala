package com.sir.elm

import org.apache.spark.rdd.RDD
import com.sir.util.ClassedPoint
import com.sir.util.TimeTracker
import com.sir.analysis.Strategy
/**
 * Generic Predictor provides predict.
 * Created by Qin on 2015. 12. 15..
 */
class ELM(val strategy: Strategy){
  /** 
   * Method to train a ELM over an RDD 
   * 
   * @param input Training data: RDD of [ClassedPoint]. 
   * @return ELMModel that can be used for prediction. 
   */ 
  def run(input: RDD[ClassedPoint]): ELMModel = { 
    val timer = new TimeTracker() 
    timer.start("total") 

    timer.stop("total") 
    new ELMModel() 
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
    strategy: Strategy): ELMModel = {
      new ELM(strategy).run(trainSet)
  }
  
  def trainRegressor(
    trainSet: RDD[ClassedPoint],
    strategy: Strategy): ELMModel = {
      new ELM(strategy).run(trainSet)
  }
}