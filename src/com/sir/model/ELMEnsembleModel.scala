package com.sir.model

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import scala.collection.mutable.Map
import com.sir.util.Predictor
import com.sir.util.ClassedPoint
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.config.CombinationType
import com.sir.config.CombinationType._

class ELMEnsembleModel(
    val elmType: ELMType,
    val combinationType: CombinationType,
    val flocks: Array[Predictor])extends Serializable with Predictor{
  /** 
   * Predict values for a single data point using the model trained. 
   * 
   * @param features array representing a single data point 
   * @return predicted category from the trained model 
   */ 
  override def predict(features: Array[Double]): Double = {
    (elmType, combinationType) match { 
      case (ELMType.Regression, CombinationType.Sum) => 
        predictBySumming(features) 
      case (ELMType.Regression, CombinationType.Average) => 
        predictBySumming(features)
      case (ELMType.Classification, CombinationType.Vote) => 
        predictByVoting(features) 
      case _ => 
        throw new IllegalArgumentException( 
         "ELMEnsembleModel given unsupported (elmType, combinationType) combination: " + 
            s"($elmType, $combinationType).") 
     } 
  }
  
  /** 
   * Predict values for the given data set. 
   * 
   * @param features RDD representing data points to be predicted 
   * @return RDD[Double] where each entry contains the corresponding prediction 
   */
  override def predict(features: RDD[Array[Double]]): RDD[ClassedPoint] = { 
    features.map(x => ClassedPoint(predict(x), x)) 
  }
  /** 
   * Predicts for a single data point using the weighted sum of ensemble predictions. 
   * 
   * @param features array representing a single data point 
   * @return predicted category from the trained model 
   */ 
  private def predictBySumming(features: Array[Double]): Double = { 
    require(flocks.length > 0)
    val predictions = flocks.map(_.predict(features)) 
    predictions.sum / predictions.length
  } 
 
  /** 
   * Classifies a single data point based on (weighted) majority votes. 
   */ 
  private def predictByVoting(features: Array[Double]): Double = { 
    val votes = Map.empty[Double, Int] 
    flocks.foreach { pridictor => 
        val prediction = pridictor.predict(features) 
        votes(prediction) = votes.getOrElse(prediction, 0) + 1
    } 
    votes.maxBy(_._2)._1 
  } 
}

object ELMEnsembleModel { 
  /** 
   * Model data for model import/export. 
   */ 
  def save(sc: SparkContext, path: String, model: ELMModel): Unit = {
    
  }

  /** 
   * Load elm or k_elm for an ensemble, and return them in order. 
   * @param path path to load the model from 
   */ 
  def load(sc: SparkContext, path: String): Unit = {
    
  }
} 