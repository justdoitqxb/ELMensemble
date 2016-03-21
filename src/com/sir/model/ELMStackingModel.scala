package com.sir.model

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import com.sir.util.Predictor
import com.sir.util.ClassedPoint
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.config.CombinationType
import com.sir.config.CombinationType._
import com.sir.elm.ELMMatrix
import com.sir.util.KitBox

class ELMStackingModel(
    val elmType: ELMType,
    val flocks: Array[Predictor],
    val tier2Weight: ELMMatrix)extends Serializable with Predictor{
  /** 
   * Predict values for a single data point using the model trained. 
   * 
   * @param features array representing a single data point 
   * @return predicted category from the trained model 
   */ 
  override def predict(features: Array[Double]): Double = {
    elmType match { 
      case ELMType.Classification => 
        val output = calOutput(features)
        KitBox.maxPosition(output)
      case _ => 
        throw new IllegalArgumentException( 
         "ELMEnsembleModel given unsupported (elmType, combinationType) combination: " + 
            s"($elmType).") 
     } 
  }
  
  override def calOutput(features: Array[Double]): Array[Double] = {
    val newFeatures = ArrayBuffer[Double]()
    for(i <- 0 until flocks.length){
      newFeatures ++= flocks.apply(i).calOutput(features)
    }
    val HActive = ELMMatrix.converttoELMMatrix(newFeatures.toArray)
    (HActive * tier2Weight).applyRow(0)
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

}

object ELMStackingModel { 
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