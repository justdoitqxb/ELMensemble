package com.sir.model

import org.apache.spark.rdd.RDD
import com.sir.util.Predictor
import com.sir.util.ClassedPoint
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.config.CombinationType
import com.sir.config.CombinationType._

class ELMEnsembleModel(
    val elmType: ELMType,
    val flocks: List[Predictor])extends Serializable with Predictor{
  
  override def predict(features: Array[Double]): Double = {
    1.0
  }
  
  override def predict(features: RDD[Array[Double]]): RDD[ClassedPoint] = { 
    features.map(x => ClassedPoint(predict(x), x)) 
  }
  /** 
   * Predicts for a single data point using the weighted sum of ensemble predictions. 
   * 
   * @param features array representing a single data point 
   * @return predicted category from the trained model 
   */ 
  private def predictBySumming(features: Vector): Double = { 
    val treePredictions = flocks.map(_.predict(features)) 
    blas.ddot(numTrees, treePredictions, 1, treeWeights, 1) 
  } 
 
  /** 
   * Classifies a single data point based on (weighted) majority votes. 
   */ 
  private def predictByVoting(features: Array[Double]): Double = { 
    val votes = mutable.Map.empty[Int, Double] 
    flocks.view.zip(treeWeights).foreach { case (tree, weight) => 
      val prediction = tree.predict(features).toInt 
      votes(prediction) = votes.getOrElse(prediction, 0.0) + weight 
    } 
    votes.maxBy(_._2)._1 
  } 

  /** 
   * Predict values for a single data point using the model trained. 
   * 
   * @param features array representing a single data point 
   * @return predicted category from the trained model 
   */ 
  private def predict(features: Array[Double], combinationType: CombinationType): Double = { 
   (elmType, combinationType) match { 
      case (Regression, Sum) => 
        predictBySumming(features) 
      case (Regression, Average) => 
        predictBySumming(features) / sumWeights 
      case (Classification, Sum) => // binary classification 
        val prediction = predictBySumming(features) 
        // TODO: predicted labels are +1 or -1 for GBT. Need a better way to store this info. 
        if (prediction > 0.0) 1.0 else 0.0 
      case (Classification, Vote) => 
        predictByVoting(features) 
      case _ => 
        throw new IllegalArgumentException( 
         "TreeEnsembleModel given unsupported (algo, combiningStrategy) combination: " + 
            s"($elmType, $combinationType).") 
     } 
  } 

  /** 
   * Predict values for the given data set. 
   * 
   * @param features RDD representing data points to be predicted 
   * @return RDD[Double] where each entry contains the corresponding prediction 
   */ 
  def predict(features: RDD[Vector]): RDD[Double] = features.map(x => predict(x)) 

  /** 
   * Java-friendly version of [[org.apache.spark.mllib.tree.model.TreeEnsembleModel#predict]]. 
   */ 
  def predict(features: JavaRDD[Vector]): JavaRDD[java.lang.Double] = { 
    predict(features.rdd).toJavaRDD().asInstanceOf[JavaRDD[java.lang.Double]] 
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