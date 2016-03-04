package com.sir.model

import org.apache.spark.SparkContext 
import org.apache.spark.annotation.Since 
import org.apache.spark.api.java.JavaRDD 
import org.apache.spark.mllib.linalg.Vector 
import org.apache.spark.mllib.tree.configuration.{FeatureType} 
import org.apache.spark.rdd.RDD 
import org.apache.spark.util.Utils 
import scala.collection.mutable
import com.sir.elm.ELMType
import com.sir.elm.ELMType._
import com.sir.util.Predictor
class ELMModel(
    val weight: Matrix,
    val bete: Vector,
    val activationFunc: String,
    val eType:ELMType)extends Serializable with Predictor{
  
  override def predict(features: Array[Double]): Option[Double] = None
  /** 
   * Print a summary of the model. 
   */ 
  override def toString: String = eType match { 
    case Classification => s"ELMModel classifier" 
    case Regression => s"ELMModel regressor" 
    case _ => throw new IllegalArgumentException(s"ELMModel given unknown algo parameter: $eType.") 
  } 

  /** 
   * Predict values for a single data point using the model trained. 
   * 
   * @param features array representing a single data point 
   * @return Double prediction from the trained model 
   */ 
  def predict(features: Vector): Double = { 
    topNode.predict(features) 
  }
  /** 
   * Predict values for the given data set using the model trained. 
   * 
   * @param features RDD representing data points to be predicted 
   * @return RDD of predictions for each of the given data points 
  */  
  def predict(features: RDD[Vector]): RDD[Double] = { 
    features.map(x => predict(x)) 
  } 
  
  /** 
   * @param sc  Spark context used to save model data. 
   * @param path  Path specifying the directory in which to save this model. 
   *              If the directory already exists, this method throws an exception. 
   */ 
  override def save(sc: SparkContext, path: String): Unit = { 
    
  }
}

object ELMModel{ 
  def save(sc: SparkContext, path: String, model: ELMModel): Unit = { 
    // Create JSON metadata. 
    val metadata = compact(render( 
      ("class" -> thisClassName) ~ ("version" -> thisFormatVersion) ~ 
      ("algo" -> model.algo.toString) ~ ("numNodes" -> model.numNodes))) 
      sc.parallelize(Seq(metadata), 1).saveAsTextFile(Loader.metadataPath(path))  
    // Create Parquet data. 
    val nodes = model.topNode.subtreeIterator.toSeq 
    val dataRDD: DataFrame = sc.parallelize(nodes) 
      .map(NodeData.apply(0, _)) 
      .toDF() 
    dataRDD.write.parquet(Loader.dataPath(path)) 
  } 

 
  /**  
   * @param sc  Spark context used for loading model files. 
   * @param path  Path specifying the directory to which the model was saved. 
   * @return  Model instance 
   */ 
  def load(sc: SparkContext, path: String): ELMModel = { 
    implicit val formats = DefaultFormats 
    val (loadedClassName, version, metadata) = Loader.loadMetadata(sc, path) 
    val algo = (metadata \ "algo").extract[String] 
    val numNodes = (metadata \ "numNodes").extract[Int] 
    val classNameV1_0 = SaveLoadV1_0.thisClassName 
     (loadedClassName, version) match { 
        case (className, "1.0") if className == classNameV1_0 => 
          SaveLoadV1_0.load(sc, path, algo, numNodes)        case _ => throw new Exception( 
         s"DecisionTreeModel.load did not recognize model with (className, format version):" + 
         s"($loadedClassName, $version).  Supported:\n" + 
         s"  ($classNameV1_0, 1.0)") 
    } 
  } 
} 
