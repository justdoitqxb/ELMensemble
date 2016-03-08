package com.sir.model

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.Vector
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.util.Predictor
import com.sir.elm.ELMMatrix
import com.sir.config.ActivationFuncType
import com.sir.config.ActivationFuncType._
import com.sir.util.ClassedPoint
import com.sir.activefunc.ActivationFunc

class ELMModel(
    val elmType: ELMType,
    val WAug: ELMMatrix,
    val beta: ELMMatrix,
    val activationFunc: ActivationFuncType,
    val tainingAccuracy: Double)extends Serializable with Predictor{
  
  override def predict(features: Array[Double]): Double = {
    val fAug = features :+ 1.0
    val newFeatures = ELMMatrix.converttoELMMatrix(fAug)
    val HActive = ActivationFunc.calActiveFunc(activationFunc, newFeatures * WAug)
    val output = (HActive * beta).applyRow(0)
    maxPosition(output)
  }
  
  private def maxPosition(arr: Array[Double]): Double = {
    require(arr.length > 0)
    var maxValue = arr.apply(0)
    var maxPosition = 0
    for(i <- 1 until arr.length){
      if(arr.apply(i) > maxValue){
        maxValue = arr.apply(i)
        maxPosition = i
      }
    }
    maxPosition.toDouble
  }
  
  /** 
   * Print a summary of the model. 
   */ 
  override def toString: String = elmType match { 
    case Classification => s"ELMModel classifier" 
    case Regression => s"ELMModel regressor" 
    case _ => throw new IllegalArgumentException(s"ELMModel given unknown algo parameter: $elmType.") 
  } 


  /** 
   * Predict values for the given data set using the model trained. 
   * 
   * @param features RDD representing data points to be predicted 
   * @return RDD of predictions for each of the given data points 
   */  
  override def predict(features: RDD[Array[Double]]): RDD[ClassedPoint] = { 
    features.map(x => ClassedPoint(predict(x), x)) 
  } 
  
  /** 
   * @param sc  Spark context used to save model data. 
   * @param path  Path specifying the directory in which to save this model. 
   *              If the directory already exists, this method throws an exception. 
   */ 
  def save(sc: SparkContext, path: String): Unit = { 
    
  }
}

object ELMModel{ 
  def save(sc: SparkContext, path: String, model: ELMModel): Unit = { 
//    // Create JSON metadata. 
//    val metadata = compact(render( 
//      ("class" -> thisClassName) ~ ("version" -> thisFormatVersion) ~ 
//      ("algo" -> model.algo.toString) ~ ("numNodes" -> model.numNodes))) 
//      sc.parallelize(Seq(metadata), 1).saveAsTextFile(Loader.metadataPath(path))  
//    // Create Parquet data. 
//    val nodes = model.topNode.subtreeIterator.toSeq 
//    val dataRDD: DataFrame = sc.parallelize(nodes) 
//      .map(NodeData.apply(0, _)) 
//      .toDF() 
//    dataRDD.write.parquet(Loader.dataPath(path)) 
  } 

 
  /**  
   * @param sc  Spark context used for loading model files. 
   * @param path  Path specifying the directory to which the model was saved. 
   * @return  Model instance 
   */ 
  def load(sc: SparkContext, path: String): Unit = { 
//    implicit val formats = DefaultFormats 
//    val (loadedClassName, version, metadata) = Loader.loadMetadata(sc, path) 
//    val algo = (metadata \ "algo").extract[String] 
//    val numNodes = (metadata \ "numNodes").extract[Int] 
//    val classNameV1_0 = SaveLoadV1_0.thisClassName 
//     (loadedClassName, version) match { 
//        case (className, "1.0") if className == classNameV1_0 => 
//          SaveLoadV1_0.load(sc, path, algo, numNodes)        case _ => throw new Exception( 
//         s"DecisionTreeModel.load did not recognize model with (className, format version):" + 
//         s"($loadedClassName, $version).  Supported:\n" + 
//         s"  ($classNameV1_0, 1.0)") 
//    } 
  } 
} 
