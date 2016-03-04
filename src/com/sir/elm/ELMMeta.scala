package com.sir.elm

import org.apache.spark.rdd.RDD
import com.sir.analysis.Strategy
import com.sir.analysis.Strategy._
import com.sir.analysis.ELMType._
import com.sir.analysis.ActivationFuncType._
import com.sir.util.ClassedPoint

/*
 * Initial the parameter used in the model training
 * 
 * @param numFeatures: number of feature 
 * @param numExamples: number of example per elm model used
 * @param numberofHiddenNode: node number of the hidden layer of elm
 * @param WAug: (W:b)
 * @param subFeatureSet: feature selected
 */

class ELMMeta(
  val flag: ELMType,
  val numFeatures: Int, 
  val numExamples: Long, 
  val numberofHiddenNode: Int,
  val WAug: ELMMatrix,
  val activationFunc: ActivationFuncType)extends Serializable { 
  //define parameter validation function
} 

object ELMMeta { 
  /** 
   * Construct a [ELMMeta] instance for this dataset and parameters.  
   */ 
  def buildMeta(input: RDD[ClassedPoint], strategy: Strategy): ELMMeta = { 
    val numFeatures = input.map(_.features.size).take(1).headOption.getOrElse { 
      throw new IllegalArgumentException(s"ELM requires size of input RDD > 0, " + s"but was given by empty one.") 
    } 
    val numExamples = input.count() 
    
    val numberofHiddenNode = 100
    val WAug: ELMMatrix = new ELMMatrix(numFeatures + 1, numberofHiddenNode).rand()
    val activationFunc: ActivationFuncType = strategy
    
//     val numFeaturesPerModel: Int = _featureSubsetStrategy match { 
//       case "all" => numFeatures 
//       case "sqrt" => math.sqrt(numFeatures).ceil.toInt 
//       case "log2" => math.max(1, (math.log(numFeatures) / math.log(2)).ceil.toInt) 
//       case "onethird" => (numFeatures / 3.0).ceil.toInt 
//     } 
     new ELMMeta() 
   }
} 