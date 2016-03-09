package com.sir.config

import org.apache.spark.rdd.RDD
import com.sir.util.ClassedPoint
import com.sir.elm.ELMMatrix
import ELMType._
import ActivationFuncType._

/*
 * Initial the parameter used in the model training
 * 
 * @param numFeatures: number of feature 
 * @param numExamples: number of example per elm model used
 * @param numberofHiddenNode: node number of the hidden layer of elm
 * @param WAug: (W:b)
 * @param numClasses -- for classification
 * @param subFeatureSet: feature selected
 */

class ELMMeta(
  val flag: ELMType,
  val numFeatures: Int, 
  val numExamples: Int, 
  val numberofHiddenNode: Int,
  val WAug: ELMMatrix,
  val numClasses: Int,
  val activationFunc: ActivationFuncType)extends Serializable { 
  //define parameter validation function
} 

object ELMMeta { 
  /** 
   * Construct a [ELMMeta] instance for this dataset and parameters.  
   */ 
  def buildMeta(input: RDD[ClassedPoint], strategy: Strategy): ELMMeta = { 
    val (flag, numberofHiddenNode, numClasses, activationFunc) = strategy match {
      case ELMStrategy(eflag, enumberofHiddenNode, numClasses, eactivationFunc) => (eflag, enumberofHiddenNode, numClasses, eactivationFunc)
      case _ => throw new IllegalArgumentException(s"strategy not validate")
    }
    val numFeatures = input.map(_.features.size).take(1).headOption.getOrElse { 
      throw new IllegalArgumentException(s"ELM requires size of input RDD > 0, " + s"but was given by empty one.") 
    } 
    val numExamples = input.count().toInt 
    
    val WAug: ELMMatrix = new ELMMatrix(numberofHiddenNode, numFeatures + 1).rand()

    
//     val numFeaturesPerModel: Int = _featureSubsetStrategy match { 
//       case "all" => numFeatures 
//       case "sqrt" => math.sqrt(numFeatures).ceil.toInt 
//       case "log2" => math.max(1, (math.log(numFeatures) / math.log(2)).ceil.toInt) 
//       case "onethird" => (numFeatures / 3.0).ceil.toInt 
//     } 
     new ELMMeta(flag, numFeatures, numExamples, numberofHiddenNode, WAug, numClasses, activationFunc) 
   }
} 