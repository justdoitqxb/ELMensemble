package com.sir.config

import org.apache.spark.rdd.RDD
/*
 * Initial the parameter used in the model training
 * 
 */

class ELMEnsembleMeta(
  val elmMeta: ELMMeta = null,
  val kernelELMMeta: KernelELMMeta = null)extends Serializable { 
  //define parameter validation function
} 

object ELMEnsembleMeta { 
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
     new ELMMeta(flag, numFeatures, numExamples, numberofHiddenNode, WAug, numClasses, activationFunc) 
   }
} 