package com.sir.elm

import org.apache.spark.rdd.RDD
import com.sir.config.ELMType
import com.sir.config.ELMType._
import com.sir.config.ActivationFuncType
import com.sir.config.ActivationFuncType._
import com.sir.util.ClassedPoint
import com.sir.config.KernelType
import com.sir.config.KernelType._

/*
 * Initial the parameter used in the model training
 * 
 * @param numFeatures: number of feature 
 * @param numExamples: number of example per elm model used
 * @param numClasses -- for classification
 * @param subFeatureSet: feature selected
 * @param regularizationCoefficient -- 2^n
 * @parma kernelType: Kernel method
 */

class KernelELMMeta(
  val flag: ELMType,
  val numFeatures: Int, 
  val numExamples: Int, 
  val numClasses: Int,
  val regularizationCoefficient: Double,
  val kernelType: KernelType)extends Serializable { 
  //define parameter validation function
} 

object KernelELMMeta { 
  /** 
   * Construct a [ELMMeta] instance for this dataset and parameters.  
   */ 
  def buildKernelMeta(input: RDD[ClassedPoint], strategy: Strategy): KernelELMMeta = { 
    val (flag, numClasses, regularizationCoefficient, kernelType) = strategy match {
      case KernelELMStrategy(eflag, enumClasses, eRegularizationCoefficient, ekernelType) => (eflag, enumClasses, eRegularizationCoefficient, ekernelType)
      case _ => throw new IllegalArgumentException(s"strategy not validate")
    }
    val numFeatures = input.map(_.features.size).take(1).headOption.getOrElse { 
      throw new IllegalArgumentException(s"ELM requires size of input RDD > 0, " + s"but was given by empty one.") 
    } 
    val numExamples = input.count().toInt   
    new KernelELMMeta(flag, numFeatures, numExamples, numClasses, regularizationCoefficient,kernelType) 
  }
} 