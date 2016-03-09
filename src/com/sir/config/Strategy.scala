package com.sir.config

import scala.util.Random
import ELMType._
import KernelType._
import StrategyType._
import ClassifierType._
import ActivationFuncType._
/** 
 * configuration options for elm & elm ensemble construction 
 * @param elmType  Learning goal.  Supported: 
 *              [[com.sir.config.ELMType.Classification]], 
 *              [[com.sir.config.ELMType.Regression]] 
 * @param numClasses Number of classes for classification. Default value is 2 (binary classification). 
 * @param subsamplingRate Fraction of the training data used for learning elm model. 
 */ 
trait Strategy{
  def assertValid: Unit
}
/** 
 * Stores all the configuration options 
 * @param flag: Learning goal.  Supported: 
 *              [[Classification]], 
 *              [[Regression]] 
 * @param numberofHiddenNode: The node number of the hidden layer
 * @param activationFunc: The activate function type of the hidden nodes
 * @param numClasses: Number of classes for classification. 
 */
case class ELMStrategy(
    flag: ELMType,
    numberofHiddenNode: Int,
    numClasses: Int,
    activationFunc: ActivationFuncType) extends Strategy{

  def isClassification: Boolean = { 
    flag == ELMType.Classification
  } 

  /** 
   * Check validity of parameters. 
   * Throws exception if invalid. 
   */ 
  override def assertValid(): Unit = { 
    flag match { 
       case ELMType.Classification => require(true, "test")
       case ELMType.Regression => 
         require(true, "test")
       case _ => throw new IllegalArgumentException( 
          s"Strategy given invalid flag parameter: $flag." + 
          s"Valid settings are: Classification, Regression.") 
    } 
  }
}

case class KernelELMStrategy(
    flag: ELMType,
    numClasses: Int,
    regularizationCoefficient: Double,
    kernelType: KernelType) extends Strategy{
  /** 
   * Check validity of parameters. 
   * Throws exception if invalid. 
   */ 
  override def assertValid(): Unit = { 
    flag match { 
       case ELMType.Classification => 
         require(numClasses >= 2, 
           s"Strategy for Classification must have numClasses >= 2," + 
           s" but numClasses = $numClasses.") 
       case ELMType.Regression => 
         require(true, 
           s"Strategy given invalid impurity for Regression:." + 
           s"  Valid settings: Variance") 
       case _ => 
         throw new IllegalArgumentException( 
          s"Strategy given invalid flag parameter: $flag." + 
          s"  Valid settings are: Classification, Regression.") 
    } 
  }
}

case class ELMEnsembleStrategy(
    classifierType: ClassifierType,
    flag: ELMType,
    numELM: Int,
    numClasses: Int,    
    numberofHiddenNode: Int = 100,
    activationFunc: ActivationFuncType = ActivationFuncType.Sigmoid,
    regularizationCoefficient: Double = (2^5).toDouble,
    kernelType: KernelType = KernelType.Sigmoid) extends Strategy{
  /** 
   * Check validity of parameters. 
   * Throws exception if invalid. 
   */ 
  override def assertValid(): Unit = { 
    flag match { 
       case ELMType.Classification => 
         require(numClasses >= 2, 
           s"Strategy for Classification must have numClasses >= 2," + 
           s" but numClasses = $numClasses.") 
       case ELMType.Regression => 
         require(true, 
           s"Strategy given invalid impurity for Regression:." + 
           s"  Valid settings: Variance") 
       case _ => 
         throw new IllegalArgumentException( 
          s"Strategy given invalid flag parameter: $flag." + 
          s"  Valid settings are: Classification, Regression.") 
    } 
  }
}

object Strategy { 
  /** 
   * Construct a default set of parameters for [[com.sir.elm.ELM]] or [[com.sir.elmensemble.ELMEnsemble]] according to flag
   * @param flag  "ELM" or "ELMEnsemble"
   * @param elmType  "Classification" or "Regression" 
   */ 
  def defaultStrategy(flag: StrategyType): Strategy = flag match {
    case StrategyType.ELM => 
      ELMStrategy(ELMType.Classification, 100, 2, ActivationFuncType.Sigmoid) 
    case StrategyType.KernelELM =>
      KernelELMStrategy(ELMType.Classification, 2, (2^5).toDouble, KernelType.Linear)
    case StrategyType.ELMEnsemble => 
      ELMEnsembleStrategy(ClassifierType.Mix, ELMType.Classification,3, 2) 
    case _ => 
      throw new IllegalArgumentException(s"given unsupported parameter")
  } 
  
  /** 
   * Construct a set of parameters for [[com.sir.elm.ELM]] or [[com.sir.elmensemble.ELMEnsemble]] according to flag
	 */  
  def generateStrategy(
      flag: StrategyType,
      elmType: ELMType,
      numClasses: Int,
      numELM: Int = 3,
      classifierType: ClassifierType = ClassifierType.Mix,
      numberofHiddenNode: Int = 100,
      activationFunc: ActivationFuncType = ActivationFuncType.Sigmoid,
      regularizationCoefficient: Double = (2^5).toDouble,
      kernelType: KernelType = KernelType.Sigmoid): Strategy = flag match{
    case StrategyType.ELM => 
      ELMStrategy(elmType, numberofHiddenNode, numClasses, activationFunc) 
    case StrategyType.KernelELM => 
      KernelELMStrategy(ELMType.Classification, 2, 2^5, KernelType.Linear)
    case StrategyType.ELMEnsemble => 
      ELMEnsembleStrategy(classifierType, elmType, numELM, numClasses, numberofHiddenNode, activationFunc, regularizationCoefficient, kernelType)
    case _ => 
      throw new IllegalArgumentException(s"given unsupported parameter")
  }
  
  def generateChildStrategy(strategy: Strategy): (ClassifierType, Strategy) = strategy match{
    case ELMEnsembleStrategy(classifierType, elmType, numELM, numClasses, numberofHiddenNode, activationFunc, regularizationCoefficient, kernelType) =>
      val newClassifierType = if(classifierType == ClassifierType.Mix){
        Random.nextInt(2) match{
          case 0 => ClassifierType.ELM
          case 1 => ClassifierType.KernelELM
        }
      }else{
        classifierType
      }
      newClassifierType match{
        case ClassifierType.ELM => (newClassifierType, ELMStrategy(elmType , numberofHiddenNode, numClasses, activationFunc))
        case ClassifierType.KernelELM =>(newClassifierType, KernelELMStrategy(ELMType.Classification, numClasses, regularizationCoefficient, kernelType))
      }
    case _ => throw new IllegalArgumentException(s"given unsupported parameter")
  }
  
  private def getClassifierType(strategy: Strategy): ClassifierType = strategy match {
    case ELMEnsembleStrategy(classifierType, elmType, numELM, numClasses, numberofHiddenNode, activationFunc, regularizationCoefficient, kernelType) =>
      classifierType
    case _ => throw new IllegalArgumentException(s"given unsupported parameter")
   }
} 