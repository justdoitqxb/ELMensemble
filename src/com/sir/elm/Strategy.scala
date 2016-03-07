package com.sir.elm

import com.sir.config.ELMType
import com.sir.config.KernelType
import com.sir.config.StrategyType
import com.sir.config.ActivationFuncType
import com.sir.config.ELMType._
import com.sir.config.KernelType._
import com.sir.config.StrategyType._
import com.sir.config.ActivationFuncType._
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
          s"DecisionTree Strategy given invalid flag parameter: $flag." + 
          s"  Valid settings are: Classification, Regression.") 
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
           s"DecisionTree Strategy for Classification must have numClasses >= 2," + 
           s" but numClasses = $numClasses.") 
       case ELMType.Regression => 
         require(true, 
           s"DecisionTree Strategy given invalid impurity for Regression:." + 
           s"  Valid settings: Variance") 
       case _ => 
         throw new IllegalArgumentException( 
          s"DecisionTree Strategy given invalid flag parameter: $flag." + 
          s"  Valid settings are: Classification, Regression.") 
    } 
  }
}

case class ELMEnsembleStrategy(
    flag: ELMType,
    numClasses: Int,
    RegularizationCoefficient: Double,
    kernelType: KernelType) extends Strategy{
  /** 
   * Check validity of parameters. 
   * Throws exception if invalid. 
   */ 
  override def assertValid(): Unit = { 
    flag match { 
       case ELMType.Classification => 
         require(numClasses >= 2, 
           s"DecisionTree Strategy for Classification must have numClasses >= 2," + 
           s" but numClasses = $numClasses.") 
       case ELMType.Regression => 
         require(true, 
           s"DecisionTree Strategy given invalid impurity for Regression:." + 
           s"  Valid settings: Variance") 
       case _ => 
         throw new IllegalArgumentException( 
          s"DecisionTree Strategy given invalid flag parameter: $flag." + 
          s"  Valid settings are: Classification, Regression.") 
    } 
  }
}

case class KernelELMEnsembleStrategy(
    flag: ELMType,
    numClasses: Int,
    RegularizationCoefficient: Double,
    kernelType: KernelType) extends Strategy{
  /** 
   * Check validity of parameters. 
   * Throws exception if invalid. 
   */ 
  override def assertValid(): Unit = { 
    flag match { 
       case ELMType.Classification => 
         require(numClasses >= 2, 
           s"DecisionTree Strategy for Classification must have numClasses >= 2," + 
           s" but numClasses = $numClasses.") 
       case ELMType.Regression => 
         require(true, 
           s"DecisionTree Strategy given invalid impurity for Regression:." + 
           s"  Valid settings: Variance") 
       case _ => 
         throw new IllegalArgumentException( 
          s"DecisionTree Strategy given invalid flag parameter: $flag." + 
          s"  Valid settings are: Classification, Regression.") 
    } 
  }
}

case class MixEnsembleStrategy(
    flag: ELMType,
    numClasses: Int,
    RegularizationCoefficient: Double,
    kernelType: KernelType) extends Strategy{
  /** 
   * Check validity of parameters. 
   * Throws exception if invalid. 
   */ 
  override def assertValid(): Unit = { 
    flag match { 
       case ELMType.Classification => 
         require(numClasses >= 2, 
           s"DecisionTree Strategy for Classification must have numClasses >= 2," + 
           s" but numClasses = $numClasses.") 
       case ELMType.Regression => 
         require(true, 
           s"DecisionTree Strategy given invalid impurity for Regression:." + 
           s"  Valid settings: Variance") 
       case _ => 
         throw new IllegalArgumentException( 
          s"DecisionTree Strategy given invalid flag parameter: $flag." + 
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
    case StrategyType.ELMEnsemble => 
      ELMEnsembleStrategy(ELMType.Classification, 2, (2^5).toDouble, KernelType.Linear) 
    case StrategyType.KernelELM =>
      KernelELMStrategy(ELMType.Classification, 2, (2^5).toDouble, KernelType.Linear)
    case StrategyType.KernelELMEnsemble =>
      KernelELMStrategy(ELMType.Classification, 2, (2^5).toDouble, KernelType.Linear)
    case StrategyType.MixEnsemble =>
      MixEnsembleStrategy(ELMType.Classification, 2, (2^5).toDouble, KernelType.Linear)
  } 
  
  /** 
   * Construct a set of parameters for [[com.sir.elm.ELM]] or [[com.sir.elmensemble.ELMEnsemble]] according to flag
	 */  
  def generateStrategy(
      flag: StrategyType, 
      elmType: ELMType, 
      activationFunc: ActivationFuncType, 
      numberofHiddenNode: Int,
      numClasses: Int): Strategy = flag match{
    case StrategyType.ELM => 
      ELMStrategy(elmType, numberofHiddenNode, numClasses, activationFunc) 
    case StrategyType.KernelELM => 
      KernelELMStrategy(ELMType.Classification, 2, 2^5, KernelType.Linear)
    case StrategyType.ELMEnsemble => 
      ELMEnsembleStrategy(ELMType.Classification, 2, 2^5, KernelType.Linear)
    case StrategyType.KernelELMEnsemble => 
      KernelELMEnsembleStrategy(ELMType.Classification, 2, 2^5, KernelType.Linear)
    case StrategyType.MixEnsemble => 
      MixEnsembleStrategy(ELMType.Classification, 2, 2^5, KernelType.Linear)
  }
} 