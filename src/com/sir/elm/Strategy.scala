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
 *              [[com.sir.analysis.ELMType.Classification]], 
 *              [[com.sir.analysis.ELMType.Regression]] 
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
    activationFunc: ActivationFuncType,
    numberofHiddenNode: Int = 100) extends Strategy{

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

case class ELMEnsembleStrategy(
    flag: ELMType,
    numClasses: Int) extends Strategy{
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
  def defaultELMStrategy(flag: StrategyType, elmType: ELMType, activationFunc: ActivationFuncType): Strategy = flag match { 
    case StrategyType.ELM => 
      ELMStrategy(elmType, activationFunc) 
    case StrategyType.ELMEnsemble => 
      ELMEnsembleStrategy(flag = elmType, numClasses = 0) 
  } 
  
  /** 
   * Construct a set of parameters for [[com.sir.elm.ELM]] or [[com.sir.elmensemble.ELMEnsemble]] according to flag
	 */  
  def generateStrategy(flag: StrategyType, elmType: ELMType, activationFunc: ActivationFuncType): Strategy = {
    defaultELMStrategy(flag, elmType, activationFunc)
  }
  

} 