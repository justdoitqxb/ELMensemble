package com.sir.analysis

import ELMType._
import KernelType._
import StrategyType._
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

case class ELMStrategy(
    var flag: ELMType,
    var numClasses: Int) extends Strategy{

  def isClassification: Boolean = { 
    flag == Classification
  } 

  //Sets Algorithm using a String.  
  def setELMType(name: String): Unit = {
    flag = ELMType.fromString(name)
  }

  /** 
   * Check validity of parameters. 
   * Throws exception if invalid. 
   */ 
  override def assertValid(): Unit = { 
    flag match { 
       case Classification => 
         require(numClasses >= 2, 
           s"DecisionTree Strategy for Classification must have numClasses >= 2," + 
           s" but numClasses = $numClasses.") 
       case Regression => 
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
    var flag: ELMType,
    var numClasses: Int) extends Strategy{
  /** 
   * Check validity of parameters. 
   * Throws exception if invalid. 
   */ 
  override def assertValid(): Unit = { 
    flag match { 
       case Classification => 
         require(numClasses >= 2, 
           s"DecisionTree Strategy for Classification must have numClasses >= 2," + 
           s" but numClasses = $numClasses.") 
       case Regression => 
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
  def defaultELMStrategy(flag: StrategyType, elmType: ELMType): Strategy = flag match { 
    case StrategyType.ELM => 
      ELMStrategy(flag = elmType, numClasses = 2) 
    case StrategyType.ELMEnsemble => 
      ELMEnsembleStrategy(flag = elmType, numClasses = 0) 
  } 
  
  /** 
   * Construct a set of parameters for [[com.sir.elm.ELM]] or [[com.sir.elmensemble.ELMEnsemble]] according to flag
	 */
  
  def generateStrategy(flag: StrategyType, elmType: ELMType): Strategy = {
    defaultELMStrategy(flag, elmType)
  }
} 