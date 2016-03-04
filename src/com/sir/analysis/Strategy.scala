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

case class ELMStrategy(flag: StrategyType) extends Strategy{

//  def isMulticlassClassification: Boolean = { 
//    algo == Classification
//  } 
//
//  def isMulticlassWithCategoricalFeatures: Boolean = { 
//    isMulticlassClassification && (categoricalFeaturesInfo.size > 0) 
//  } 
//
//  /** 
//   * Sets Algorithm using a String. 
//   */ 
//  def setAlgo(algo: String): Unit = algo match { 
//    case "Classification" => setAlgo(Classification) 
//    case "Regression" => setAlgo(Regression) 
//  } 
//
//  /** 
//   * Check validity of parameters. 
//   * Throws exception if invalid. 
//   */ 
//  override def assertValid(): Unit = { 
//     algo match { 
//       case Classification => 
//         require(numClasses >= 2, 
//         s"DecisionTree Strategy for Classification must have numClasses >= 2," + 
//         s" but numClasses = $numClasses.") 
//       require(Set(Gini, Entropy).contains(impurity), 
//         s"DecisionTree Strategy given invalid impurity for Classification: $impurity." + 
//         s"  Valid settings: Gini, Entropy") 
//       case Regression => 
//         require(impurity == Variance, 
//           s"DecisionTree Strategy given invalid impurity for Regression: $impurity." + 
//           s"  Valid settings: Variance") 
//       case _ => 
//         throw new IllegalArgumentException( 
//          s"DecisionTree Strategy given invalid algo parameter: $algo." + 
//          s"  Valid settings are: Classification, Regression.") 
//    } 
//     require(maxDepth >= 0, s"DecisionTree Strategy given invalid maxDepth parameter: $maxDepth." + 
//      s"  Valid values are integers >= 0.") 
//    require(maxBins >= 2, s"DecisionTree Strategy given invalid maxBins parameter: $maxBins." + 
//      s"  Valid values are integers >= 2.") 
//    require(minInstancesPerNode >= 1, 
//      s"DecisionTree Strategy requires minInstancesPerNode >= 1 but was given $minInstancesPerNode") 
//    require(maxMemoryInMB <= 10240, 
//      s"DecisionTree Strategy requires maxMemoryInMB <= 10240, but was given $maxMemoryInMB") 
//    require(subsamplingRate > 0 && subsamplingRate <= 1, 
//      s"DecisionTree Strategy requires subsamplingRate <=1 and >0, but was given " + 
//      s"$subsamplingRate") 
//  } 
}
case class ELMEnsembleStrategy(flag: StrategyType) extends Strategy{
  
}

object Strategy { 
  /** 
   * Construct a default set of parameters for [[com.sir.elm.ELM]] or [[com.sir.elmensemble.ELMEnsemble]] according to flag
   * @param flag  "ELM" or "ELMEnsemble"
   * @param elmType  "Classification" or "Regression" 
   */ 
  def defaultELMStrategy(flag: StrategyType, elmType: ELMType): Strategy = flag match { 
//    case StrategyType.ELM => 
//      new Strategy(algo = Classification, impurity = Gini, maxDepth = 10, 
//        numClasses = 2) 
//    case StrategyType.ELMEnsemble => 
//      new Strategy(algo = Regression, impurity = Variance, maxDepth = 10, 
//        numClasses = 0) 
  } 
  
  /** 
   * Construct a set of parameters for [[com.sir.elm.ELM]] or [[com.sir.elmensemble.ELMEnsemble]] according to flag
	 */
  
  def generateStrategy(flag: StrategyType, elmType: ELMType): Strategy = {
    defaultELMStrategy(flag, elmType)
  }
} 