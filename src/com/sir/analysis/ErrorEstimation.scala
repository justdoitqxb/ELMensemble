package com.sir.analysis

import org.apache.spark.rdd.RDD
import com.sir.util.ClassedPoint
import com.sir.util.Predictor

/**
 * Created by Qin on 2015. 12. 25..
 */
object ErrorEstimation {
  def estimateTrainingACC(labelAndPred: RDD[(Double, Double)]): Double = {
    labelAndPred.filter(r => r._1 == r._2).count.toDouble / labelAndPred.count()
  }
  
  def estimateError(labelAndPreds: RDD[(Double, Double)]): Unit = {
    val total = labelAndPreds.count()
    val pos = labelAndPreds.filter(r => 0.0 == r._2 )
    val positive = pos.count.toDouble
    val neg = labelAndPreds.filter(r => 1.0 == r._2 )
    val negitive = neg.count.toDouble
    val goodPred = labelAndPreds.filter(r => r._1 == r._2)
    val good = goodPred.count.toDouble
    val posgoodPred = labelAndPreds.filter(r => 0.0 == r._1 && 0.0 == r._2)
    val posgood = posgoodPred.count.toDouble
    val neggoodPred = labelAndPreds.filter(r => 1.0 == r._1 && 1.0 == r._2)
    val neggood = neggoodPred.count.toDouble
    val accRate = good / (total + 0.01)
    val sensitivity = posgood / (positive + 0.01)
    val specificity = neggood / (negitive + 0.01)
    println("Accuracy rate : " + accRate)
    println("Sensitivity: " + sensitivity)
    println("Specificity: " + specificity)
  }
}
