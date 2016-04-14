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
    val positive = labelAndPreds.filter(r => 0.0 == r._2 ).count.toDouble
    val negitive = labelAndPreds.filter(r => 1.0 == r._2 ).count.toDouble
    val good = labelAndPreds.filter(r => r._1 == r._2).count.toDouble
    val posgood = labelAndPreds.filter(r => 1.0 == r._2 & 0.0 == r._2).count.toDouble
    val neggood = labelAndPreds.filter(r => 1.0 == r._2 & 1.0 == r._2).count.toDouble
    val accRate = good / (total + 0.01)
    val sensitivity = posgood / (positive + 0.01)
    val specificity = neggood / (negitive + 0.01)
    println("Accuracy rate : " + accRate)
    println("Sensitivity: " + sensitivity)
    println("Specificity: " + specificity)
  }
}
