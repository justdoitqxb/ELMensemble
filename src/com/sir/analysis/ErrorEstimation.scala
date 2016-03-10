package com.sir.analysis

import org.apache.spark.rdd.RDD
import com.sir.util.ClassedPoint
import com.sir.util.Predictor

/**
 * Created by Qin on 2015. 12. 25..
 */
object ErrorEstimation {
  def estimateError(labelAndPreds: RDD[(Double, Double)]): Unit = {
//    val total = labelAndPreds.count()
//    val fail = labelAndPreds.filter( x => x._1.toInt != x._2.label.toInt)
//    val good = labelAndPreds.filter(r => r._1.toInt == r._2.label.toInt)
    val accRate = labelAndPreds.filter(r => r._1 == r._2).count.toDouble / labelAndPreds.count()
    println("Accuracy rate : " + accRate)
//    println("Total Count = " + total + " Good: " + good)
//    println("Fail to predict = " + fail)
//    println("Error rate = " + (1.0 - (good.toDouble / total)))
  }
}
