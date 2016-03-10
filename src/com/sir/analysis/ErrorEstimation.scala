package com.sir.analysis

import org.apache.spark.rdd.RDD
import com.sir.util.ClassedPoint
import com.sir.util.Predictor

/**
 * Created by Qin on 2015. 12. 25..
 */
object ErrorEstimation {
  def estimateError(labelAndPreds: RDD[(Double, ClassedPoint)]) {
    val total = labelAndPreds.count()
    val fail = labelAndPreds.filter( x => x._1.toInt != x._2.label.toInt)
    val good = labelAndPreds.filter(r => r._1.toInt == r._2.label.toInt)
    labelAndPreds.collect().foreach(println)
    fail.collect().foreach(println)
    good.collect().foreach(println)
//    println("Total Count = " + total + " Good: " + good)
//    println("Fail to predict = " + fail)
//    println("Error rate = " + (1.0 - (good.toDouble / total)))
  }
}
