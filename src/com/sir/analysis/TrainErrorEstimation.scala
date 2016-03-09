package com.sir.analysis

import org.apache.spark.rdd.RDD
import com.sir.util.ClassedPoint
import com.sir.util.Predictor

/**
 * Created by Qin on 2015. 12. 25..
 */
object TrainErrorEstimation {
  def estimateError(labelAndPreds: RDD[(Double, ClassedPoint)]) {
    val totalCount = labelAndPreds.count()
    val failCount = labelAndPreds.filter( x =>x._1 != x._2.label).count()

    val normalCount = totalCount - failCount
    val good = labelAndPreds.filter(r => r._1 == r._2).count()

    println("Total Count = " + totalCount)
    println("Fail to predict = " + failCount)
    println("Error rate = " + (1.0 - (good.toDouble / normalCount)))

    val predxy = (x:Int, y:Int) => labelAndPreds.filter(v => v._1 == x && v._2 == y).count()
    println("Label 0, Pred 0 : " + predxy(0, 0))
    println("Label 0, Pred 1 : " + predxy(0, 1))
    println("Label 1, Pred 0 : " + predxy(1, 0))
    println("Label 1, Pred 1 : " + predxy(1, 1))
  }
}
