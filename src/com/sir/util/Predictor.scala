package com.sir.util

import org.apache.spark.rdd.RDD

/**
 * Generic Predictor provides predict.
 * Created by Qin on 2015. 12. 15..
 */

// @param weight -- training accuracy, default value 1.0
trait Predictor {
  var weight: Double = 1.0
  def calOutput(features: Array[Double]): Array[Double]
  def predict(features: Array[Double]): Double
  def predict(features: RDD[Array[Double]]): RDD[ClassedPoint]
  
}
