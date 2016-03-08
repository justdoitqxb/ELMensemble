package com.sir.util

import org.apache.spark.rdd.RDD

/**
 * Generic Predictor provides predict.
 *
 * Created by Qin on 2015. 12. 15..
 */
trait Predictor {
  def predict(features: Array[Double]): Double
  def predict(features: RDD[Array[Double]]): RDD[ClassedPoint]
}
