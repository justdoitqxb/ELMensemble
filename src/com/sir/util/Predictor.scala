package com.sir.util

/**
 * Generic Predictor provides predict.
 *
 * Created by Qin on 2015. 12. 15..
 */
trait Predictor {
  def predict(features: Array[Double]): Double
}
