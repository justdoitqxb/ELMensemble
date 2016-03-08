package com.sir.util

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import scala.util.Random

object Splitter {

  /*
   * select m features in [0,M). M--total feature number
   */
  def randomIndices(m: Int, M: Int): Array[Int] = {
    if (m < M && m > 0) {
      var result = Array.fill(m)(0)
      var i = 0
      // select m no-repeat features
      while (i < m) {
        val n = Random.nextInt(M)
        if (!result.contains(n)) {
          result(i) = n
          i += 1
        }
      }
      result
    } else {
      var result = Array.fill(M)(0)
      for (i <- 0 until M) {
        result(i) = i
      }
      result
    }
  }
  
  /*
   * calculate the entropy of an array
   */
  def entropy(buf: Array[String]): Double = {
    val len = buf.length
    val invLog2 = 1.0 / Math.log(2)
    if (len > 1) {
      val invLen = 1.0 / len.toDouble
      var ent = 0.0

      for (v <- buf.distinct) {
        val p_v = buf.count(x => x == v).toDouble * invLen
        ent -= p_v * Math.log(p_v) * invLog2
      }
      ent
    } else {
      0.0
    }
  }
  
  /*
   * bootstrap sampling -- with repeat
   */
  def bootstrapSampling(dataSet: RDD[ClassedPoint], numSamples: Int): RDD[ClassedPoint] = {
    val fraction = numSamples.toDouble / dataSet.count().toDouble
    dataSet.sample(true, fraction)
  }
  
  /*
   * Sub sampling -- without repeat
   */
  def subSampling(dataSet: RDD[ClassedPoint], numSamples: Int): RDD[ClassedPoint] = {
    val all = dataSet.count()
    val fraction = if(all > numSamples.toInt) numSamples.toDouble / dataSet.count().toDouble else 1.0
    dataSet.sample(true, fraction)
  }
  
  def selectSubFeatures(dataSet: RDD[ClassedPoint], featureIndice: Array[Int]): RDD[ClassedPoint] = {
    val m = featureIndice.length
    dataSet.map { x => 
      var selectedFeatures = Array.fill(m)(0.0)
      for (j <- 0 until m) {
        selectedFeatures(j) = x.features(featureIndice(j))
      }
      ClassedPoint(x.label, selectedFeatures) }
  }
  /*
 	* calculate the entropy gain ratio of a numerical attribute 
  */
  def gainRatioNumerical(att_cat: Array[(String, String)], ent_cat: Double): (Double, Array[String]) = {
    val sorted = att_cat.map(line => (line._1.toDouble, line._2)).sortBy(_._1)
    val catValues = sorted.map(line => line._2)
    val attValues = sorted.map(line => line._1)
    if (attValues.distinct.length == 1) {
      (0, Array(""))
    } else {
      val invLog2 = 1.0 / math.log(2)
      val len = catValues.length
      val invLen = 1.0 / len.toDouble
      var maxInfoGain = 0.0
      var c = 1
      for (i <- 1 until len) {

        if (catValues(i - 1) != catValues(i)) {

          var infoGain = ent_cat
          infoGain -= i * invLen * entropy(catValues.take(i))
          infoGain -= (1 - i * invLen) * entropy(catValues.takeRight(len - i))

          if (infoGain > maxInfoGain) {
            maxInfoGain = infoGain
            c = i
          }
        }
      }
      if (attValues(c) == attValues.last) {
        (0, Array(""))
      } else {
        val p = c * invLen
        val ent_att = -p * math.log(p) * invLog2 - (1 - p) * math.log(1 - p) * invLog2
        val infoGainRatio = maxInfoGain / ent_att
        val splitPoint = (attValues(c - 1) + attValues(c)) * 0.5
        (infoGainRatio, Array(splitPoint.toString))
      }
    }
  }
  
  /*
  * calculate the entropy gain ratio of a categorical attribute 
  */
  def gainRatioCategorical(att_cat: Array[(String, String)], ent_cat: Double): (Double, Array[String]) = {
    val att = att_cat.map(_._1)
    val values = att.distinct
    if (values.length != 1) {
      var gain = ent_cat
      val invL = 1.0 / att_cat.length
      for (i <- values) {
        val cat_i = att_cat.filter(_._1 == i).map(_._2)
        gain -= cat_i.length * invL * entropy(cat_i)
      }

      val gainRatio = gain / entropy(att)
      (gainRatio, values)
    } else {
      (0, values)
    }
  }
  
  /*
  * calculate the majority in an array
  */
  def majority(buf: Array[String]): (String, Double) = {
    var major = buf(0)
    var majorityNum = 0
    for (i <- buf.distinct) {
      val tmp = buf.count(x => x == i)
      if (tmp > majorityNum) {
        majorityNum = tmp
        major = i
      }
    }
    (major, majorityNum.toDouble / buf.length)
  }
}