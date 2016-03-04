package com.sir.util

import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.rdd.RDD.rddToPairRDDFunctions

object Gini {
  /**
   * Calculates Gini impurity.
   *
   * @param splits Splitted RDD with Boolean label.
   * @return Gini impurity value
   */
  def impurity(splits: RDD[(Boolean, LabeledPoint)]): Double = {
    val r1 = splits.groupByKey().map(split => {
      // one child.
      val groupsum = split._2.groupBy(_.label).values.foldLeft(0.0)((a, b) => a + b.size.toDouble * b.size) // split using labels.
      val sz = split._2.size
      val gini = 1.0 - groupsum / sz / sz
      (sz, gini * sz)
    })
    val total = r1.reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    total._2 / total._1
  }

  /**
   * Calculates Gini score about two classes
   *
   * @param c1
   * @param c2
   * @return
   */
  def nodeScore(c1: Double, c2: Double): Double = {
    val sqsum = c1 * c1 + c2 * c2
    val sum = c1 + c2

    1.0 - sqsum / sum / sum
  }

  def score(x: List[Int]): Double = {
    x.map(_.toDouble) match {
      case List(a, b, c, d) => {
        val s1 = nodeScore(a, c)
        val s2 = nodeScore(b, d)
        (s1 * (a + c) + s2 * (b + d)) / (a + b + c + d)
      }
      case _ => ???
    }
  }

  def parimpurity[T <: Iterable[Option[Boolean]]](splits: RDD[(T, Int)]): Iterable[Double] = {
    val scored = splits.map(x => {
      // FIXME: Currently only binary classification is supported
      val f = x._2 == 1
      x._1.map(yy => yy match {
        case Some(y) => List(f && y, f && !y, !f && y, !f && !y).map(if (_) 1 else 0)
        case None => List(0, 0, 0, 0)
      })
    })
    val scoresum = scored.reduce((x, y) => {
      x.zip(y).map(t => t._1.zip(t._2).map(u => u._1 + u._2))
    })

    val scores = scoresum.map(x => score(x))
    scores
  }
}