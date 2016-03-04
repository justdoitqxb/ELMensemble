package com.sir.util
import java.util.StringTokenizer
import scala.collection.mutable.ArrayBuilder

/**
 * Generic Predictor provides predict.
 *
 * Created by Qin on 2015. 12. 15..
 */
case class ClassedPoint(label: Int, features: Array[Double]) {
  override def toString: String = {
    "ClassedPoint(%s, %s)".format(label, features.mkString("[", ", ", "]"))
  }
}

object ClassedPoint { 
  /** 
   * Parses a string resulted from toString into [ClassedPoint]
   */ 
  def parse(s: String): ClassedPoint = { 
    if (s.startsWith("[") | s.startsWith("(")) { 
      val tokenizer = new StringTokenizer(s, "[],", false)
      val label = java.lang.Integer.parseInt(tokenizer.nextToken()) 
      val values = ArrayBuilder.make[Double]
      while(tokenizer.hasMoreTokens()){
        values += java.lang.Double.parseDouble(tokenizer.nextToken())
      }
      ClassedPoint(label, values.result())
   } else {
     val parts = s.split(",")
     val label = java.lang.Integer.parseInt(parts.head) 
     val features = parts.tail.map(java.lang.Double.parseDouble)
     ClassedPoint(label, features) 
   } 
 } 
} 
