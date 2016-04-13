package com.sir.util
import java.util.StringTokenizer
import scala.collection.mutable.ArrayBuilder

/**
 * Generic Predictor provides predict.
 * Created by Qin on 2015. 12. 15..
 */
case class ClassedPoint(label: Double, features: Array[Double]) {
  override def toString: String = {
    "ClassedPoint(%s, %s)".format(label, features.mkString("[", ", ", "]"))
  }
}

object ClassedPoint { 
  private def parseDouble(x: String): Double = {
    x match{
      case "NAN" => Double.NaN
      case x => x.toDouble        
    }
  }
  /** 
   * Parses a string resulted from toString into [ClassedPoint]
   */ 
  def parse(s: String): ClassedPoint = { 
    if (s.startsWith("[") | s.startsWith("(")) { 
      val tokenizer = new StringTokenizer(s, "[],", false)
      val label = parseDouble(tokenizer.nextToken()) 
      val values = ArrayBuilder.make[Double]
      while(tokenizer.hasMoreTokens()){
        values += parseDouble(tokenizer.nextToken())
      }
      ClassedPoint(label, values.result())
   } else {
     val parts = s.split(",")
     val label = parseDouble(parts.head) 
     val features = parts.tail.map(parseDouble)
     ClassedPoint(label, features) 
   } 
  } 
  def parse(s: String, flag: Boolean = true): ClassedPoint = { 
    val parts = s.split(",")
    val label = parseDouble(parts.head) - 1.0
    val features = parts.tail.map(parseDouble)
    ClassedPoint(label, features) 
  } 
} 
