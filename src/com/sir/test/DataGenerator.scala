package com.sir.test

import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import java.util.Random
import scala.collection.mutable.Map
import com.sir.util.ClassedPoint


object DataGenerator {
  def generate(numExamples: Int, numFeatures: Int, numClasses: Int, sc: SparkContext): RDD[ClassedPoint] = {
    val rand = new Random(65656)
    val labels = Map.empty[Int, Double]
    (0 until numClasses).foreach(x => labels(x) = labels.getOrElse(x, rand.nextDouble() * 10.0))
    
    sc.parallelize(0 until numExamples).map { index => 
      val rand = new Random(66 + index)
      val label = new Random().nextInt(numClasses)
      val features = Array.fill[Double](numFeatures) { 
        rand.nextGaussian() * 2.0 + labels(label)
      } 
      ClassedPoint(label.toDouble, features) 
    } 
  }
  
//  def main(args: Array[String]) { 
////    if (args.length < 2) { 
////      println("Usage: DataGenerator +[sparkMaster] [output_fileName] [num_examples] [num_features]") 
////      System.exit(1) 
////    } 
////    val sparkMaster: String = args(0) 
////    val outputFileName: String = args(1) 
////    val numExamples: Int = if (args.length > 2) args(2).toInt else 10000 
////    val numFeatures: Int = if (args.length > 3) args(3).toInt else 2 
////    val numClasses: Int = if (args.length > 4) args(4).toInt else 2 
//    val sparkMaster: String = "local" 
//    val outputFileName: String = "elm.txt" 
//    val numExamples = 100 
//    val numFeatures = 10 
//    val numClasses = 2 
//    val PREFIX: String = "C://Users//bsn//Scala//ELM_ensemble//data//"
//    
//    val sc = new SparkContext(sparkMaster, "DataGenerator") 
//    val rand = new Random(65656)
//    val labels = Map.empty[Int, Double]
//    (0 until numClasses).foreach(x => labels(x) = labels.getOrElse(x, rand.nextDouble() * 10.0))
//    
//    val data: RDD[ClassedPoint] = sc.parallelize(0 until numExamples).map { index => 
//      val rand = new Random(66 + index)
//      val label = new Random().nextInt(numClasses)
//      val features = Array.fill[Double](numFeatures) { 
//        rand.nextGaussian() * 2.0 + labels(label)
//      } 
//      ClassedPoint(label, features) 
//    } 
//    //data.saveAsTextFile(PREFIX + outputFileName) 
//    data.collect().foreach { x => println(x.toString()) }
//    sc.stop() 
//  } 
}