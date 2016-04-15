package com.sir.analysis

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import com.sir.util.ClassedPoint

object Stastics {
  def main(args: Array[String]){
    if (args.length < 2) { 
      println("Usage: [training file] [validation file]") 
      System.exit(1) 
    } 
    val conf = new SparkConf().setAppName("AFAlertBagging").setMaster("spark://Master:7077")
    val sc = new SparkContext(conf)
    val trainingData = sc.textFile("hdfs://172.17.0.2:9000" + args(0))
    val training = trainingData.map{ x => ClassedPoint.parse(x, true)}
    val validataionData = sc.textFile("hdfs://172.17.0.2:9000" + args(1))
    val validation = validataionData.map { ClassedPoint.parse }
         
    val splits = validation.randomSplit(Array(0.8, 0.2))
    val (trainDataPart, testData) = (splits(0), splits(1))
    val trainData = training ++ trainDataPart
    val pos = validation.filter { _.label == 0.0 }.count()
    val neg = validation.filter { _.label == 1.0 }.count()
    val total = validation.count
    val tpos = trainData.filter { _.label == 0.0 }.count()
    val tneg = trainData.filter { _.label == 1.0 }.count()
    val ttotal = trainData.count
    println("Validation total: " + total)
    println("Validation pos: " + pos)
    println("Validation neg: " + neg)
    println("Training total: " + ttotal)
    println("Training pos: " + pos)
    println("Training neg: " + neg)
    sc.stop()
  }
}