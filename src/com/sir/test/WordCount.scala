package com.sir.test

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext

object WordCount {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    conf.setAppName("WordCount").setMaster("local")
    val sc = new SparkContext(conf)
    val PREFIX: String = "C://Users//bsn//Scala//ELM_ensemble//data//"
    val fileName = "svd.txt"
    val data = sc.textFile(PREFIX + fileName, 1)
    val parts = data.flatMap{ line => line.split(" ") }.map { (_, 1) }.reduceByKey(_+_)
    val sort = parts.map{ pair => (pair._2, pair._1)}.sortByKey(false)
    sort.collect().foreach( pair => println(pair._1 + "," + pair._2))
    sc.stop()
  }
}