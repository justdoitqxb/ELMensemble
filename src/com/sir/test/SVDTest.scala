package com.sir.test 

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.linalg.distributed.RowMatrix
class SVDTest{
    
}

object SVDTest {
 
  def process(dataFile: String){
    def parseDouble(x: String): Double = {
      x match{
        case "NAN" => Double.NaN
        case x => x.toDouble        
      }
    }

    val sc = new SparkContext("local","svdtest")
    val svdMatrix = sc.textFile(dataFile).map(_.split(" "))
    val rowMatrix = new RowMatrix(svdMatrix.map(_.map(_.toDouble)).map(_.toArray).map(line => Vectors.dense(line)))
    val svd = rowMatrix.computeSVD(5, true, 0)
    svd.s.toArray.foreach(println)
    svd.U.rows.foreach(println)
    svd.V.toArray.foreach(println)
    sc.stop()
  }

  def main(args: Array[String]) {
    //System.setProperty("spark.executor.memory", "1g")
    //println("Usage: [file_path]" + args(0))
    val PREFIX: String = "C://Users//bsn//Scala//ELM_ensemble//data//"
    val fileName = "svd.txt"
    process(PREFIX + fileName)
  }
}