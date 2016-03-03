package com.sir.test

import com.sir.elm.ELMMatrix
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext

object ELMMatrixTest {
  def printElement(mat: ELMMatrix){
    mat.matrix.foreach(row => println(row.mkString(",")))
    println()
  }
  
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
    conf.setAppName("pinv").setMaster("local")
    val sc = new SparkContext(conf)
    
    val elmMat1 = new ELMMatrix(3,3).rand()
    printElement(elmMat1)  
    val elmMat2 = new ELMMatrix(3,3).ones()
    printElement(elmMat2) 
    val elmMat3 = new ELMMatrix(3,3).zeros()
    printElement(elmMat3) 
    val elmMat4 = elmMat3 + 1
    printElement(elmMat4)
    elmMat4 + 1
    printElement(elmMat3)
    println("rows: " + elmMat1.rows() + "columns: " + elmMat1.columns())
    println(elmMat1.applyRow(1).mkString(","))
    println(elmMat1.applyColumn(1).mkString(","))
    println()
    elmMat1.set(0,1,1.0)
    printElement(elmMat1)
    println(elmMat1.get(1,1))
    val elmMat5 = elmMat1.T
    printElement(elmMat5)
    printElement(elmMat2.**(Array(1.0D, 2.0D, 3.0D),false))
    printElement(elmMat2 ** elmMat2)
    printElement(elmMat2)
    printElement(ELMMatrix.log(ELMMatrix.exp(elmMat2)))
    elmMat1.set(0, 0, 0.7275636800328681);
    elmMat1.set(0, 1, 0.6832234717598454);
    elmMat1.set(0, 2, 0.30871945533265976);
    elmMat1.set(1, 0, 0.27707849007413665);
    elmMat1.set(1, 1, 0.6655489517945736);
    elmMat1.set(1, 2, 0.9033722646721782);
    elmMat1.set(2, 0, 0.36878291341130565);
    elmMat1.set(2, 1, 0.2757480694417024);
    elmMat1.set(2, 2, 0.46365357580915334);
    
    //printElement(ELMMatrix.pinv(elmMat1, sc))
    
    val elmMat6 = new ELMMatrix(4,5).zeros()
    elmMat6.set(0, 0, 1.0);
    elmMat6.set(0, 4, 2.0);
    elmMat6.set(1, 2, 3.0);
    elmMat6.set(3, 1, 4.0);
    //printElement(elmMat6.T * elmMat6)
    val mat = new ELMMatrix(40, 50).rand()
    printElement(ELMMatrix.pinv(mat, sc) * mat)
    sc.stop()
  }
}