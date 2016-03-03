package com.sir.elm

import java.util.Random
import org.apache.spark.SparkContext
import org.apache.spark.mllib.linalg.SingularValueDecomposition
import org.apache.spark.mllib.linalg.Vectors
import org.apache.spark.mllib.linalg.distributed.RowMatrix
import org.apache.spark.mllib.linalg.Matrix

/*
 * Created by Qin on 2015. 12. 15..
 *
 * ELM矩阵，用于封装模型参数
 */
class ELMMatrix(rowNum: Int, columnNum: Int) extends Serializable{
  require(rowNum > 0 && columnNum > 0) 
  var matrix = Array.ofDim[Double](rowNum, columnNum)
  
  /*
   * 矩阵的行数
   */
  def rows(): Int = {
    rowNum
  }
  
  /*
   * 矩阵的列数
   */
  def columns(): Int = {
    columnNum
  }

  /*
   * 矩阵的行向量
   */
  def applyRow(i: Int): Array[Double] = {
    matrix(i)
  }
  
  def applyColumn(i: Int): Array[Double] = {
    for(elm <- matrix) yield elm(i)
  }

  /**
   * 构造0矩阵
   */
  def zeros(): ELMMatrix = {
    for (i <- 0 until rowNum) {
      for (j <- 0 until columnNum) {
        matrix(i)(j) = 0
      }
    }
    this
  }
  
   /**
   * 构造1矩阵
   */
  def ones(): ELMMatrix = {
    for (i <- 0 until rowNum) {
      for (j <- 0 until columnNum) {
        matrix(i)(j) = 1.0D
      }
    }
    this
  }

  /**
   * 随机初始化矩阵的值 from -1 to 1
   */
  def rand(): ELMMatrix = {
    val rand = new Random(42)
    for (i <- 0 until rowNum) {
      for (j <- 0 until columnNum) {
        matrix(i)(j) = rand.nextDouble
      }
    }
    this
  }

  /*
   * 给矩阵元素赋值
   */
  def set(i: Int, j: Int, value: Double) {
    matrix(i)(j) = value
  }

  /*
   * 矩阵某个位置的元素
   */
  def get(i: Int, j: Int): Double = {
    matrix(i)(j)
  }
  
  /*
   * 两个向量乘法
   */
  private def vecMultiply(v1: Array[Double], v2: Array[Double]): Double = { 
    require(v1.length == v2.length) 
    val temp = for((n1, n2) <- v1 zip v2) yield n1 * n2 
    temp.reduce(_+_) 
  }

  /*
   * 矩阵每个元素加上同一个数
   */
  def +(scalar: Double): ELMMatrix = {
    val newMat = new ELMMatrix(rows(), columns())
    for (i <- 0 until rowNum){
      for (j <- 0 until columnNum){
        newMat.matrix(i)(j) = matrix(i)(j) + scalar
      }
    }
    newMat
  }
  /*
   * 两个同型矩阵相加
   */
  def +(mat: ELMMatrix): ELMMatrix = {
    val newMat = new ELMMatrix(rows(), columns())
    require(rows() == mat.rows() && columns() == mat.columns())
    for (i <- 0 until rowNum){
      for (j <- 0 until columnNum){
        newMat.matrix(i)(j) = matrix(i)(j) + mat.get(i,j)
      }
    }
    newMat
  }

  def -(scalar: Double): ELMMatrix = {
    this + (-1.0 * scalar)
  }

  def -(other: ELMMatrix): ELMMatrix = {
    this + (other * (-1.0))
  }

  def *(scalar: Double): ELMMatrix = {
    val newMat = new ELMMatrix(columns(), rows())
    for (i <- 0 until rowNum){
      for (j <- 0 until columnNum){
        newMat.matrix(i)(j) = matrix(i)(j) * scalar
      }
    }
    newMat
  }
  
  def /(scalar: Double): ELMMatrix = {
    require(scalar != 0)
    this * (1 / scalar)
  }

  def *(mat: ELMMatrix): ELMMatrix = {
    require(columns() == mat.rows())
    var product = new ELMMatrix(rows(), mat.columns())
    for(row <- 0 until rows()){
      for(column <- 0 until mat.columns()){
        product.set(row, column, vecMultiply(applyRow(row), mat.applyColumn(column))) 
      }   
    }
    product
  }
  
  def **(mat: ELMMatrix): ELMMatrix = {
    require(rows() == mat.rows() && columns() == mat.columns())
    val newMat = new ELMMatrix(columns(), rows())
    for (i <- 0 until rows()) yield {
      for (j <- 0 until columns()) yield {
        newMat.matrix(i)(j) = matrix(i)(j) * mat.get(i,j)
      }
    }
    newMat
  }
  
  def **(vet: Array[Double], roworColumn: Boolean): ELMMatrix = {
    val newMat = new ELMMatrix(columns(), rows())
    if(roworColumn){
      require(columns() == vet.length)
      for (i <- 0 until rows()){
      for (j <- 0 until columns()){
        newMat.matrix(i)(j) = matrix(i)(j) * vet(j)
      }
    }
    }else{
      require(rows() == vet.length)
      for (i <- 0 until rows()){
        for (j <- 0 until columns()){
          newMat.matrix(i)(j) = matrix(i)(j) * vet(i)
        }
      }
    }
    newMat
  }
  
  def T(): ELMMatrix = {
    val newMat = new ELMMatrix(columns(), rows())
    for (i <- 0 until columns()){
      for (j <- 0 until rows()){
        newMat.set(i,j,get(j,i))
      }
    }
    newMat
  } 
  
  def printElement(){
    matrix.foreach(row => println(row.mkString(",")))
    println()
  }
}

object ELMMatrix {
  def power(mat: ELMMatrix, degree: Double): ELMMatrix = {
    val newMat = new ELMMatrix(mat.rows(), mat.columns())
    for (i <- 0 until mat.rows()){
      for (j <- 0 until mat.columns()){
        newMat.set(i, j, Math.pow(mat.get(i, j), degree))
      }
    }
    newMat
  }
  
  def exp(mat: ELMMatrix): ELMMatrix = {
    val newMat = new ELMMatrix(mat.rows(), mat.columns())
    for (i <- 0 until mat.rows()){
      for (j <- 0 until mat.columns()){
        newMat.set(i, j, Math.exp(mat.get(i, j)))
      }
    }
    newMat
  }
  
  def tanh(mat: ELMMatrix): ELMMatrix = {
    val newMat = new ELMMatrix(mat.rows(), mat.columns())
    for (i <- 0 until mat.rows()){
      for (j <- 0 until mat.columns()){
        newMat.set(i, j, Math.tanh(mat.get(i, j)))
      }
    }
    newMat
  }
  
  def cos(mat: ELMMatrix): ELMMatrix = {
    val newMat = new ELMMatrix(mat.rows(), mat.columns())
    for (i <- 0 until mat.rows()){
      for (j <- 0 until mat.columns()){
        newMat.set(i, j, Math.cos(mat.get(i, j)))
      }
    }
    newMat
  }
  
  def log(mat: ELMMatrix): ELMMatrix = {
    val newMat = new ELMMatrix(mat.rows(), mat.columns())
    for (i <- 0 until mat.rows()){
      for (j <- 0 until mat.columns()){
        newMat.set(i, j, Math.log(mat.get(i, j)))
      }
    }
    newMat
  }

  def sum(mat: ELMMatrix, dim: Int = 1): ELMMatrix = {
    val newMat = dim match{
      case 1 => new ELMMatrix(1, mat.columns())
      case 2 => new ELMMatrix(mat.rows(), 1)
      case _ => throw new IllegalArgumentException("Illegal param, Dim should be 1 or 2")
    }
    if(dim == 1){
      for(i <- 0 until mat.columns()){
        newMat.set(0, i, mat.applyColumn(i).reduce(_+_))
      }
    }else{
      for(i <- 0 until mat.columns()){
        newMat.set(i, 0, mat.applyRow(i).reduce(_+_))
      }
    }
    newMat
  }
  
  /*
   * Computes the Moore-Penrose pseudo inverse of the given real matrix .
 	 *
	 *     The pseudo inverse is nothing but the least-squares solution to AX=B,
   * hence:
   *        d/dX 1/2 (AX-B)^2 = A^T (AX-B)
   *  Solving A^T (AX-B) = 0 for X yields
   *        A^T AX = A^T B
   *     =>      X = (A^T A)^(-1) A^T B  
   */
  
  def pinv(mat: ELMMatrix, sc: SparkContext): ELMMatrix = {
    def SInvConstruct(V: Array[Double], p: Int): ELMMatrix = {
      val SS = new ELMMatrix(p, p)
      for(i <- 0 until p){
        SS.set(i, i, 1.0 / V(i))
      }
      SS
    }
    def UConstruct(U: RowMatrix, rows: Int, p: Int): ELMMatrix = {
      val UU = new ELMMatrix(rows, p)
      var i = 0
      for(row <- U.rows.collect()){
        val rowArr = row.toArray
        for(j <- 0 until p){
          UU.set(i, j, rowArr.apply(j))
        }
        i += 1
      }
      UU
    }
    def VConstruct(V: Matrix): ELMMatrix = {
      val VV = new ELMMatrix(V.numRows, V.numCols)
      for(i <- 0 until V.numRows){
        for(j <- 0 until V.numCols){
          VV.set(i, j, V.apply(i, j))
        } 
      }
      VV
    }
    val ELMMatrixtoRDD = sc.parallelize(mat.matrix, 1)
    val rowMatrix = new RowMatrix(ELMMatrixtoRDD.map(line => Vectors.dense(line)))
    val svd: SingularValueDecomposition[RowMatrix, Matrix] = rowMatrix.computeSVD(mat.columns, true)
//      svd.s.toArray.foreach(println)
//      svd.U.rows.foreach(println)
//      svd.V.toArray.foreach(println)
//      println()
//      println(svd.U.numRows())
//      println(svd.U.numCols())
//      println(svd.s.size)
//      println(svd.V.numRows)
//      println(svd.V.numCols)
    val p = svd.s.size
    VConstruct(svd.V) * SInvConstruct(svd.s.toArray, p) * (UConstruct(svd.U, mat.rows, p).T)
  }
}