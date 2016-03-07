package com.sir.elm

import scala.collection.mutable.HashMap

class SparserELMMatrix(rowNum: Int, columnNum: Int) extends Serializable{
  require(rowNum > 0 && columnNum > 0) 
  var elements = new HashMap[(Int, Int), Double]
  
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

  def insert(index: (Int, Int), value: Double) {
    elements += index -> value;
  }

  def speye(): SparserELMMatrix = {
    require(rowNum == columnNum)
    for(i <- 0 until rowNum){
      insert((i, i), 1.0)
    }
    this
  }
  
  def *(scalar: Double): SparserELMMatrix = {
    elements = elements.map{x => ((x._1._1, x._1._2), x._2 * scalar)}
    this
  }
  
  def /(scalar: Double): SparserELMMatrix = {
    require(scalar != 0.0)
    this * (1 / scalar)
    this
  }
}
