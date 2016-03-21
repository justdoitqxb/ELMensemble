package com.sir.util

object KitBox {
  def maxPosition(arr: Array[Double]): Double = {
    require(arr.length > 0)
    var maxValue = arr.apply(0)
    var maxPosition = 0
    for(i <- 1 until arr.length){
      if(arr.apply(i) > maxValue){
        maxValue = arr.apply(i)
        maxPosition = i
      }
    }
    maxPosition.toDouble
  }
}