package com.sir.test

object Test {
  def main(args: Array[String]): Unit = {
    val arr = Array.apply("a", "b", "c")
    println("Test: " + arr.mkString("[", ", ", "]"))
  }
}