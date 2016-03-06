package com.sir.config
/*
 * Type of activation function: 
 *    'sigmoid' for Sigmoidal function
 *    'sin' for Sine function
 *    'hardlim' for Hardlim function
 *    'tanh" for tanh function
 */
object ActivationFuncType extends Enumeration{
  type ActivationFuncType = Value
  val Sigmoid, Tanh, Sin, Hardlim = Value 
  private val symbol = Array.apply("sigmoid", "Sigmoid", "tanh", "Tanh", "sin", "Sin", "hardlim", "Hardlim")
  
  private[sir] def fromString(name: String): ActivationFuncType = name match { 
    case "sigmoid" | "Sigmoid" => Sigmoid
    case "tanh" | "Tanh" => Tanh 
    case "sin" | "Sin" => Sin
    case "hardlim" | "Hardlim" => Hardlim
    case _ => throw new IllegalArgumentException(s"Did not recognize ELMType name: $name. Symbol limited in " + symbol.mkString("[", ",", "]")) 
  } 
}