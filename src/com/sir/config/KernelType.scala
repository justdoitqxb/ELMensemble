package com.sir.config

object KernelType extends Enumeration{
  type KernelType = Value
  val Linear, Polynomial, Sigmoid, RBF, Wavelet = Value 
  private val symbol = Array.apply("linear", "Linear", "linearkernel", "LinearKernel", "polynomial", "Polynomial", "polynomialKernel", "PolynomialKernel", "sigmoid", "Sigmoid", "sigmoidKernel", "SigmoidKernel", "rbf", "RBF", "rbfKernel", "RBFKernel", "wavelet", "Wavelet", "waveletKernel", "WaveletKernel")
  
  private[sir] def fromString(name: String): KernelType = name match { 
    case "linear" | "Linear" | "linearkernel" | "LinearKernel" => Linear
    case "polynomial" | "Polynomial" | "polynomialKernel" | "PolynomialKernel" => Polynomial
    case "sigmoid" | "Sigmoid" | "sigmoidKernel" | "SigmoidKernel" => Sigmoid
    case "rbf" | "RBF" | "rbfKernel" | "RBFKernel" => RBF
    case "wavelet" | "Wavelet" | "waveletKernel" | "WaveletKernel" =>  Wavelet
    case _ => throw new IllegalArgumentException(s"Did not recognize KernelType name: $name. Symbol limitted in " + symbol.mkString("[", ",", "]"))
  } 
}