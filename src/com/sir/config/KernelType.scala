package com.sir.config

object KernelType extends Enumeration{
  type KernelType = Value
  val LinearKernel, PolynomialKernel, SigmoidKernel, RBFKernel, WaveletKernel = Value 
  private val symbol = Array.apply("linear", "Linear", "linearkernel", "LinearKernel", "polynomial", "Polynomial", "polynomialKernel", "PolynomialKernel", "sigmoid", "Sigmoid", "sigmoidKernel", "SigmoidKernel", "rbf", "RBF", "rbfKernel", "RBFKernel", "wavelet", "Wavelet", "waveletKernel", "WaveletKernel")
  
  private[sir] def fromString(name: String): KernelType = name match { 
    case "linear" | "Linear" | "linearkernel" | "LinearKernel" => LinearKernel 
    case "polynomial" | "Polynomial" | "polynomialKernel" | "PolynomialKernel" => PolynomialKernel 
    case "sigmoid" | "Sigmoid" | "sigmoidKernel" | "SigmoidKernel" => SigmoidKernel 
    case "rbf" | "RBF" | "rbfKernel" | "RBFKernel" => RBFKernel
    case "wavelet" | "Wavelet" | "waveletKernel" | "WaveletKernel" =>  WaveletKernel 
    case _ => throw new IllegalArgumentException(s"Did not recognize KernelType name: $name. Symbol limitted in " + symbol.mkString("[", ",", "]"))
  } 
}