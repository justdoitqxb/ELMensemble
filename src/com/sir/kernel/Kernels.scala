package com.sir.kernel

import com.sir.elm.ELMMatrix

/*
 * Created by Qin on 2015. 12. 15..
 */

trait Kernel{
  def kernel(matrix_x: ELMMatrix, sample_x: ELMMatrix): ELMMatrix
}

case class LinearKernel(flag: String = "linear") extends Kernel{
  override def kernel(matrix_x: ELMMatrix, sample_x: ELMMatrix): ELMMatrix = {
    matrix_x * (sample_x.T) + 1.0
  }
}

case class PolynomialKernel(flag: String = "polynomial", k : Double = 1.0D, degree: Double = 2.0D) extends Kernel{
  override def kernel(matrix_x: ELMMatrix, sample_x: ELMMatrix): ELMMatrix = {
    ELMMatrix.power(matrix_x * (sample_x.T) + k,degree)
  }
}

case class SigmoidKernel(flag: String = "sigmoid", k: Double = 1.0D, delta: Double = 1.0D) extends Kernel{
  override def kernel(matrix_x: ELMMatrix, sample_x: ELMMatrix): ELMMatrix = {
    ELMMatrix.tanh(matrix_x * (sample_x.T) * k - delta)
  }
}

case class RBFKernel(flag: String = "rbf", sigma: Double = 2.0D) extends Kernel{
  override def kernel(matrix_x: ELMMatrix, sample_x: ELMMatrix): ELMMatrix = {
    val onesSample = new ELMMatrix(1, sample_x.rows()).ones
    val onesMatrix = new ELMMatrix(1, matrix_x.rows()).ones
    val xxh1 = ELMMatrix.sum(ELMMatrix.power(matrix_x, 2), 2) * onesSample
    val xxh2 = ELMMatrix.sum(ELMMatrix.power(sample_x, 2), 2) * onesMatrix
    val omega = xxh1 + (xxh2.T) - ((matrix_x * sample_x.T) * 2.0)
    ELMMatrix.exp(omega / (-2.0 * sigma * sigma))
  }
}

case class WaveletKernel(flag: String = "wavelet", k: Double = 1.0D, degree: Double = 2.0D, sigma: Double = 1.0D) extends Kernel{
  override def kernel(matrix_x: ELMMatrix, sample_x: ELMMatrix): ELMMatrix = {
    val onesSample = new ELMMatrix(1, sample_x.rows()).ones
    val onesMatrix = new ELMMatrix(1, matrix_x.rows()).ones
    val xxh1 = ELMMatrix.sum(ELMMatrix.power(matrix_x, 2), 2) * onesSample
    val xxh2 = ELMMatrix.sum(ELMMatrix.power(sample_x, 2), 2) * onesMatrix
    val omega = (xxh2.T) + xxh1 - ((matrix_x * sample_x.T) * 2.0)
    val omega1 = xxh1 - (xxh2.T)
    ELMMatrix.cos(omega1 / degree * k) * ELMMatrix.exp(omega / (-2.0 * sigma * sigma))
  }
}

object Kernel {
  private val kernelMethod = Array.apply("linear", "polynomial", "sigmoid", "rbf", "wavelet")
  /** 
   * Method to calculate kernel matrix with different method 
   * 
   * @param flag --  kernel method that can be used. 
   * @param matrix_x -- [ELMMatrix], the training dataSet
   * @param sample_x -- [ELMMatrix], the training dataSet or testing dataSet
   * @param k -- coefficient used within [PolynomialKernel,SigmoidKernel,WaveletKernel] ; Default value: 1.0D, 
   * @param degree -- coefficient used within [PolynomialKernel,WaveletKernel]; Default value: 2.0D, 
   * @param sigma -- coefficient used within [RBFKernel]; Default value: 1.0D,
   * @param delta -- coefficient used within [SigmoidKernel,WaveletKernel]; Default value:1.0D
   * 
   * @return [ELMMatrix] that presents corresponding kernel matrix. 
   */
  def calKernel(
      flag: String,
      matrix_x: ELMMatrix,
      sample_x: ELMMatrix,
      k: Double = 1.0D, 
      degree: Double = 2.0D, 
      sigma: Double = 1.0D,
      delta: Double = 1.0D): ELMMatrix = {
    flag match{
      case "linear" => LinearKernel(flag).kernel(matrix_x, sample_x)
      case "polynomial" => PolynomialKernel(flag, k, degree).kernel(matrix_x, sample_x)
      case "sigmoid" => SigmoidKernel(flag, k, delta).kernel(matrix_x, sample_x)
      case "rbf" => RBFKernel(flag, sigma).kernel(matrix_x, sample_x)
      case "wavelet" => WaveletKernel(flag, k, degree, sigma).kernel(matrix_x, sample_x)
      case _ => throw new IllegalArgumentException("Kernel used should be in " + kernelMethod.mkString("[", ", ", "]"))
    }
  } 
}