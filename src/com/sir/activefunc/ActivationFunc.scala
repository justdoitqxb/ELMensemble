package com.sir.activefunc

import com.sir.elm.ELMMatrix
import com.sir.config.ActivationFuncType
import com.sir.config.ActivationFuncType._

/*
 * Created by Qin on 2015. 12. 15..
 */

trait ActivationFunc{
  def active(mat: ELMMatrix): ELMMatrix
}

case class SigmoidActivation(flag: ActivationFuncType = Sigmoid) extends ActivationFunc{
  override def active(mat: ELMMatrix): ELMMatrix = {
    mat + 1.0
  }
}

case class TanhActivation(flag: ActivationFuncType = Tanh) extends ActivationFunc{
  override def active(mat: ELMMatrix): ELMMatrix = {
    mat
  }
}

case class SinActivation(flag: ActivationFuncType= Sin) extends ActivationFunc{
  override def active(mat: ELMMatrix): ELMMatrix = {
    mat
  }
}

case class HardlimActivation(flag: ActivationFuncType = Hardlim) extends ActivationFunc{
  override def active(mat: ELMMatrix): ELMMatrix = {
    mat
  }
}


object ActivationFunc {
  private val activeMethod = Array.apply("sigmoid", "sin", "tanh", "hardlim")
  /** 
   * Method to calculate activation function with different method 
   * 
   * @param flag --  active method that can be used. 
   * @param mat -- [ELMMatrix], the H matrix
   * @param k -- coefficient used within [PolynomialKernel,SigmoidKernel,WaveletKernel] ; Default value: 1.0D, 
   * @param degree -- coefficient used within [PolynomialKernel,WaveletKernel]; Default value: 2.0D, 
   * @param sigma -- coefficient used within [RBFKernel]; Default value: 1.0D,
   * @param delta -- coefficient used within [SigmoidKernel,WaveletKernel]; Default value:1.0D
   * 
   * @return [ELMMatrix] that presents corresponding active function. 
   */
  def calActiveFunc(
      flag: ActivationFuncType,
      mat: ELMMatrix,
      k: Double = 1.0D, 
      degree: Double = 2.0D, 
      sigma: Double = 1.0D,
      delta: Double = 1.0D): ELMMatrix = {
    flag match{
      case ActivationFuncType.Sigmoid => SigmoidActivation(flag).active(mat)
      case ActivationFuncType.Tanh => TanhActivation(flag).active(mat)
      case ActivationFuncType.Sin => SinActivation(flag).active(mat)
      case ActivationFuncType.Hardlim => HardlimActivation(flag).active(mat)
      case _ => throw new IllegalArgumentException("Activation should be in " + activeMethod.mkString("[", ", ", "]"))
    }
  } 
}