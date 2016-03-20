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

case class SigmoidActivation(flag: ActivationFuncType = ActivationFuncType.Sigmoid) extends ActivationFunc{
  override def active(mat: ELMMatrix): ELMMatrix = {
    ELMMatrix.sigmod(mat)
  }
}

case class TanhActivation(flag: ActivationFuncType = ActivationFuncType.Tanh) extends ActivationFunc{
  override def active(mat: ELMMatrix): ELMMatrix = {
    ELMMatrix.tanh(mat)
  }
}

case class SinActivation(flag: ActivationFuncType= ActivationFuncType.Sin) extends ActivationFunc{
  override def active(mat: ELMMatrix): ELMMatrix = {
    ELMMatrix.sin(mat)
  }
}

case class HardlimActivation(flag: ActivationFuncType = ActivationFuncType.Hardlim) extends ActivationFunc{
  override def active(mat: ELMMatrix): ELMMatrix = {
    ELMMatrix.hardlim(mat)
  }
}


object ActivationFunc {
  private val activeMethod = Array.apply("sigmoid", "sin", "tanh", "hardlim")
  /** 
   * Method to calculate activation function with different method 
   * 
   * @param flag --  active method that can be used. 
   * @param mat -- [ELMMatrix], the H matrix
   * 
   * @return [ELMMatrix] that presents corresponding active function. 
   */
  def calActiveFunc(
      flag: ActivationFuncType,
      mat: ELMMatrix): ELMMatrix = {
    flag match{
      case ActivationFuncType.Sigmoid => SigmoidActivation(flag).active(mat)
      case ActivationFuncType.Tanh => TanhActivation(flag).active(mat)
      case ActivationFuncType.Sin => SinActivation(flag).active(mat)
      case ActivationFuncType.Hardlim => HardlimActivation(flag).active(mat)
      case _ => throw new IllegalArgumentException("Activation should be in " + activeMethod.mkString("[", ", ", "]"))
    }
  } 
}