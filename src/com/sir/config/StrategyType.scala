package com.sir.config

object StrategyType extends Enumeration{
  type StrategyType = Value
  val ELM, KernelELM, ELMEnsemble, KernelELMEnsemble, MixEnsemble = Value 
  private val symbol = Array.apply("elm", "ELM", "elmensemble", "elmEnsemble", "ELMEnsemble", "kernelelm", "KernelELM", "kernelelmensemble", "KernelELMEnsemble", "mixensemble", "MixEnsemble")
  
  private[sir] def formString(name: String): StrategyType = name match { 
    case "elm" | "ELM" => ELM
    case "elmensemble" | "elmEnsemble" | "ELMEnsemble" => ELMEnsemble
    case "kernelelm" | "KernelELM" => KernelELM
    case "kernelelmensemble" | "KernelELMEnsemble" => KernelELMEnsemble
    case "mixensemble" | "MixEnsemble" => MixEnsemble
    case _ => throw new IllegalArgumentException(s"Did not recognize StrategyType name: $name, symbol limited in " + symbol.mkString("[", ",", "]")) 
  } 
}