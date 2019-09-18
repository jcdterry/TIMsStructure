AddDirectScaledTIM <- function(J, TIM_Dens, TIMStrength){
  if(TIM_Dens==0){return(J)}
  
  S <- dim(J)[1]
  
  TIMs <- PotentialTIMs(J)
  
  ## Cut down TIM list to correct number
  # NumWantedTIM interactions  / NumPossibleTIM interactions
  ProbTIMIntPresent<- (TIM_Dens * S)  / nrow(TIMs)
  TIMs <- TIMs[rbinom( nrow(TIMs),1, prob=ProbTIMIntPresent)==1,  ]
  
  
  ## Draws TIMs from uniform distribution bounded by +/- mean strength of interaction
  
  TIMs$MeanInteractionStrength <-(abs(J[cbind(TIMs$prey, TIMs$pred)]) + abs(J[cbind(TIMs$pred, TIMs$prey)]))/2
  
  ## Standardises bounds to be twice the target mean magnitude
  Bounds<- TIMs$MeanInteractionStrength  *2*TIMStrength / mean(TIMs$MeanInteractionStrength)
  
  
  TIMs$EffectSize <- runif(nrow(TIMs), min = -Bounds, max = Bounds)
  
  ###########
  TIMEffectMatrix <- matrix(0, ncol=S, nrow= S)
  
  ## Have to deal with difficulty of multiple TIMs, potentially of different signs, acting via the same effective interaction
  # Assume additive effect of TIMs
  if(nrow(TIMs)>0){
    for(i in 1: nrow(TIMs)){# Cycle through TIMs
      TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] - TIMs$EffectSize[i]
      TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] + TIMs$EffectSize[i]
    }
  }
  J<- J + TIMEffectMatrix # Add TIM Effect
  return(J)
}
