AddRandomLogisticTIM <- function(J, TIM_Dens, TIMStrength ){
  if(TIM_Dens==0){return(J)}
  S <- dim(J)[1]
  
  if(TIMStrength == 0.5){
    TIMStrengthPARAM <- 0.361
  }else{
    TIMStrengthPARAM <- 0.361
    warning('Logistic TIM: Scale parameter needs to be estimated for TIM Strength ')
  }
  
  TIMs <- PotentialTIMs(J)

  ## Cut down TIM list to correct number
  # NumWantedTIM interactions  / NumPossibleTIM interactions
  ProbTIMIntPresent<- (TIM_Dens * S)  / nrow(TIMs)
  TIMs <- TIMs[rbinom( nrow(TIMs),1, prob=ProbTIMIntPresent)==1,  ]
  
   # NB Reparamertise SD in terms of scale parameter:
  TIMs$EffectSize <- rlogis(nrow(TIMs), location = 0, scale = TIMStrengthPARAM)
  
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