AddDeceaseSkewTIM <- function(J, TIM_Dens, TIMStrength ){
  if(TIM_Dens==0){return(J)}
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution
  
  S <- dim(J)[1]
  TIMs <- PotentialTIMs(J)
  
  ## Cut down TIM list to correct number
  # NumWantedTIM interactions  / NumPossibleTIM interactions
  ProbTIMIntPresent<- (TIM_Dens * S)  / nrow(TIMs)
  TIMs <- TIMs[rbinom( nrow(TIMs),1, prob=ProbTIMIntPresent)==1,  ]
  
  TIMs$EffectSize <- rnorm(nrow(TIMs), mean = 0, sd = TIMStrength)
  TIMEffectMatrix <- matrix(0, ncol=S, nrow= S)
  
  # Assume additive effect of TIMs
  if(nrow(TIMs)>0){
    
    for(i in 1: nrow(TIMs)){# Cycle through TIMs
      
      Effects<-c(abs(TIMs$EffectSize[i])*0.5 ,abs(TIMs$EffectSize[i])*-1.5 )
      ## Randomly reorder effects:
      Effects <- sample(Effects, size = 2, replace = FALSE)
      
      TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] + Effects[1] 
      TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] + Effects[2]
    }
    
  }
  J<- J + TIMEffectMatrix # Add TIM Effect
  return(J)
}