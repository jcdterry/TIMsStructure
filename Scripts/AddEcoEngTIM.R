AddEcoEngTIM <- function(J, TIM_Dens, TIMStrength ){
  if(TIM_Dens==0){return(J)}
  
  ## In this set up species each have different probabilityies of being a modifier
  ## This represents certain species have an outsized contribution to the TIMs in the community
  # Signs of TIMs are randomly allocated.
  
  S <- dim(J)[1]
  TIMs <- PotentialTIMs(J)
  TIMStrength <- (TIMStrength * sqrt(pi))/sqrt(2) # Needed as an adjustor for half-normal distribution

  
  ## Distribution of probabilities of TIMs follow a beta distribution  

  ## Cut down TIM list to correct number
  # NumWantedTIM interactions  / NumPossibleTIM interactions
  ProbTIMIntPresent<- (TIM_Dens * S)  / nrow(TIMs)
  
  # mean of a beta distrbution = a/(a/+b)
  # Given that need mean to equal ProbTIMIntPresent
  # Set a to always equal 1, so to maintain b is then defined as 
  b = (1/ProbTIMIntPresent)-1
  
 Eng_Value <- rbeta(S,1, b) # each species is assigned an eng value
  TIMs$Eng_Value <- Eng_Value[TIMs$mod]
  
  
  # Use Eng value to determine TIM presence:
  # Because this can greatly increase the variance in the number of TIMs with some 
  # draws of the eng values (CLT with moderate S is not that strong)
  # Introduce an additonal step to repeat draw if divergence v. large (>10%)
  SelectedTIMs <- TIMs[rbinom( nrow(TIMs),1, prob=TIMs$Eng_Value)==1,  ] 
  ExpNum<-(TIM_Dens * S)
  DrawnNum<- nrow(SelectedTIMs)
  Divergence <- abs((ExpNum-DrawnNum)/ExpNum)
  
  while(Divergence >0.10){
    Eng_Value <- rbeta(S,1, b) # each species is assigned an eng value
    TIMs$Eng_Value <- Eng_Value[TIMs$mod]
    SelectedTIMs <- TIMs[rbinom( nrow(TIMs),1, prob=TIMs$Eng_Value)==1,  ] 
    Divergence <- abs(((TIM_Dens * S)-nrow(SelectedTIMs))/(TIM_Dens * S))
  }
  
  TIMs<- SelectedTIMs
  
  
  TIMs$EffectSize <- rnorm(nrow(TIMs), mean = 0, sd = TIMStrength)
  TIMEffectMatrix <- matrix(0, ncol=S, nrow= S)
  ## Have to deal with difficulty of multiple TIMs, potentially of different signs, acting via the same effective interaction
  # Assume additive effect of TIMs
  if(nrow(TIMs)>0){
    for(i in 1: nrow(TIMs)){# Cycle through TIMs
      
      TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$prey[i], TIMs$mod[i]] + TIMs$EffectSize[i]
      TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] <- TIMEffectMatrix[TIMs$pred[i], TIMs$mod[i]] - TIMs$EffectSize[i]
      
    }
  }
  J<- J + TIMEffectMatrix # Add TIM Effect
  return(J)
}