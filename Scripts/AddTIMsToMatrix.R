AddTIMsToMatrix<- function(J,TIM_Model,TIM_Dens, TIMStrength ){
  
  # List when checked :
  
  if(TIM_Model=='Nearby'){    J<-   AddNearbyTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='Far'){   J<-   AddFarTIM(J, TIM_Dens, TIMStrength )  }  # :)
  if(TIM_Model=='Normal'){   J<-   AddRandomTIM(J, TIM_Dens, TIMStrength )  }  #:)
  if(TIM_Model=='Random'){    J<-   AddRandomInteractions(J, TIM_Dens, TIMStrength )  } #:)
  if(TIM_Model=='Positive'){   J<-   AddPositiveTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='Negative'){   J<-   AddNegativeTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='TightRecip'){   J<-   AddTightReciprocalTIM(J, TIM_Dens, TIMStrength )  }# :)
  if(TIM_Model=='Switching'){   J<-   AddSwitchingTIM(J, TIM_Dens, TIMStrength )  } #:)
  if(TIM_Model=='Logistic'){   J<-   AddRandomLogisticTIM(J, TIM_Dens, TIMStrength )  } # :) # best ignored
  if(TIM_Model=='LaPlace'){   J<-   AddRandomLaPlaceTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='Unif'){    J<-   AddUnifTIMs(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='Scaled'){   J<-   AddDirectScaledTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='EcoEng'){   J<-   AddEcoEngTIM(J, TIM_Dens, TIMStrength )  }  #  :)
  if(TIM_Model=='BoundSw'){   J<-   AddBoundSwitchTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='IncSkew'){   J<-   AddIncreaseSkewTIM(J, TIM_Dens, TIMStrength )  } #  :)
  if(TIM_Model=='DecSkew'){   J<-   AddDeceaseSkewTIM(J, TIM_Dens, TIMStrength )  } # :)
  if(TIM_Model=='RanSkew'){   J<-   AddRandSkewTIM(J, TIM_Dens, TIMStrength )  }  # :)
  
  return(J) 
}