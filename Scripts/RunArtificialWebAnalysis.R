RunArtificialWebAnalysis <- function(mux= -1,
                                     muy= 1,
                                     sigx= 0.5,
                                     sigy= 0.5,
                                     rhoxy=-2/3,
                                     C=0.2,
                                     Size =60,
                                     TIMStrength=0.5,
                                     Repeats= 10,
                                     Name='Unnamed',
                                     Models=c('TightRecip','Switching','BoundSw','Scaled',
                                              'Random', 'Normal','EcoEng', 'Unif',
                                              'Nearby', 'Far','Positive', 'Negative',   'LaPlace', 
                                              'IncSkew','DecSkew','RanSkew'),
                                     RowStruct=FALSE,
                                     FREQ_TO_TEST=c(1)
){
  
  data<- data.frame( 'obs' =NA,'May'=NA,'Tang'=NA,'rho'=NA,
                     'mu' =NA,
                     'Omega'=NA,'Omega_d2'=NA, 'Omega_d5'=NA,
                     'DotToRightOfDiskMay' =NA,
                     'DotToRightOfDiskTang'=NA, 
                     'V'=NA,
                     'cov'=NA,
                     'NT_VAR'= NA,
                     'C_VAR'=  NA,
                     'MeanMagNT'= NA,
                     'MeanNT'=  NA,
                     'Mean_C'= NA,
                     'NT_Conn'= NA,
                     'ID'=NA,'TIM_Model'=NA,
                     'Size'=NA, 'TIM_Dens'=NA,'TrophC'=NA,
                     'ActualTrophC'= NA,'TIMStrength'=NA,'Tot_Conn'=NA,
                     'rhoxy'=NA,'Reactivity'= NA,'AInHet' =NA,
                     'AOutHet'=NA,'ABothHet'=NA, 
                     'RowStructure'=NA, 'ColStructure'=NA)
  
  Bs<- lapply(1:Repeats,BuildWeb,Size,C,mux, muy,sigx, sigy, rhoxy)
  ActualTrophC <- map_dbl(Bs, Tot_Conn)
  
  for(TIM_Model in Models){
    print(TIM_Model)
    for(TIM_Dens in FREQ_TO_TEST){ # 
      try({
        
        As<- lapply(Bs,AddTIMsToMatrix,TIM_Model, TIM_Dens, TIMStrength)
        
        Cs <- map2(As, Bs, function(A,B){A-B})
        
        if(RowStruct){
          
          Bs<-map(Bs, function(M){M*rlnorm(dim(M)[1], -0.25, sqrt(0.5))})
          Cs<-map(Cs, function(M){M*rlnorm(dim(M)[1], -0.25, sqrt(0.5))})
          As <- map2(Bs, Cs, function(B,C){B+C})
        }
        
        Ap<- map_df(As, StabilityCriterion)
        Feasib <- map_df(As, CalcFeasEnv)
        RowColStrcut <-map_df(As, RowColStructure)
        Cstats <- map2_df(As, Bs, TIM_Dist_Examine)
        ADegrHet <- map_df(As, CalcDegHetero)
        
        data<-bind_rows(data, data.frame('obs' =Ap$obs, 
                                         'May'=Ap$May,
                                         'Tang'=Ap$Tang,
                                         'rho'=Ap$rho,
                                         'mu'=Ap$mu,
                                         'Omega'=Feasib$Omega,
                                         'Omega_d2'=Feasib$Omega_d2,
                                         'Omega_d5'=Feasib$Omega_d5,
                                         'DotToRightOfDiskMay' = Ap$DotToRightOfDiskMay, 
                                         'DotToRightOfDiskTang'=Ap$DotToRightOfDiskTang, 
                                         'V'=Ap$V,
                                         'cov'=  Cstats$Covariance,
                                         'NT_VAR'=  Cstats$NT_VAR,
                                         'C_VAR'=  Cstats$C_VAR,
                                         'MeanMagNT'=  Cstats$MeanMagNT,
                                         'MeanNT'=  Cstats$MeanNT,
                                         'Mean_C'=  Cstats$Mean_C,
                                         'NT_Conn'= Cstats$NT_Conn,
                                         'ID'= 1:Repeats,
                                         'TIM_Model'=TIM_Model,
                                         'Size'=Size,
                                         'TIM_Dens'=TIM_Dens,
                                         'TrophC'=C, 
                                         'ActualTrophC'= ActualTrophC,
                                         'TIMStrength' = TIMStrength,
                                         'Tot_Conn'= map_dbl(As, Tot_Conn), 
                                         'rhoxy' =rhoxy,
                                         'Reactivity'= map_dbl(As, Reactivity),
                                         'AInHet' =ADegrHet$In,
                                         'AOutHet'=ADegrHet$Out,
                                         'ABothHet'=ADegrHet$Both,
                                         'RowStructure'=RowColStrcut$RowHet,
                                         'ColStructure'=RowColStrcut$ColHet))
      })
    }
  }
  data<- data[-1,]
  
  save(data,file = paste0(Name,'DataFrame' )) 
  return(data)
}