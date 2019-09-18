## Recursive calls to map for direct way to cycle over multiple inputs while printing progress

RunEmpiricalAnalysis<- function( LocationSet,TIMModelSet,TIM_Densities, Repeats,EmpiricalWebs, NAME  ){
  Empirical_data<- map_df(LocationSet, .f = MapOverLocation,TIMModelSet,TIM_Densities, Repeats,  EmpiricalWebs )
  save(file = NAME, Empirical_data)  
}

MapOverLocation<- function(Location,TIMModelSet,TIM_Densities, Repeats,  EmpiricalWebs){
  print(Location)
  map_df(TIMModelSet,.f =  MapOverModel, Location,TIM_Densities, Repeats,EmpiricalWebs)
}

MapOverModel <- function(TIM_Model,Location,TIM_Densities, Repeats,EmpiricalWebs){
  print(TIM_Model)
  map_df(TIM_Densities, .f = MapOverDensity,  Location,TIM_Model,TIMStrength,Repeats,EmpiricalWebs)
}

MapOverDensity <- function(TIM_Dens,Location,TIM_Model,TIMStrength,Repeats,EmpiricalWebs){
  map_df(1:Repeats,.f = MapOverRepeats, Location,TIM_Model,TIMStrength, TIM_Dens, EmpiricalWebs)
}


MapOverRepeats<- function(x,Location,TIM_Model,TIMStrength, TIM_Dens, EmpiricalWebs){
  
  j<-  which(EmpiricalWebs$Location ==  Location) ## Pluck out what j is
  TIMStrength= (EmpiricalWebs$MeanStr[j]/2)
  
  OrigAp <-   EmpiricalWebs$OrigAp[[j]]
  B<- EmpiricalWebs$J[[j]]
  A<- AddTIMsToMatrix(B,TIM_Model, TIM_Dens, TIMStrength)
  
  Ap<- StabilityCriterion(A)
  Feasib <- CalcFeasEnv_Emprical(A)
  ADegrHet <- CalcDegHetero(A)
  RowColStrcut <- RowColStructure(A)
  Cstats <- TIM_Dist_Examine(A,  B) 
  
  return(data.frame('Origobs'=OrigAp$obs,'OrigMay'= OrigAp$May,
                    'OrigTang'= OrigAp$Tang,  'Origrho'=OrigAp$rho,
                    'obs' =Ap$obs, 
                    'May'=Ap$May,
                    'Tang'=Ap$Tang,
                    'rho'=Ap$rho,
                    'mu'=Ap$mu,
                    'Omega'=Feasib$Omega,
                    'Omega_d100'=Feasib$Omega_d100,
                    'Omega_d500'=Feasib$Omega_d500,    
                    'Omega_d5000'=Feasib$Omega_d5000,
                    'Omega_d50000'=Feasib$Omega_d50000,
                    'V'=Ap$V,
                    'Location'=Location ,
                    'cov'=  Cstats$Covariance,
                    'NT_VAR'=  Cstats$NT_VAR,
                    'C_VAR'=  Cstats$C_VAR,
                    'MeanMagNT'=  Cstats$MeanMagNT,
                    'MeanNT'=  Cstats$MeanNT,
                    'Mean_C'=  Cstats$Mean_C,
                    'NT_Conn'= Cstats$NT_Conn,
                    'RepID' = x,
                    'TIM_Model'=TIM_Model,
                    'Size'=Ap$S,
                    'TIM_Dens'=TIM_Dens,
                    'ActualTrophC'= OrigAp$Tot_Conn,
                    'TIMStrength' = TIMStrength,
                    'Tot_Conn'= Tot_Conn(A),
                    'AInHet' =ADegrHet$In,
                    'AOutHet'=ADegrHet$Out,
                    'ABothHet'=ADegrHet$Both,
                    'RowStructure'=RowColStrcut$RowHet,
                    'ColStructure'=RowColStrcut$ColHet))  
}

