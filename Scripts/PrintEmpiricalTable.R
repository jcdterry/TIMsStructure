PrintEmpiricalTable  <- function(Location, Empirical_data){
  
  ROWORDER <-  c('Normal','Random' , 
                 'Nearby',  'Far' , 'EcoEng', 
                 'Positive', 'Negative','Unif' ,'LaPlace', 'Scaled' ,
                 'TightRecip', 'Switching','BoundSw' ,
                 'RanSkew' ,
                 'IncSkew' ,'DecSkew')
  
  LABELS<- c('Normally\nDistributed Strengths',
             'Random NTEs',
             'Nearby TIMs', 'Far TIMs',    'Heterogenous TIM Out-degree',
             'Facilitating Modifications', 'Interfering Modifications', 
             'Uniformally\nDistributed Strengths',
             'Laplace\nDistributed Strengths',  
             'Scaled Strengths',
             'Reciprocal Modifications',
             'Reciprocal Interference',
             'Bounded Reciprocal Interference',
             'Unbalanced NTEs', 
             'Larger Positive NTEs', 'Larger Negative NTEs')
  
  VARLIST<- c ("rho" ,"mu","V", "cov","C_VAR",
               "NT_Conn","Tot_Conn",
               "AInHet","AOutHet",
               "RowStructure" ,"ColStructure" )
  
  L <-Location 
  try(dataEmp$TIM_Model <- relevel(factor(dataEmp$TIM_Model),ref = 'Normal'))
  
  filter(dataEmp, Location==L) %>%
    filter(TIM_Dens ==10) %>%
    group_by(TIM_Model) %>%
    summarise(rho=signif(mean(rho, na.rm=TRUE),3),
              mu=signif(mean(mu, na.rm=TRUE),5),
              V=signif(mean(V, na.rm=TRUE),3),
              cov=signif(mean(cov, na.rm=TRUE),3),
              C_VAR=signif(mean(C_VAR, na.rm=TRUE),3),
              NT_Conn=signif(mean(NT_Conn, na.rm=TRUE),3),
              Tot_Conn=signif(mean(Tot_Conn, na.rm=TRUE),3),
              AInHet=round(mean(AInHet, na.rm=TRUE),4),
              AOutHet=round(mean(AOutHet, na.rm=TRUE),4),
              RowStructure=signif(mean(RowStructure, na.rm=TRUE),4),
              ColStructure=signif(mean(ColStructure, na.rm=TRUE),4))%>%
    gather('Column','Mean', rho:ColStructure )  %>%
    mutate(TIM_Model = factor(TIM_Model, levels = ROWORDER, labels = LABELS))%>%
    arrange(TIM_Model) %>%
    spread(Column, Mean) %>%
    dplyr::select(TIM_Model,
                  Tot_Conn	,
                  NT_Conn,
                  V,
                  C_VAR,
                  mu,
                  cov,
                  rho,
                  RowStructure,
                  ColStructure,
                  AInHet,
                  AOutHet)%>%
    rename( `TIM Model` =TIM_Model,
            `Total`=Tot_Conn	,
            `Non-Trophic`=NT_Conn,
            `Total`=V,
            `Non-Trophic`=C_VAR,
            `Mean Interaction Strength`= mu,
            `Trophic : Non-Trophic Covariance `=cov,
            `Pairwise Correlation`=rho,
            `Row Structure`=RowStructure,
            `Column Structure`=ColStructure,
            `In-degree`=AInHet,
            `Out-degree`=AOutHet)%>%
    kable(format = "html", escape = FALSE ,align = 'lccccccccccc', 
          caption = paste('Table showing structural properties of ', 
                          L,
                          'network with added TIMs.')) %>%  
    add_header_above(c(" ",
                       "Connectance" = 2, 
                       "   Variance  " = 2, 
                       ' '= 3,
                       "Structuring" = 2,
                       'Total Degree Heterogeneity'=2)) %>% print
  
}