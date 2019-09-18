StabilityCriterion <- function(J){
  
  # adapted from eyeball::eye.approximate.ReL1 (Allesina et al)
  S <- dim(J)[1]
  N <- S * (S - 1)
  d <- mean(diag(J))
  diag(J) <- 0
  mu <- sum(J)/N
  var <- sum(J^2)/N - mu^2
  rho <- (sum(J * t(J))/N - mu^2)/var
  
  
  Dot<- ((S - 1) * mu) +  d
  MayDisk <- sqrt((S - 1) * var) - mu +  d
  TangDisk <- sqrt((S - 1) * var) * (1 + rho) - mu + d
  
  May <- max(Dot , MayDisk)
  Tang <- max(Dot,  TangDisk)
  
  eigens <- eigen(J, only.values = TRUE, symmetric = FALSE)$values
  obs <- max(Re(eigens))
  
  
  return(list('obs'=obs ,
              'May'=May,
              'Tang' = Tang,
              'S' = S, 
              'V' = var, 
              'mu' = mu, 
              'rho' = rho,
              'Tot_Conn' = Tot_Conn(J),
              'DotToRightOfDiskMay' = Dot>MayDisk, 
              'DotToRightOfDiskTang' = Dot>TangDisk))
}
