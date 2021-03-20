implied_vol = function(cash)
{
  library(RND)
  
  option = read.csv(file = "10500.csv")
  implied_10500 = vector(mode = "numeric")
  for (i in 1:NROW(option)) {
    
    rf = 0.06957/252
    t_expiry<- as.Date(as.character(option$Expiry[i]),  format="%d-%b-%Y")-
      as.Date(as.character(option$Date[i]),  format="%d-%b-%Y")
    t_expiry = as.numeric(as.data.frame(t_expiry[1]))  
    implied_10500[i] =  tryCatch((compute.implied.volatility(rf,t_expiry,option$Underlying.Value[i],
                                   option$Strike.Price[i],
                       call.price = option$Close[i],lower = 0,upper = 2,y = 0)),error = function(e) NA)
    
  }
  
  
  option = read.csv(file = "10000.csv")
  implied_10000 = vector(mode = "numeric")
  for (i in 1:NROW(option)) {
    
    rf = 0.06957/252
    t_expiry<- as.Date(as.character(option$Expiry[i]),  format="%d-%b-%Y")-
      as.Date(as.character(option$Date[i]),  format="%d-%b-%Y")
    t_expiry = as.numeric(as.data.frame(t_expiry[1]))  
    implied_10000[i] =  tryCatch((compute.implied.volatility(rf,t_expiry,option$Underlying.Value[i],
                                                             option$Strike.Price[i],
                                                             call.price = option$Close[i],lower = 0,upper = 2,y = 0)),error = function(e) NA)
  }
  
  
  option = read.csv(file = "10100.csv")
  implied_10100 = vector(mode = "numeric")
  for (i in 1:NROW(option)) {
    
    rf = 0.06957/252
    t_expiry<- as.Date(as.character(option$Expiry[i]),  format="%d-%b-%Y")-
      as.Date(as.character(option$Date[i]),  format="%d-%b-%Y")
    t_expiry = as.numeric(as.data.frame(t_expiry[1]))  
    implied_10100[i] =  tryCatch((compute.implied.volatility(rf,t_expiry,option$Underlying.Value[i],
                                                             option$Strike.Price[i],
                                                             call.price = option$Close[i],lower = 0,upper = 2,y = 0)),error = function(e) NA)
  }
  
  
  option = read.csv(file = "10300.csv")
  implied_10300 = vector(mode = "numeric")
  for (i in 1:NROW(option)) {
    
    rf = 0.06957/252
    t_expiry<- as.Date(as.character(option$Expiry[i]),  format="%d-%b-%Y")-
      as.Date(as.character(option$Date[i]),  format="%d-%b-%Y")
    t_expiry = as.numeric(as.data.frame(t_expiry[1]))  
    implied_10300[i] =  tryCatch((compute.implied.volatility(rf,t_expiry,option$Underlying.Value[i],
                                                             option$Strike.Price[i],
                                                             call.price = option$Close[i],lower = 0,upper = 2,y = 0)),error = function(e) NA)
  }
  
  
  option = read.csv(file = "10400.csv")
  implied_10400 = vector(mode = "numeric")
  for (i in 1:NROW(option)) {
    
    rf = 0.06957/252
    t_expiry<- as.Date(as.character(option$Expiry[i]),  format="%d-%b-%Y")-
      as.Date(as.character(option$Date[i]),  format="%d-%b-%Y")
    t_expiry = as.numeric(as.data.frame(t_expiry[1]))  
    implied_10400[i] =  tryCatch((compute.implied.volatility(rf,t_expiry,option$Underlying.Value[i],
                                                             option$Strike.Price[i],
                                                             call.price = option$Close[i],lower = 0,upper = 2,y = 0)),error = function(e) NA)
  }
  
  mean_i = as.data.frame(cbind(implied_10000,implied_10100,implied_10300,implied_10400,implied_10500))
  mean_implied = rowMeans(mean_i,na.rm = TRUE)
  
  out1 = cbind(as.character(option$Date),mean_implied)
  return(out1)
}