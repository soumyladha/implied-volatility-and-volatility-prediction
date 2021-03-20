garch1 = function(NSE,t_ahead)
{
  library(quantmod)
  library(forecast)
  library(tseries)
  library(rugarch)
  library(zoo)
  
  date = row.names(NSE)
  
  NSE =  NSE[,4]
  NSE =  cbind(as.data.frame(date),as.data.frame(NSE))
  NSE = na.omit(NSE)
  colnames(NSE) = c("date","price")
  
  
  ######################################################################
  n1 = NROW(NSE)
  retn = log(NSE[2:n1,2]) - log(NSE[1:(n1-1),2])
  
  ######################################################################
  adf.test(retn)
  model = auto.arima(retn)
  a = model$arma[1]
  b = model$arma[2]
  ######################################################################
  min_AIC = 0
  training = retn
  for (i in 0:4) {
    
    for (j in 0:4) {
      
      spec <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(i, j)),
                         mean.model= list(armaOrder = c(a, b)))
      garch <- ugarchfit(spec = spec, data = training, 
                         solver = "hybrid")
      
      if(min_AIC>infocriteria(garch)[1])
      {
        min_AIC = infocriteria(garch)[1]
        p = i
        q = j
      }
    }
  }
  
  ######################################################################
  spec <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(p, q)),mean.model= list(armaOrder = c(a, b)))
  
  w = 300
  ######################################################################
  predic_volatality = numeric(length = (n1-1)-w-t_ahead)
  actual_volatality = numeric(length = (n1-1)-w-t_ahead)
  predic_return = numeric(length = (n1-1)-w-t_ahead)
  actual_return = numeric(length = (n1-1)-w-t_ahead)
  ######################################################################
  # ret_rmse = 0
  i = w
  while (i<=(n1-2-t_ahead))
  {
    training = retn[(i-299):i]
    garch <- ugarchfit(spec = spec, data = training, solver = "hybrid")
    forecst = ugarchforecast(garch,n.ahead = t_ahead)
    predic_volatality[i-(w-1)] = forecst@forecast$sigmaFor[1]
    predic_return[i-(w-1)] = forecst@forecast$seriesFor[1]
    
    k = forecst@forecast$seriesFor - retn[(i+1):(i+t_ahead)]
    # ret_rmse = ret_rmse + k^2
    
    actual_volatality[i-(w-1)] = sd(k)
    actual_return[i-(w-1)] = mean(retn[(i+1):(i+t_ahead)])
    i = i + 1
    
  }
  
  out1 = cbind(predic_volatality,actual_volatality)
  out1 = as.data.frame(out1)
  # out1 = sqrt(out1)
  out_list = vector(mode = "list")
  out_list[[1]] = out1
  out_list[[2]] = as.data.frame(cbind(predic_return,actual_return))
  
  ###############Plotting##############################
  nrow1 = nrow(out1)
  out1 = tail(out1,round(5*nrow1/6))
  png(file="Garchmodel_vol.png",width=1000,height=600)
  plot(out1[,2],
       type = "l",col = "blue",xlab = "Time",ylab = "Volatality",ylim = c(0,0.05),main = "Volatality vs Time",xaxt='n')
  axis(1, at=1:nrow(out1), lab=date[(NROW(date)+1-nrow(out1)-t_ahead):(NROW(date)-t_ahead)])
  lines(out1[,1],col = "red")
  legend("topright", legend=c("Predicted Volatality", "Actual Volatality"),
         col=c("red", "blue"),pch = 15,lty = c(1,1))
  dev.off()
  
  out2 = cbind(predic_return,actual_return)
  out2 = as.data.frame(out2)
  nrow1 = nrow(out2)
  out2 = tail(out2,round(5*nrow1/6))
  # out1 = sqrt(out1)
  png(file="Garchmodel_retn.png",width=1000,height=600)
  plot(out2[,2],
       type = "l",col = "blue",xlab = "Time",ylab = "Return",ylim = c(-0.05,0.05), main = "Return vs Time",xaxt='n')
  axis(1, at=1:nrow(out2), lab=date[(NROW(date)+1-nrow(out2)-t_ahead):(NROW(date)-t_ahead)])
  lines(out2[,1],col = "red")
  legend("topright", legend=c("Predicted Return", "Actual Return"),
         col=c("red", "blue"),pch = 15,lty = c(1,1))
  dev.off()
  
  
  return(out_list)

}