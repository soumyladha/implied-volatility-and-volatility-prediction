hist_model = function(NSE,t_ahead)
{
  date = row.names(NSE)
  NSE =  NSE[,4]
  NSE =  cbind(as.data.frame(date),as.data.frame(NSE))
  NSE = na.omit(NSE)
  colnames(NSE) = c("date","price")
  
  NSE = NSE[1:nrow(NSE),]
  ######################################################################
  n1 = NROW(NSE)
  retn = log(NSE[2:n1,2]) - log(NSE[1:(n1-1),2])
  retn = as.data.frame(retn)
  retn <- retn[retn<=0.5,]
  retn = as.data.frame(retn)
  retn <- retn[retn>=-0.5,]
  n1 = NROW(retn)+1
  
  
  days = 101
  # t_ahead = 30
  lambda = 0.1
  i = days
  actual_volatality = numeric(n1-days-t_ahead)
  predicted_volatality = numeric(n1-days-t_ahead)
  predic_return = numeric(n1-days-t_ahead)
  actual_return = numeric(n1-days-t_ahead)
  while(i<n1-t_ahead)
  {
    
    actual_volatality[i-(days-1)] = mean((retn[i:(i+t_ahead)])^2)
    predicted_volatality[i-(days-1)] = mean((retn[(i-(days-1)):(i)])^2)
    predic_return[i-(days-1)] = mean((retn[(i-(days-1)):(i)]))
    actual_return[i-(days-1)] = mean((retn[i:(i+t_ahead)]))
    i = i+1
  }
  out1 = cbind(predicted_volatality,actual_volatality)
  out1 = as.data.frame(out1)
  out1 = sqrt(out1)
  
  list_some = vector(mode = "list")
  list_some[[1]] = out1
  list_some[[2]] = as.data.frame(cbind(predic_return,actual_return))
  
  ###############Plotting##############################
  nrow1 = nrow(out1)
  out1 = tail(out1,round(5*nrow1/6))
  png(file="Histmodel_vol.png",width=1000,height=600)
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
  png(file="Histmodel_retn.png",width=1000,height=600)
  plot(out2[,2],
       type = "l",col = "blue",xlab = "Time",ylab = "Return",ylim = c(-0.05,0.05),main = "Return vs Time",xaxt='n')
  axis(1, at=1:nrow(out2), lab=date[(NROW(date)+1-nrow(out2)-t_ahead):(NROW(date)-t_ahead)])
  lines(out2[,1],col = "red")
  legend("topright", legend=c("Predicted Return", "Actual Return"),
         col=c("red", "blue"),pch = 15,lty = c(1,1))
  dev.off()
  
  
  return(list_some)
}