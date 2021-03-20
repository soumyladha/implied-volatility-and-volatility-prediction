ewma = function(NSE,t_ahead)
{  
  date = row.names(NSE)
  
  NSE =  NSE[,4]
  NSE =  cbind(as.data.frame(date),as.data.frame(NSE))
  NSE = na.omit(NSE)
  colnames(NSE) = c("date","price")
  

  ######################################################################
  n1 = NROW(NSE)
  retn = log(NSE[2:n1,2]) - log(NSE[1:(n1-1),2])
  
  ######################################################################
  source("EWMA sse calculator.R")
  lambda = 0.1
  max = .Machine$integer.max
  
  temp3 = vector(mode = "numeric")
  temp2 = vector(mode = "numeric")
  
  
  
  while (lambda<1)
  {
    temp = lamda(retn,lambda,200,t_ahead)
    temp3[100*(lambda-0.09)] = temp^2
    #print(-6/log(lambda,base = 10))
    temp2[100*(lambda-0.09)] = lambda
    if (temp<max)
    {
      max = temp
      lambda_best = lambda
    }
    
    lambda = lambda + 0.01
  }
  png(file="ewma_lambda.png",width=1000,height=600)
  plot(temp2,log(temp3),type="l", col="red",xlab = "lambda",ylab = "MSE",main = "MSE vs lambda")
  dev.off()
  
  source("EWMA predictor.R")
  out = dailyv(retn,lambda_best,200,t_ahead)
  
  out1 = as.data.frame(out)
  colnames(out1) = c("predicted","actual")
  # out2 = exp(out2)
  
  nrow1 = nrow(out1)
  out1 = tail(out1,round(5*nrow1/6))
  png(file="Volatality modelling (ewma).png",width=1000,height=600)
  plot(out1[,2],
       type = "l",col = "blue",xlab = "Time",ylab = "Volatality",ylim = c(0,0.05),main = "Volatality vs Time",xaxt='n')
  axis(1, at=1:nrow(out1), lab=date[(NROW(date)+1-nrow(out1)-t_ahead):(NROW(date)-t_ahead)])
  lines(out1[,1],col = "red")
  legend("topright", legend=c("Predicted Volatality", "Actual Volatality"),
         col=c("red", "blue"),pch = 15,lty = c(1,1))
  dev.off()
  
  return(out)
}