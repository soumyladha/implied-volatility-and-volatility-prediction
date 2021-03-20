dailyv = function(ret,lambda,k,time_ahead)
{
  n1 = length(ret)
  
  actual_v = numeric(length = n1-k-time_ahead)
  predic_v = numeric(length = n1-k-time_ahead)
  
  i = 1
  temp = 0
  sse = 0
  while (i<= n1-1-time_ahead)
  {
    
    temp = temp*lambda + (1-lambda)*(ret[i])^2
    
    i = i + 1
    if (i>k)
      actual_v[i-k] = sqrt(var(ret[i:(i+time_ahead)]))
      predic_v[i-k] = sqrt(temp)
    
      
  }
  return (list(predic_v,actual_v))
}