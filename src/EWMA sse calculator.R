lamda = function(ret,lambda,k,time_ahead)
{
  n1 = length(ret)
  
  i = 1
  temp = 0
  sse = 0
  while (i<= n1-1-time_ahead)
  {

    temp = temp*lambda + (1-lambda)*(ret[i])^2
    
    i = i + 1
    if (i>k)
      sse = (temp-(var(ret[i:(i+time_ahead)])))^2 + sse
  }
  return (sqrt((sse)/(n1-k-time_ahead)))
}