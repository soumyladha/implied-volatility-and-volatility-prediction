library(quantmod)
library(fitdistrplus)
set.seed(100)
object1 = getSymbols("%5ENSEI", src="yahoo")
NSE=as.data.frame(get(object1))

################################################
t = 1
source("mean_model.R")
mean_model_out = mean_model(NSE,t)
source("EWMA estimation.R")
ewma_model_out = ewma(NSE,t)
source("Moving windows Garch.R")
garch_model = garch1(NSE,t+1)
source("Hist.R")
hist_model_out = hist_model(NSE,t)

source("Implied Volatality.R")
implied_vol_out = implied_vol(1)
implied_vol_out = as.data.frame(implied_vol_out)
implied_vol_out[,2] = as.numeric(as.character(implied_vol_out[,2]))

NSE = head(NSE,-t)
all_vol = cbind(tail(row.names(NSE),2000),tail(mean_model_out[[1]],2000),tail(as.data.frame(ewma_model_out),2000),
                tail(garch_model[[1]],2000),tail(hist_model_out[[1]],2000))
colnames(all_vol) = c("Date","Mean Model Predicted","Mean Model Actual","EWMA Model Predicted",
                      "EWMA Model Actual","Garch Model Predicted","Garch Model Actual","Hist Model Predicted",
                      "Hist Model Actual")

row.names(all_vol) <- 1:nrow(all_vol)
k = (1886+t):(1886+t+66)
png(file="Comparision_vol.png",width=1000,height=600)
plot(implied_vol_out[,2],
     type = "l",col = "blue",xlab = "Time",ylab = "Volatility",ylim = c(0,0.05),main = "Volatility vs Time",xaxt='n')
axis(1, at=1:nrow(implied_vol_out), lab=as.character(all_vol[k,1]))
lines(all_vol[k,2],col = "red")
lines(all_vol[k,4],col = "green")
lines(all_vol[k,6],col = "black")
lines(all_vol[k,8],col = "yellow")
legend("topright", legend=c("Implied Volatility", "Mean Model Volatility",
                            "EWMA Model Volatility","Garch Model Volatility","Historical Model Volatility"),
       col=c("blue", "red","green","black","yellow"),pch = 15,lty = c(1,1))
dev.off()
################################################