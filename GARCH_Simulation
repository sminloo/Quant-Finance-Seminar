library(xts)
library(rugarch)
library(zoo)
# Import data and convert into time series format
sp <- read.zoo("SP-500.csv", header = TRUE, sep = ",",format="%m/%d/%Y",index.column = 1)
ing <- read.zoo("ING.csv", header = TRUE, sep = ",",format="%Y/%m/%d",index.column = 1)
rds <- read.zoo("RDS-B.csv", header = TRUE, sep = ",",format="%Y/%m/%d",index.column = 1)
aex <- read.zoo("AEX.csv", header = TRUE, sep = ",",format="%Y/%m/%d",index.column = 1)
sp <- xts(sp)
ing <- xts(ing)
aex <-xts(aex)
rds <- xts(rds)
aex <-na.omit(aex) # doesnt work! 
# Negative log returns
sp <- - 100*diff(log(sp))
ing <- - 100*diff(log(ing))
rds<- - 100*diff(log(rds))
# aex<- - 100*diff(log(aex))  needs to remove na


#GARCH estimation 
# GARCH with Student's t
sp <-sp[2:8406]
sp2 <-sp[6400:7400]
sp_garch_spec_t <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(1, 0)),distribution.model ="std")
sp_garch_t1 <-ugarchfit(spec =sp_garch_spec_t ,data = sp2,solver) #fit GARCH

sp_garch_t_forecast <-ugarchforecast(sp_garch_spec_t, data = sp, n.ahead = 1, n.roll = 2000,
                                     out.sample = 2000)  # doesnt work ,later! 

plot(sp_garch_t1)
plot(sp2)
plot(sp_garch_t1@fit$sigma,type="l")
acf(as.data.frame(sp2),lag=30)

acf(sp_garch_t1@fit$residuals,lag=30)
acf(abs(sp_garch_t1@fit$residuals),lag=30)

res1 <-sp_garch_t1@fit$residuals
# testing VaR with Student's t
sp_garch_t <-ugarchroll(spec =sp_garch_spec_t,data =sp, n.ahead = 1, 
                        n.start = 1000,
                        refit.every = 1, refit.window = "moving",
                        calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
report(sp_garch_t, type="VaR", VaR.alpha = 0.005, conf.level = 0.95)    
report(sp_garch_t, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(sp_garch_t, type="VaR", VaR.alpha = 0.05, conf.level = 0.95) 
report(sp_garch_t, type="fpm")
                        
# GARCH with Normal distribution 
sp_garch_spec_n <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(1, 1)),distribution.model ="norm")

sp_garch_n <-ugarchroll(spec =sp_garch_spec_n,data =sp, n.ahead = 1, 
                        n.start = 1000,
                        refit.every = 1, refit.window = "moving",
                        calculate.VaR = TRUE, VaR.alpha = c(0.005,0.01, 0.05))
report(sp_garch_n, type="VaR", VaR.alpha = 0.005, conf.level = 0.95)    
report(sp_garch_n, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(sp_garch_n, type="VaR", VaR.alpha = 0.05, conf.level = 0.95) 


install.packages("extRemes")
library(extRemes)

a <- rt(1000,4) # Generate 1000 random obs from t(4)
#a <- as.vector(a)

fit1<-fevd(res1,type="GP",threshold= quantile(a,0.95))
fit1$re
mrlplot(res1)
threshrange.plot(res1, r = c(0, 1))
plot(fit1,"rl")

# Calculate VaR based on GPD using moving window length of 1000
sp1 <-sp[1:1030]
recur_sp <-rollapply(sp1,
                      width = 1000, 
                      FUN= function(sp1)
                      { 
                        recur_garch<- ugarchfit(spec =sp_garch_spec_t ,data =as.data.frame(sp1))
                        res_garch <-recur_garch@fit$residuals
                        gpd_fit <-fevd(res_garch,type="GP",threshold= quantile(res_garch,0.90))
                        coef <- gpd_fit$results$par
                        xi <-coef[2]
                        beta <-coef[1]
                        q <- 0.95
                        var <- quantile(res_garch,0.899) + (beta/xi)* (((1-q)*1000/100)^(-xi) -1)
                        out <-union(var,coef)
                        return (out)
                      },
                      by.column=FALSE, align="right")
recur_sp 

#install.packages("backtest")
library(backtest)
test1 <- as.data.frame(cbind(sp[1000:1030],recur_sp[,1]))
names(test1) <-c("obs","var") #add names to column
backtest(test1,in.var="obs",ret.var="var",by.period = FALSE)
summary(backtest)  #doesnt work

# Simulation study for threshold selection
t <-lapply(1:10, function(x) (rt(1000,4)))
simu_zq<-sapply(t,FUN=function(t) {
       k<-0.9  # determine # of exceedances 1000*(1-k)
       simu_fit<- fevd(t,type="GP",threshold= quantile(t,k))
       coef <- simu_fit$results$par
       xi <-coef[2]
       beta <-coef[1]
       zq_hat <- quantile(t,(k-0.001)) + (beta/xi)*  ((0.01/(1-k))^(-xi)-1)
       zq <-quantile(t,0.99)
       out <-union(zq,zq_hat)
       return(out)
       } )    
simu_zq 

simu_zq <- t(as.data.frame(simu_zq))           

mse <- sum((simu_zq[,2]-simu_zq[,1])^2)


