# read 
# setwd("your working directory")
rds_b <- read.csv("RDS-B.csv")
rds_b$Date <- as.Date(rds_b$Date)
sp500 <- read.csv("SP-500.csv")
sp500$Date <- as.Date(sp500$Date, "mm/dd/YYYY")
ing <- read.csv("ING.csv")
ing$Date <- as.Date(ing$Date)
aex <- read.csv("AEX.csv")
aex$Date <- as.Date(aex$Date)

#transform to log returns
n <- length(rds_b$Date)
rds_b$Returns <- c(-log(rds_b$Close[1:n-1]/rds_b$Close[2:n]), NA)
n <- length(sp500$Date)
sp500$Returns <- c(-log(sp500$Close[1:n-1]/sp500$Close[2:n]), NA)
n <- length(ing$Date)
ing$Returns <- c(-log(ing$Close[1:n-1]/ing$Close[2:n]), NA)
n <- length(aex$Date)
aex$Close[aex$Close=="null"] <- NA
aex$Close <- as.numeric(aex$Close)
aex$Returns <- c(-log(aex$Close[1:n-1]/aex$Close[2:n]), NA)

#xts demo code
aex_xts <- xts(x = aex$Close, order.by = aex$Date)

#garch
library(rugarch)
T <- length(rds_b$Date)
n <- 1000

for(i in n:T){
  gspec.ru <-  ugarchspec(mean.model=list(
    armaOrder=c(1,0)), distribution="std")
  
  gfit.ru <- ugarchfit(gspec.ru, rds_b$Returns[i-n+1:i])
  coefficients[i] <- coef(gfit.ru)  
}

