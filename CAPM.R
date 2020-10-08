#Case 1: CAPM
#Author: Yun Xiao
#Step 0.1: Prepare (data download, R scrpit, WD)
rm(list = ls())
# remove all variables in the workspace
#Step 0.2: load the data
setwd("C:/Users/hbjzx/Desktop/FIN")
CAPM<-read.csv("Case1CAPM.csv", header = TRUE, sep=",")
#CAPM<-read.csv('Case1CAPM.csv', header = TRUE, sep=",")
#Step 0.3: Dimension and Names of the variables
dim(CAPM) #5540 observations, 4 variables
names(CAPM)
# 5540 rows (days), 4 variables; Time series data
#Step 0.4: Read data descriptions 2.3 + view Data
View(CAPM)
#Step 0.5: Change the class of vairable DATE to be "Date"
class(CAPM$DATE) # integer
CAPM$DATE<-as.Date(as.character(CAPM$DATE), "%Y%m%d")
class(CAPM$DATE) # Date
DATE<-CAPM$DATE

#Step 1.1 Create the excess returns of IBM
ibmRET<-CAPM$IBMRET
marketEXERT<-CAPM$MarketEXRET
RF<-CAPM$RF
IBMEXERT<-ibmRET-RF

##Step 1.2 Create yearly excess returns
lg<-length(ibmRET)
IBMEXERT_Annualized<-rep(NA,lg)
marketEXERT_Annualized<-rep(NA,lg)
for (i in 252:lg){
  IBMEXERT_Annualized[i]<-(prod(IBMEXERT[(i-252+1):(i)]/100+1)-1)*100
  marketEXERT_Annualized[i]<-(prod(marketEXERT[(i-252+1):(i)]/100+1)-1)*100
}

# Step 1.3 Time-Series Plot of Yearly returns
jpeg(filename = "Case1_marketEXERT_Annualized.jpeg")
plot(DATE[252:lg], marketEXERT_Annualized[252:lg], type = "l",
     col="blue", xlab="Year", ylab="Market Excess Return",
     main="Daily Market Excess Return (annualized percentage)",ylim=c(-60, 160))
dev.off()

jpeg(filename = "Case1_IBMEXERT_Annualized.jpeg")
plot(DATE[252:lg], IBMEXERT_Annualized[252:lg], type = "l", col="red"
     , xlab="Year", ylab="IBM Excess Return",
     main="Daily IBM Excess Return (annualized percentage)", ylim=c(-60, 160))
dev.off()

# find the global maximum
maximum<-max(marketEXERT_Annualized,na.rm=T)
maxvalue<-grepl(maximum, marketEXERT_Annualized)
findmax<-which(maxvalue)
DATE[findmax] #"2010-03-09"
marketEXERT_Annualized[findmax] #75.05754

# Step 1.4 Five-year Investment 
IBMEXERT_5Year<-rep(NA,lg)
marketEXERT_5Year<-rep(NA,lg)
for (i in (252*5):lg){
  IBMEXERT_5Year[i]<-(prod(IBMEXERT[(i-252*5+1):(i)]/100+1)^(1/5)-1)*100
  marketEXERT_5Year[i]<-(prod(marketEXERT[(i-252*5+1):(i)]/100+1)^(1/5)-1)*100
}

# Step 1.5 Time-Series Plot of Five-year annualized returns
jpeg(filename = "Case1_marketEXERT_5Year.jpeg")
plot(DATE[(252*5):lg], marketEXERT_5Year[(252*5):lg], type = "l", col="blue",
     xlab="Year", ylab="Market Excess Return", main="Daily Market Excess returns (annualized percentage)",
     ylim=c(-10, 60))
dev.off()
jpeg(filename = "Case1_IBMEXERT_5Year.jpeg")
plot(DATE[(252*5):lg], IBMEXERT_5Year[(252*5):lg], type = "l", col="red",
     xlab="Year", ylab="IBM Excess Return", main="Daily IBM Excess returns (annualized percentage)",
     ylim=c(-10, 60))
dev.off()

# Step 1.6 Check Your Work
mean(IBMEXERT_Annualized[252:lg]) #18.01655%
mean(IBMEXERT_5Year[(252*5):lg]) #15.07887%
mean(marketEXERT_Annualized[252:lg]) #8.466417%
mean(marketEXERT_5Year[(252*5):lg]) #5.010191%

# Step 2.1 Create a box plot for market excess returns and another box plot for IBM excess returns.
jpeg(filename = "Case1_marketEXERTboxplot.jpeg")
boxplot(marketEXERT,main="Daily Market Excess returns (percentage)")
dev.off()

jpeg(filename = "Case1_IBMEXERTboxplot.jpeg")
boxplot(IBMEXERT,main="Daily IBM Excess returns (percentage)")
dev.off()

# Step 2.2 Scatter plot
jpeg(filename = "Case1_IBMvsmarketEXERT.jpeg")
plot(IBMEXERT~marketEXERT,xlab="Daily Market Return (percentage)",ylab="Daily IBM Excess Return (percentage",main="Scatter Plots of Stock Return")
dev.off()
# Step 2.3 Numerical Moments
library(e1071)
# Install statistics functions skewness & kurtosis
##Compute Descriptive Statistics for market excess return in daily percentage.
MKTmean<-mean(marketEXERT)*252
MKTsd<-sd(marketEXERT)*sqrt(252)
MKTskew<-skewness(marketEXERT)
MKTkurto<-kurtosis(marketEXERT)
MKTmin<-min(marketEXERT)
MKTmax<-max(marketEXERT)
# Sharpe Ratio
MKTsr<-MKTmean/MKTsd
# Value at Risk
MKTVaR<-quantile(marketEXERT, probs = c(0.05))
#Expected Shortfall
numES<-lg*0.05
numESInteger<-floor(numES)
numESDecimal<-numES-numESInteger
datasort<-sort(marketEXERT, decreasing = FALSE)
MKTES<-sum(datasort[1:numESInteger]+
             datasort[numESInteger+1]*numESDecimal)/numES
tail(datasort) # 6.27  6.35  6.79  6.89  9.77 11.35 

##Compute Descriptive Statistics for IBM excess return in daily percentage.
IBMmean<-mean(IBMEXERT)*252
IBMsd<-sd(IBMEXERT)*sqrt(252)
IBMskew<-skewness(IBMEXERT)
IBMkurto<-kurtosis(IBMEXERT)
IBMmin<-min(IBMEXERT)
IBMmax<-max(IBMEXERT)
# Sharpe Ratio
IBMsr<-IBMmean/IBMsd
# Value at Risk
IBMVaR<-quantile(IBMEXERT, probs = c(0.05))
#Expected Shortfall
datasort2<-sort(IBMEXERT, decreasing = FALSE)
IBMES<-sum(datasort2[1:numESInteger]+
             datasort2[numESInteger+1]*numESDecimal)/numES
## compute the correlation
IBMcMarket<-cor(IBMEXERT, marketEXERT)

# Construct each column of our table.
Name<-c("Mean:", "Std:", "Skewness:", "Kurtosis:",
        "Sharpe Ratio","Value at Risk","Expected Shortfall","Correlation:" )
IBM<-c(IBMmean, IBMsd, IBMskew, IBMkurto, IBMsr, IBMVaR, IBMES, IBMcMarket)
Market<-c(MKTmean, MKTsd, MKTskew, MKTkurto, MKTsr,MKTVaR, MKTES, NA)
## Construct table
data.frame(round(IBM,4), round(Market,4),row.names =Name,check.names = TRUE)

#Step 3.1 FIG Histogram:
jpeg(filename = "Case1_histmarketEXERT.jpeg")
hist(marketEXERT, breaks=50,prob=TRUE,main="Daily Market Excess returns (percentage)")
curve(dnorm(x,mean=mean(marketEXERT),sd=sd(marketEXERT)),add=TRUE,col="blue")

dev.off()

jpeg(filename = "Case1_hisIBMEXERT.jpeg")
hist(IBMEXERT, breaks=100,prob=TRUE,main="Daily IBM Excess returns (percentage)")
curve(dnorm(x,mean=mean(IBMEXERT),sd=sd(IBMEXERT)),add=TRUE,col="blue")
dev.off()

#Step 3.2 FIG QQ plot:
jpeg(filename = "Case1_QQmarketEXERT.jpeg")
qqnorm(marketEXERT, main="Q-Q plot of Market returns")
qqline(marketEXERT)
dev.off()

jpeg(filename = "Case1_QQIBMEXERT.jpeg")
qqnorm(IBMEXERT, main="Q-Q plot of IBM returns")
qqline(IBMEXERT)
dev.off()

# Step 3.3 HT The Jarque-Bera Test
library(tseries)
jarque.bera.test(IBMEXERT) #reject H0, not normal
jarque.bera.test(marketEXERT) #reject H0, not normal

# Step 3.4 HT The Lilliefors test
library(nortest)
lillie.test(IBMEXERT) #reject H0, not normal
lillie.test(marketEXERT) #reject H0, not normal

#Build a regression model
Model<-lm(IBMEXERT~marketEXERT)
summary(Model)
testValue<-1

Model2<-lm(IBMEXERT~marketEXERT)

tstats2<-(Model2$coefficients[2]-testValue)/summary(Model2)[["coefficients"]][2,2]

Result<-ifelse(tstats2>qt(0.95, length(marketEXERT)),"Reject", "Can't Reject")
Result # marketEXERT  "Can't Reject" 
#(Intercept) marketEXERT
#0.04041978 0.91177448
names(summary(Model))
#[1] "call" "terms" "residuals" "coefficients" "aliased" "sigma"
#[7] "df" "r.squared" "adj.r.squared" "fstatistic" "cov.unscaled"
summary(Model)[["coefficients"]]
#Estimate Std. Error t value Pr(>|t|)
#(Intercept) 0.04041978 0.01961324 2.060842 0.0393647
#marketEXERT 0.91177448 0.01652523 55.174702 0.0000000
summary(Model)[["call"]]
#lm(formula = IBMEXERT ~ marketEXERT)
summary(Model)[["sigma"]]
#[1] 1.459328
summary(Model)[["df"]]
#[1] 2 5538 2
summary(Model)[["r.squared"]]
#[1] 0.3547145
summary(Model)[["fstatistic"]]
#value numdf dendf
#3044.248 1.000 5538.000

#Step 8 Plot the OSL Line
jpeg(filename = "Case1_OLSLINE.jpeg")
plot(marketEXERT, IBMEXERT,
     main="Scatter Plot of IBM Excess returns Vs. Market Excess returns",
     xlab= "Market Excess returns", ylab="IBM Excess returns")
abline(lm(IBMEXERT~marketEXERT), col="blue")
dev.off()

# Step 9.1: Is the adjusted returns zero
#s1: According to the null
testValue<-0
Model<-lm(IBMEXERT~marketEXERT)
#s2: compute test statistics
estimatedcoeff<-Model$coefficients[1]
estimatedstd<-summary(Model)[["coefficients"]][1,2]
tstats<-(estimatedcoeff-testValue)/estimatedstd
#s3: decision rule for two sided test
decisionRule<-abs(tstats)>qt(0.975, length(marketEXERT)-1-1)
#s4: conclusion
Result<-ifelse(decisionRule, "Reject", "Can't Reject")
Result #Reject

# Step 9.2: Is the risk exposure higher than one?
#s1: According to the null
testValue<-1
Model<-lm(IBMEXERT~marketEXERT)
#s2: compute test statistics
estimatedcoeff<-Model$coefficients[2]
estimatedstd<-summary(Model)[["coefficients"]][2,2]
tstats<-(estimatedcoeff-testValue)/estimatedstd
#s3: decision rule for two sided test
decisionRule<-tstats>qt(0.95, length(marketEXERT)-1-1)
#s4: conclusion
Result<-ifelse(decisionRule, "Reject", "Can't Reject")
Result #Can't reject

