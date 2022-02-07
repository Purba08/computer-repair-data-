data =read.csv('computer repair.csv')
data
#fitting the regression 
reg=lm(formula=Y.minutes.~X.Units.X ,data=data)
summary(reg)
#From the R_squared value we can conclude that 98.74% of data fit the linear regression model .
#visualizing the results
library(ggplot2)
ggplot()+geom_point(aes(x=data$X.Units.X,y=data$Y.minutes.),color="red")+
  geom_line(aes(x=data$X.Units.X,y=predict(lin_reg,newdata=data)),color='blue')+
  ylab('minutes')+xlab('Units')
# Extract the fitted values of y  :
#19.67043  35.17920  50.68797  66.19674  66.19674  81.70551  97.21429  97.21429 112.72306 128.23183 
#143.74060 143.74060 159.24937 159.24937 
y.fitted <- reg$fitted.values
y.fitted
#The fitted values are :
# Coefficients of the linear model, beta0 and beta1
b0 <- reg$coefficients[1]
b1 <- reg$coefficients[2]
b0
b1
#beta0=4.162 and beta1=15.509

# Find SSE and MSE
sse <- sum((data$Y.minutes.- y.fitted)^2)
sse
n= n <- length(data$Y.minutes.)
n
mse <- sse / (n - 2)
mse
# Critical value of t at alpha=0.01
t.val=qt(0.995, n - 2)
t.val
#t0.005,12=3.05454
#testing if beta0=6 or not
T1=(Mod(4.162-6))/3.355
T1
#T1=0.547839 ,n  tabulated t=3.055 at 1%level of significance
#we can not reject the null hypothesis beta not=6
T2=(15.509-13)/0.505
T2
#T2=4.968317 and tabulated t=3.055 at 1% level of significance .
#we reject the null hypothesis beta1=13

#confidence interval for beta0 and beta1
confint(reg,level=0.99)

#testing the correlation between x and y
res=cor.test(data$Y.minutes.,data$X.Units.X,method="pearson")
res
#test statistics=30.715 and p value is less than 0.01 .
#We can conclude that X and Y are not independent .


##Prediction 
#the prediction of the values of the response variable corresponding to any chosen value
#predict the value of y when the given value of x is 4
pred=predict(reg,data.frame(X.Units.X=4))
pred
#The Predicted value of y is 66.19674 when X=4
#standard error of predicted value
mean_x=mean(data$X.Units.X)
mean_x
ssx=sum((data$X.Units.X-mean_x)^2)
ssx
ss_pred=(4-mean_x)^2
ss_pred 
pred.se.fit=(1 + (1 / n) + (ss_pred/ssx))
pred.se.fit
sse_pred=sqrt(mse * pred.se.fit)
sse_pred
#standard error of predicted value of Y is 5.671614 at X=4

#confidence interval for predicted Y at X=4
UI=pred+t.val+sse_pred
UI
LI=pred-t.val+sse_pred
LI

#confidence interval for Predicted Y at x=4 (68.81382,74.9229)at 1% level of significance.

#The estimation of mean response mu0 when X=X0
x0=4
mu0=b0+b1*x0
mu0              

#standard error of estimated mu0
mu.se.fit=((1 / n) + (ss_pred/ssx))
mu.se.fit
sse_mu=sqrt(mse * mu.se.fit)
sse_mu
#standard error of estimated mu0 at X0=4 is 1.759688

#confidence interval for estimated mu at X=4
UI=mu0+t.val+sse_mu
UI
LI=mu0-t.val+sse_mu
LI

#confidence interval for estimated mu at x=4 ( 64.90189 , 71.01097 )at 1% level of significance.


