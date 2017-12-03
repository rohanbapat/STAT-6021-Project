library(tidyverse)
library(qpcR)
library(MASS)
library(calibrate)
library(leaps)
library(car)
library(broom)
library(glmnet)
library(lmtest)
library(DAAG)
library(readxl)
library(olsrr)

#### Data Prep ####
# read in data
data <- read_csv("data-table-B1.csv") #Read in the xls data file #NFL data

# look at data
View(data)
# plot data, looking at each regressor versus the response
plot(y~., data = data)

plot(y~x1, data=data) #appears to be a positive linear relationship
plot(y~x2, data=data) #maybe slightly positive linear relationship
plot(y~x3, data=data)
plot(y~x4, data=data)
plot(y~x5, data=data) #maybe slightly positive linear relationship
plot(y~x6, data=data)
plot(y~x7, data=data) #appears to be a positive linear relationship
plot(y~x8, data=data) #negative relationship
plot(y~x9, data=data)

#### Basic Linear Model ####
#Create basic lm
nfl.lm <- lm(y~., data=data)

# plot and add regression line to plot
plot(nfl.lm) #have to hit return in console to scroll through all plots
#abline(nfl.lm) #doesn't work with 8 regressors, only with one/simple regression model

## View additional model information including standard error for regression and tests for each coefficient
summary(nfl.lm)
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -7.292e+00  1.281e+01  -0.569 0.576312    
#x1           8.124e-04  2.006e-03   0.405 0.690329    
#x2           3.631e-03  8.410e-04   4.318 0.000414 ***
#x3           1.222e-01  2.590e-01   0.472 0.642750    
#x4           3.189e-02  4.160e-02   0.767 0.453289    
#x5           1.511e-05  4.684e-02   0.000 0.999746    
#x6           1.590e-03  3.248e-03   0.490 0.630338    
#x7           1.544e-01  1.521e-01   1.015 0.323547    
#x8          -3.895e-03  2.052e-03  -1.898 0.073793 .  
#x9          -1.791e-03  1.417e-03  -1.264 0.222490    

##(Intercept): The intercept, in our example, is the expected value of the games won when we consider the 
##average value of the other variables dataset. In other words, there is an average of -7.292 games won per 
##season when all other variables equal zero. The negative intercept tells you where the linear model 
##predicts games won (y) would be when the x variables are all equal to 0.

##Estimates for x values = The beta values for each variable. The slope term in our model is saying that 
##for every 1 game increase in the record, the rushing yards (x1) in the season need to go up by .000812*x.

##t-value: a measure of how many standard deviations our coefficient estimate is far away from 0. We want it to be 
##far away from zero as this would indicate we could reject the null hypothesis. In this case the t-statistic values for x2
##are relatively far away from zero and are large relative to the standard error, which could indicate a relationship exists. 
##In general, t-values are also used to compute p-values.

##p-value: probability of observing any value equal or larger than t. 

#Residual standard error: 1.83 on 18 degrees of freedom
#Multiple R-squared:  0.8156,	Adjusted R-squared:  0.7234 
#F-statistic: 8.846 on 9 and 18 DF,  p-value: 5.303e-05

##Residual Standard Error: a measure of the quality of a linear regression fit and is a representation of the standard deviation
##of the residuals. The Residual Standard Error is the average amount. To make this estimate unbiased, you have to divide the sum of the squared 
##residuals by the degrees of freedom in the model.

##Multiple R-squared: also called the coefficient of determination is the proportion of the variance in the data 
##that's explained by the model. Interpreted as 81.56% of the variance found in the response variable can be explained 
##by the predictor variable(s). In multiple regression settings, the R2R2 will always increase as more variables are included in the model. 
##That’s why the adjusted R2R2 is the preferred measure as it adjusts for the number of variables considered.

##F Statistic: good indicator of whether there is a relationship between our predictor and the response variables. 
##The further the F-statistic is from 1 the better it is. #F = ((Sum Sq of var)/2) / Residual Mean Sq.

### CURRENT CONCLUSIONS ###

##Based on this summary, there is a positive relationship with high significance between the response
##variable and x2. There is also a negative relationship with less significance between the response
##variable and x8. With all regressors included, the model explains about 82% of the variance
##in the response. It is likely, however, that a model with only a few of these regressors will perform
##just as well and explain almost the same amount of variance. Will come back to this later.

#### Residuals ####
##Residuals: When assessing how well the model fit the data, you should look for a 
##symmetrical distribution across these points on the mean value zero (0). 

## Find the residuals (and then use a plot to look for patterns/non-constant variance)
ei<-resid(nfl.lm)

##The other residuals can be used to detect influential points and outliers. 
## Find the studentized residuals
ri<-rstandard(nfl.lm)

## Find the R-student residuals
ti<-rstudent(nfl.lm)

## Normal probabilty plot
qqnorm(rstudent(nfl.lm))
qqline(rstudent(nfl.lm)) #Relatively normal distribution, although not perfect (tails)

## Residual plot vs. fitted values
yhat <- fitted(nfl.lm)
plot(yhat,ti) 
#This residual plot looks pretty good. No clustering, no funneling, no specific patterns of concern.
#Generally well distributed around zero.

## Residual plots vs. explanatory variables
plot(data$x1,ti) 
plot(data$x2,ti) #Looks okay (maybe very slight funneling in from left to right)
plot(data$x3,ti) #Clear funneling out
plot(data$x4,ti) 
plot(data$x5,ti)
plot(data$x6,ti)
plot(data$x7,ti) #Funneling out (nonconstant variance), but really only from one point
plot(data$x8,ti) #Looks generally okay, maybe a slight double bow
plot(data$x9,ti)

#### PRESS stat ####
prco2 = (nfl.lm$residuals)/(1 - lm.influence(nfl.lm)$hat)
pressc02 = sum(prco2^2)
print(pressc02)
#145.9139

#ANOTHER WAY TO DO THIS...
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}
PRESS(nfl.lm) #145.9139 (so the function works just the same as the above)

#### Boxcox Transformation #####
#This is a way to transform non-normal dependent variables into a normal shape. 
#Normality is an important assumption for many statistical techniques; if your data isn't normal, 
#applying a Box-Cox means that you are able to run a broader number of tests.

#Tukey transformation - good for fanning out
bc = boxcox(y + .0001 ~ ., data = data)

#get lambda range
lrange = range(bc$x[bc$y > max(bc$y)-qchisq(0.95,1)/2])
lambda = mean(lrange)
lambda #0.8282828

#re-do model
databc = data
databc$ytrans = databc$y^lambda
fit.trans = lm(ytrans ~ .-y, data = databc)
ti.trans <- rstudent(fit.trans)

## look at the qqplot and qqnorm again
## look at the resid plots again
qqnorm(ti.trans)
qqline(ti.trans) #MAJOR outlier

summary(fit.trans) #R^2 0.7188
summary(nfl.lm) #original R^2 0.7234 (significantly better)

#### Log Transformation of Variables ####
## Linear model using transformed x7 (since this had a funnel residual plot)
data2 = data #create new data set for ease of use later...
data2$x7_trans <- log(data$x7) #log will help reduce difference of effects between small/large values
data2$x3_trans <- log(data$x3) #log will help reduce difference of effects between small/large values
nfl.lm.trans2 <- lm(y~.-x3 -x7, data=data2) #run new model with transformed x7, plus other two vars
summary(nfl.lm.trans2) #not better in terms of R^2
PRESS(nfl.lm.trans2) #144.4999
PRESS(nfl.lm) #145.9139 slightly higher and not significant
ti2 <- rstudent(nfl.lm.trans2) #

## look at the qqplot and qqnorm again
## look at the resid plots again
qqnorm(ti2)
qqline(ti2)

plot(data2$x7_trans,ti2) #outlier in top right of plot
plot(data2$x3_trans,ti2) #outlier in top right of plot

#### Adding "Weights" #####
##Estimate weights (divide by high/low response variable)
nfl_low <- data[data$y<7,]
x2_low <- 1/summary(lm(y~x2+x7+x8, data=nfl_low))$sigma^2
nfl_high <- data[data$y>=7,]
x2_high <- 1/summary(lm(y~x2+x7+x8, data=nfl_high))$sigma^2
data$wt <- ifelse(data$y<7,x2_low,x2_high)

#weights for model
## Run the weighted linear regression
nfl.lm.wt <- lm(y~x2+x7+x8, data=data, weights=wt)
ei.wt <- weighted.residuals(nfl.lm.wt)
ti.wt <- rstudent(nfl.lm.wt)

qqnorm(ti.wt) 
qqline(ti.wt)

summary(nfl.lm)
summary(nfl.lm.wt) #not superior

#### Leverage and Influence ####

## Cook's Distance
cooks.distance(nfl.lm)
ols_cooksd_chart(nfl.lm)
#Cook's distance is useful for identifying outliers in the X values (observations for predictor 
#variables). It also shows the influence of each observation on the fitted response values. 
#An observation with Cook's distance larger than three times the mean Cook's distance might be an 
#outlier. Can measure the effect of removing a data point on all the parameters combined

## DFBETAS
dfbetas(nfl.lm)
ols_dfbetas_panel(nfl.lm)
#DFBETA measures the difference in each parameter estimate with and without the influential point. 
#recommend 2 as a general cutoff value to indicate influential observations.

## DFFITS
dffits(nfl.lm)
ols_dffits_plot(nfl.lm)
#This is the difference in fits, is used to identify influential data points. It quantifies the number 
#of standard deviations that the fitted value changes when the ith data point is omitted.

ols_diagnostic_panel(nfl.lm)
## COVRATIO
covratio(nfl.lm)
#This measures the change in the determinant of the covariance matrix of the estimates by deleting the 
#ith observation: cov.value -1 >= 3*p/n, where p is the number of parameters in the model and n is the 
#number of observations used to fit the model, are worth investigation.

## A summary of potential leverage and/or influential points
summary(influence.measures(nfl.lm))
ols_rsdlev_plot(nfl.lm)
#   dfb.1_  dfb.x1  dfb.x2  dfb.x3  dfb.x4 dfb.x5 dfb.x6 dfb.x7 dfb.x8 dfb.x9  dffit   cov.r   cook.d hat  
#4  -1.03_* -0.45    1.20_*  1.01_* -0.46  -0.94   0.88   0.28  -0.84   1.05_*  2.51_*  0.47    0.53   0.60
#5  -0.05   -0.06    0.04    0.00    0.03  -0.03   0.02   0.05   0.03   0.01   -0.11    2.89_*  0.00   0.39
#10  0.36   -1.23_*  0.18   -0.18    0.06   0.23  -0.27   0.54  -0.69   0.25   -1.78    0.17    0.25   0.37
#11  0.05   -0.04   -0.06   -0.27   -0.16   0.23   0.11   0.17   0.19   0.04    0.52    3.30_*  0.03   0.53
#18  0.13   -0.03   -0.25    0.06    0.07  -0.18  -0.11  -0.05  -0.29   0.03   -0.51    3.06_*  0.03   0.50
#23  0.02    0.00   -0.04   -0.02    0.01  -0.01  -0.02   0.00  -0.01   0.04    0.08    2.82_*  0.00   0.37
#24 -0.01   -0.05   -0.10    0.13   -0.01   0.03   0.03  -0.06  -0.10   0.05   -0.19    2.75_*  0.00   0.38
#25 -0.06    0.01    0.02   -0.02    0.06  -0.03   0.07   0.05   0.08  -0.08   -0.13    3.64_*  0.00   0.52
#27  0.01    0.01    0.01   -0.01   -0.01   0.00   0.00  -0.01   0.01   0.00    0.02    3.27_*  0.00   0.46

#Although a lot of these points show influence based on cov.r, they generally look okay (particularly
#in regards to Cook's D and aggregated metrics). If anthing, 4 seems to have some dfbeta issues.

## Points to investigate
inv_pos <- as.numeric(row.names(summary(influence.measures(nfl.lm))))

## Plots with points labeled - this is using calibrate package

#MORE OF THESE...
plot(y~x2, data=data)
textxy(data$x2[inv_pos],data$y[inv_pos],inv_pos) #Nothing looks bad here (18 and 27 are in their own cluster)

plot(y~x7, data=data)
textxy(data$x7[inv_pos],data$y[inv_pos],inv_pos) #27 may be a leverage point, but not severely

plot(y~x8, data=data)
textxy(data$x8[inv_pos],data$y[inv_pos],inv_pos) #18 and 27 may be leverage points, but not severely

yhat <- fitted(nfl.lm)
ti <- rstudent(nfl.lm)

plot(yhat,ti)
textxy(yhat[inv_pos],ti[inv_pos],inv_pos) #everything looks generally okay here (although obs 1 is high)

#### Multicollinearity ####
## Correlation matrix
cor(data[,2:10])
#       x1        x2 ...
#x1 ...

#x1 and x7 appear to have a somewhat near linear relationship (corr=0.837)

#calculate VIF
vif(nfl.lm)
#    x1     x2     x3     x4     x5     x6     x7     x8     x9 
# 4.8276 1.4202 2.1266 1.5661 1.9240 1.2760 5.4146 4.5356 1.4234

#### Ridge and Lasso ####
#These are for when multicollinearity exists (maybe for x1 and x7)

#Tranform data into matrix
data.m <- as.matrix(data)

## Setting alpha=0 designates ridge regression
## This function automatically standardizes the explanatory variables
nfl.ridge <- glmnet(data.m[,-1], data.m[,1], alpha=0, lambda=0.01)

## Ridge coefficicent estimates
coef(nfl.ridge)
#(Intercept) -7.1613707554
#x1           0.0008333182
#x2           0.0036185958
#x3           0.1195245780
#x4           0.0317191497
#x5           0.0004464231
#x6           0.0015886209
#x7           0.1530372153
#x8          -0.0038741657
#x9          -0.0017918316

## Ridge regression can also be done with several lambda values
## This procedure will yield a matrix of coefficients
nfl.ridge <- glmnet(data.m[,-1], data.m[,1], alpha=0)

## Create the ridge trace plot
plot(nfl.ridge,xvar="lambda",label=TRUE) #lambda = 4? difficult to say

## To see the lambda value that was used and the corresponding coefficients (if using 4)
nfl.ridge$lambda[2]
#2297.944
coef(nfl.ridge)[,2]

#Optimal lambda...
set.seed(4)
cv.fit <- cv.glmnet(data.m[,-1], data.m[,1], alpha = 0)
plot(cv.fit)
opt.lambda <- cv.fit$lambda.min
opt.lambda
#0.3037747 -- note, this small of a value means you shouldn't use a lambda
glm_fit <- cv.fit$glmnet.fit
summary(glm_fit)

#Re-run with optimal lambda
nfl.ridge <- glmnet(data.m[,c(3,8,9)], data.m[,1], alpha=0, lambda=opt.lambda)

#Ridge coefficicent estimates
coef(nfl.ridge)
#(Intercept) -1.337444457
#x2           0.003299101
#x7           0.185930466
#x8          -0.004515974

ridge_yhat <- -1.337444457+0.003299101*data$x2+0.185930466*data$x7+-0.004515974*data$x8
rsquare(data$y, ridge_yhat) #0.7828595

summary(nfl.lm) #R^2 = 0.7863 (original is better than ridge regression)

## Lasso regression uses the same function as ridge regression with alpha=1
h.lasso <- glmnet(data.m[,c(3,8,9)], data.m[,1], alpha=1)
plot(h.lasso,xvar = "lambda",label=TRUE)
h.lasso$lambda[0] #cancels out
coef(h.lasso)[,0]

#### Model Selection ####
#Note: need to remove non-transformed variables
## Comparative model selection
bestmod <- regsubsets(y^lambda~., data=data, nbest=10, force.out="wt")

## The 10 best models for each number of explanatory variables in the model
summary(bestmod)
#         x1  x2 ...
#1  ( 1 ) "*" " "
#1  ( 2 ) " " "*"
#2  ( 3 ) "*" "*" ...

bestmod.sum <- as.data.frame(summary(bestmod)$outmat)
bestmod.sum$p <- as.numeric(substr(rownames(bestmod.sum),1,1))+1

## The criterion values corresponding to each model
summary(bestmod)$rss 
summary(bestmod)$adjr2 
summary(bestmod)$cp 
summary(bestmod)$bic 

#add to bestmod.sum
bestmod.sum$rss <- summary(bestmod)$rss
bestmod.sum$adjr2<-summary(bestmod)$adjr2
bestmod.sum$cp<-summary(bestmod)$cp
bestmod.sum$bic<-summary(bestmod)$bic

## Determine "best" models
bestmod.sum[order(bestmod.sum$rss),] #want low - everything but x5
bestmod.sum[order(-bestmod.sum$adjr2),] #want high - x2,x7,x8,x9
bestmod.sum[order(bestmod.sum$cp),] #want low - x2,x7,x8,x9
bestmod.sum[order(bestmod.sum$bic),] #want low - x2,x7,x8

plot(bestmod, scale = 'r2')
plot(bestmod, scale = 'adjr2')
plot(bestmod, scale = 'Cp')
plot(bestmod, scale = 'bic')

#Appears to recommend x2,x7,x8, and maybe x9

## Iterative model selection
## Begin by defining the models with no variables (null) and all variables (full)
s.null <- lm(y^lambda~1, data=data)
s.full <- lm(y^lambda~.-wt, data=data)

## Forward selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="forward")
#lm(formula = y^lambda ~ x8 + x2 + x7 + x9, data = data)

## Backward selection
step(s.full, scope=list(lower=s.null, upper=s.full), direction="backward")
#lm(formula = y^lambda ~ x2 + x7 + x8 + x9, data = data)

## Stepwise selection
step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")
#same

# evaluate the best model
best.lm <- lm((y^lambda)~x2+x7+x8+x9, data=data) 

summary(best.lm) #x9 does not appear to be significant (what we saw earlier)
cor(data[,c(3,8,9,10)]) #7 and 8 show moderate correlation

vif(best.lm) 
#   x2     x7     x8     x9 
#1.1832 2.1821 2.4966 1.3043
ti <- rstudent(best.lm)
yhat <- fitted(best.lm)
summary(influence.measures(best.lm)) #All look okay
inv_pos <- as.numeric(row.names(summary(influence.measures(best.lm))))
plot(yhat,ti)
textxy(yhat[inv_pos],ti[inv_pos],inv_pos)
qqnorm(ti)
qqline(ti)
plot(yhat,ti)
plot(data$x2,ti) #seen before
plot(data$x7,ti) #seen before
plot(data$x8,ti) #seen before
plot(data$x9,ti) #Looks okay


#Try removing potential leverage/influence points...
data3 = data[-c(18,27),]
final = lm(y~x2+x7+x8,data=data3)
PRESS(final) #89.82869
summary(final) #not better than original (likely due to x8 overfitting in a different way once the
#two observations were removed)
PRESS(nfl.lm) #87.46123
summary(nfl.lm)

#### Final Variable Selection / Evaluation ####
## Partial F test for majorly insignificant variables
full.mod <- lm(y~.-wt, data=data)
PRESS(full.mod) #145.9139
bc.mod <- lm(y^lambda~.-wt,data=data)
PRESS(bc.mod) #55.2787
red.mod <- lm(y ~ x2+x7+x8+x9, data=data)
PRESS(red.mod) #87.65965
redbc.mod <- lm(y^lambda ~ x2+x7+x8+x9, data=data)
PRESS(redbc.mod) #33.56449

anova(redbc.mod, bc.mod)
#Model 1: y^lambda ~ x2 + x7 + x8 + x9
#Model 2: y^lambda ~ (x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9)
#   Res.Df    RSS     Df  Sum of Sq   F   Pr(>F)
#1     23     25.011                           
#2     18     23.275  5    1.7365   0.2686 0.9244 #NOT SIGNIFICANT - SO SELECT MODEL 1

## Reassess reduced model
summary(redbc.mod)
#x9 shows no significance, though x7 now does

#Update model:
redredbc.mod <- lm(y^lambda ~ x2+x7+x8, data=data) #Choose the 4 variables with the highest significance

anova(redredbc.mod, redbc.mod)
#Insignificant, so choose a model with x2, x7,x8 (not x9)

nfl.lm <- redredbc.mod
summary(nfl.lm)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.3089438  4.8990806  -0.063 0.950240    
#x2           0.0021888  0.0004309   5.079  3.4e-05 ***
#x7           0.1179921  0.0547109   2.157 0.041267 *  
#x8          -0.0029979  0.0007918  -3.786 0.000903 ***

#Residual standard error: 1.058 on 24 degrees of freedom
#Multiple R-squared:  0.7836,	Adjusted R-squared:  0.7565 
#F-statistic: 28.97 on 3 and 24 DF,  p-value: 3.803e-08

#Here, we can see that just these three regressors explain ~76% of the variance of the response (which
#is relatively close to the original ~80%)

PRESS(nfl.lm) #33.81536 (VERY slightly bigger than with x9, but worth it for a simpler model)

## View predicted values
fitted(nfl.lm)

## View confidence interval for each coefficient
confint(nfl.lm, level=0.95)

## View confidence interval for the mean of a subpopulation
newdata <- data.frame(x1=2000,x2=2001,x3=37,x4=52,x5=3,x6=700,x7=57.0,x8=1900,x9=2002)

predict(nfl.lm,newdata,interval="confidence")
#       fit      lwr     upr
#1 5.100385 4.45927 5.7415

## View prediction interval for the mean of a subpopulation
predict(nfl.lm,newdata,interval="prediction")
#       fit      lwr      upr
#1 5.100385 2.824636 7.376134

#### Cross Validation ####
## Split data into two groups and cross-validate
s.cv <- cv.lm(data=data, form.lm=nfl.lm, m=7, plotit=F)
#mse = 1.2
s.cv <- cv.lm(data=data, form.lm=redbc.mod, m=7, plotit=F)
#ms = 1.16 (marginally better, not worth including x9)

 #Should do this up front
#### Try Adding an Indicator Variable ####
#nfl.lm2 <- lm(y~x2+x7+x8+I(x5>0)+I(x5<0), data = data)
#summary(nfl.lm2) #indicator still not significant and x7 gets worse
#PRESS(nfl.lm) #87.46123


#### Logistic Regression ####
## Logistic regression using the logit function
bd.glm <- glm(y~., data=bddata, family=binomial)
summary(bd.glm)

## Assess goodness-of-fit using deviance 
1-pchisq(664.74, df=742)

## Test variables using the likelihood ratio test
bd.glm.red <- glm(y~.-x3, data=bddata, family=binomial)
lrtest(bd.glm, bd.glm.red)

## Use iterative procedures using the likelihood ratio test
bd.null <- glm(y~1, data=bddata, family=binomial)
bd.full <- glm(y~., data=bddata, family=binomial)

step(bd.null, scope=list(lower=bd.null, upper=bd.full), direction="both", test="LRT")

## Predict the probability of success
options(scipen=999)
bd.pred <- predict(bd.glm.red, type="response")

## Assess deviance residuals and influential point measures
bd.resid <- residuals(bd.glm.red, type="deviance")
plot(bd.pred,bd.resid)

summary(influence.measures(bd.glm.red))

## Using Receiver Operating Characteristic (ROC) curve and Area Under the Curve (AUC)
bddata$prob <- bd.pred

bd.roc <- roc(y ~ prob, data = bddata)
plot(bd.roc)  #This is not for this data set

#### END ####

