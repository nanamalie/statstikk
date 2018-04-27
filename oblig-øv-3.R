dta = read.csv("prosjektdata2.csv", sep=",")
setwd("C:/Users/Eier/Desktop/Prosjekt/Prosjekt")
dta

full = lm(formula = treff ~ ., data = dta)
summary(full)

library(leaps)
allsubs <- regsubsets(treff ~ . , data = dta , nvmax = 8)
allsummary <- summary(allsubs)
allsummary$outmat
allsummary$bic
which.min(allsummary$bic)
allsummary$adjr2
which.max(allsummary$adjr2)

fill = lm(formula = treff ~ .-(BC + ABC), data = dta)
summary(fill)
anova(full)

full2 <- lm(treff ~ (A+B+C)^3, data = dta)
summary(full2)
model.matrix(full)
model.matrix(full2)
cbind(model.matrix(full),dta$treff)

library(FrF2)
plan <- FrF2(nruns=16,nfactors=3,randomize=FALSE)
plan
plan <- add.response(plan, dta$treff)
plan
# now we have an ordinary data set up to be used with lm
lm3 <- lm(dta.treff~((A+B+C)^3), data=plan)
summary(lm3)
model.matrix(lm3)

cubePlot(lm3,"A","B","C", main = "Cubeplot for treff")
MEPlot(lm3, main = "Main effects plot for treff") # main effects plot
IAPlot(lm3, main = "Interaction plot matrix for treff") # interaction effect plots
effects <- 2*lm3$coeff
effects

# moving on to inference
# all residuals are zero, since we have 16 observations and have fitted 16 parameters
lm3$resid
# Total sum of squares, SST, is equal to the regression sum of squares, SSR, since SSE = 0:
sum((lm3$fitted-mean(dta$treff))^2) # regression sum of squares by definition
16*sum(lm3$coeff[-1]^2) # and by theory for two-level factorial design
sum((dta$treff-mean(dta$treff))^2) # total sum of squares

# (1) Lenth's metod

qnorm(.25) # The 0.674 of Lenth's method

effects <- 2*lm3$coefficients[-1] # omit intercept
abseffects<-abs(effects)
medabseffects <- median(abseffects)
tauest <- 1.5*medabseffects
keepeffects <- abseffects[abseffects< 2.5*tauest]
taustar <- 1.5*median(keepeffects)
alpha<-0.05 # significance level
cutoff<-qt(1-alpha/2,length(abseffects)/3)*taustar
cutoff
effects # effect of B significant

library(BsMD)
values<-LenthPlot(lm3) # ME = cutoff. Plot as a "side effect".

lm1 <- lm(dta.treff~.,data=plan)
summary(lm1)
sdbetahatest<-sqrt(summary(lm1)$sigma^2/8) # st. errors shown in summary table
sse <- sum((dta$treff-lm1$fitted.values)^2)
summary(lm1)$sigma^2*(8-4) # the same
16*sum(lm3$coeff[5:8]^2) # this function of removed covariates of full model is equal to sse of reduced model
2*pt(abs(lm1$coeff/sdbetahatest),16-3-1,lower.tail=FALSE) # p-values, the same as in summary(lm1)
# effects <- 2*lm1$coeff
# effects

# model check by residual plots
# 1. fitted vs studentized residuals
par(xpd=FALSE)
rres <- rstudent(lm1)
plot(lm1$fitted,rres)
abline(h=0)

# 2. if (1) strange, each x vs. studentized residuals
# hard with -1 and 1 as only values

# 3. normality of residuals
qqnorm(rres)
qqline(rres)
library(nortest)
ad.test(rstudent(lm1))
