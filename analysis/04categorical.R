### R code from vignette source '04categorical.Rtex'

###################################################
### code chunk number 1: 04categorical.Rtex:134-146
###################################################
set.seed(1234)
# generate `ordered' data with 4 categories
Y <- sample(1:4, size = 100, replace = TRUE)
# construct table of proportions
prop <- table(Y)/sum(table(Y))
prop
# cumulative proportions
cprop <- c(0, cumsum(prop))
cprop
# convert quantiles to z-scores
th <- qnorm(cprop)
th


###################################################
### code chunk number 2: 04categorical.Rtex:150-156
###################################################
library(MASS)
X1 <- rnorm(100); X2 <- rnorm(100); X3 <- rnorm(100)
# fit ordered probit regression
fit <- polr(ordered(Y) ~ X1 + X2 + X3, method = "probit")
# (residual) thresholds
fit$zeta


###################################################
### code chunk number 3: 04categorical.Rtex:186-195
###################################################
library(lavaan)
# create some random correlated data
set.seed(1234)
Y12 <- MASS:::mvrnorm(n = 100, mu = c(0,0), 
                      Sigma = matrix(c(1,0.5,0.5,1), 2, 2))
# transform to binary
y1 <- cut(Y12[,1], breaks = c(-Inf, 0, +Inf), labels = FALSE)
y2 <- cut(Y12[,2], breaks = c(-Inf, 0, +Inf), labels = FALSE)
Data <- data.frame(y1 = y1, y2 = y2)


###################################################
### code chunk number 4: 04categorical.Rtex:198-200
###################################################
# compute tetrachoric correlation
lavCor(Data, ordered = c("y1", "y2"))


###################################################
### code chunk number 5: 04categorical.Rtex:210-213
###################################################
fit <- sem('y1 ~~ y2', data = Data, ordered = c("y1", "y2"), 
           estimator = "WLS")
lavInspect(fit, "Gamma")


###################################################
### code chunk number 6: 04categorical.Rtex:307-315
###################################################
x <- c(3,4,5)
class(x)

x <- factor(x)
class(x)

x <- ordered(x)
class(x)


###################################################
### code chunk number 7: 04categorical.Rtex:324-326
###################################################
# library(lavaan)
varTable(HolzingerSwineford1939)


###################################################
### code chunk number 8: 04categorical.Rtex:360-364
###################################################
# binary version of Holzinger & Swineford
HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
                                 "x6","x7","x8","x9")]
HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels = FALSE) )


###################################################
### code chunk number 9: 04categorical.Rtex:366-370
###################################################
# single factor model
model <- ' visual  =~ x1 + x2 + x3
           textual =~ x4 + x5 + x6
           speed   =~ x7 + x8 + x9 '


###################################################
### code chunk number 10: 04categorical.Rtex:373-375
###################################################
# binary CFA
fit <- cfa(model, data=HSbinary, ordered = names(HSbinary))


###################################################
### code chunk number 11: 04categorical.Rtex:380-381
###################################################
summary(fit, fit.measures = TRUE, standardized = TRUE)


###################################################
### code chunk number 12: 04categorical.Rtex:386-387
###################################################
lavInspect(fit, "sampstat")


###################################################
### code chunk number 13: 04categorical.Rtex:440-441
###################################################
inspect(fit)


###################################################
### code chunk number 14: 04categorical.Rtex:446-447
###################################################
lavTables(fit, dim = 1)


###################################################
### code chunk number 15: 04categorical.Rtex:452-453
###################################################
head( lavTables(fit, dim = 2), 16)


