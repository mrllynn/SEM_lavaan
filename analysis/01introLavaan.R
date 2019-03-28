### R code from vignette source '01introLavaan.Rtex'

###################################################
### code chunk number 1: 01introLavaan.Rtex:184-193
###################################################
# create artificial data
set.seed(1)
x1 <- rnorm(100) * 10; x2 <- rnorm(100) * 10
x3 <- rnorm(100) * 10;  x4 <- rnorm(100) * 10
y <- 100 + 5*x1 + (-2)*x2 + 1*x3 + 0.1*x4 + rnorm(100, sd=40)
myData <- data.frame(y,x1,x2,x3,x4)

fit <- lm(formula = y ~ x1 + x2 + x3 + x4,
          data    = myData)


###################################################
### code chunk number 2: 01introLavaan.Rtex:198-199
###################################################
summary(fit)


###################################################
### code chunk number 3: 01introLavaan.Rtex:250-260
###################################################
library(lavaan)

myModel <- ' y ~ x1 + x2 + x3 + x4 '

# fit model
fit <- sem(model = myModel,
           data  = myData)

# show results
#summary(fit, nd = 4)


###################################################
### code chunk number 4: 01introLavaan.Rtex:267-268
###################################################
summary(fit, nd = 4)


###################################################
### code chunk number 5: 01introLavaan.Rtex:567-574
###################################################
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
            '

fit <- cfa(model = HS.model,
           data  = HolzingerSwineford1939)


###################################################
### code chunk number 6: 01introLavaan.Rtex:580-582
###################################################
summary(fit, fit.measures = TRUE,
             standardized = TRUE)


###################################################
### code chunk number 7: 01introLavaan.Rtex:657-677
###################################################
model <- '
  # latent variable definitions
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + a*y2 + b*y3 + c*y4
    dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual covariances
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

# 2. fitting the model using the sem() function
fit <- sem(model, data = PoliticalDemocracy)


###################################################
### code chunk number 8: 01introLavaan.Rtex:683-684
###################################################
summary(fit, standardized = TRUE)


###################################################
### code chunk number 9: 01introLavaan.Rtex:819-820
###################################################
lavOptions()


###################################################
### code chunk number 10: 01introLavaan.Rtex:952-954
###################################################
fit <- cfa(HS.model, data = HolzingerSwineford1939)
fitted(fit)


###################################################
### code chunk number 11: 01introLavaan.Rtex:959-964
###################################################
lavInspect(fit)
lavInspect(fit, "sampstat")
lavInspect(fit, "cov.lv")
lavTech(fit, "cov.lv")
lavTech(fit, "cov.lv", add.labels = TRUE, drop.list.single.group = TRUE)


###################################################
### code chunk number 12: 01introLavaan.Rtex:969-970
###################################################
fitMeasures(fit)


###################################################
### code chunk number 13: 01introLavaan.Rtex:975-976
###################################################
parameterTable(fit)[1:21,1:13]


###################################################
### code chunk number 14: 01introLavaan.Rtex:981-982
###################################################
parameterEstimates(fit)[1:21,]


###################################################
### code chunk number 15: 01introLavaan.Rtex:987-988
###################################################
modindices(fit, sort = TRUE, minimum.value = 5)


###################################################
### code chunk number 16: 01introLavaan.Rtex:993-994
###################################################
lavTestScore(fit, add = "visual =~ x9")


###################################################
### code chunk number 17: 01introLavaan.Rtex:999-1000
###################################################
lavResiduals(fit)


###################################################
### code chunk number 18: 01introLavaan.Rtex:1005-1007
###################################################
fit0 <- update(fit, orthogonal = TRUE)
lavTestLRT(fit0, fit)


