### R code from vignette source 'longitudinalShort.Rtex'

###################################################
### code chunk number 1: longitudinalShort.Rtex:151-164
###################################################
library(lavaan)
MEAN <- c(3.06893, 2.92590, 3.11013, 3.02577, 2.85656, 3.09346)
SDS  <- c(0.84194, 0.88934, 0.83470, 0.84081, 0.90864, 0.83984)
lower <- '
    1.00000
    0.55226    1.00000
    0.56256    0.60307    1.00000
    0.31889    0.35898    0.27757    1.00000
    0.24363    0.35798    0.31889    0.56014    1.00000
    0.32217    0.36385    0.32072    0.56164    0.59738    1.00000 '
COV <- getCov(lower, sds=SDS, names = c("Glad1", "Cheer1", "Happy1", 
                                        "Glad2", "Cheer2", "Happy2"))
COV


###################################################
### code chunk number 2: longitudinalShort.Rtex:169-195
###################################################
model1 <- '
    posAffect1 =~ 1*Glad1 + Cheer1 + Happy1
    posAffect2 =~ 1*Glad2 + Cheer2 + Happy2
    posAffect1 ~~ posAffect2

    # intercepts
    Glad1  ~ 1
    Glad2  ~ 1
    Cheer1 ~ 1
    Cheer2 ~ 1
    Happy1 ~ 1
    Happy2 ~ 1

    # residual covariances
    Glad1  ~~ Glad2
    Cheer1 ~~ Cheer2
    Happy1 ~~ Happy2

    # latent means: fixed to zero
    posAffect1 ~ 0
    posAffect2 ~ 0
'

fit1 <- lavaan(model1, sample.cov = COV, sample.mean = MEAN,
               sample.nobs = 823, auto.var = TRUE)
# summary(fit1, standardized = TRUE)


###################################################
### code chunk number 3: longitudinalShort.Rtex:200-226
###################################################
model2 <- '
    posAffect1 =~ 1*Glad1 + ch*Cheer1 + ha*Happy1
    posAffect2 =~ 1*Glad2 + ch*Cheer2 + ha*Happy2
    posAffect1 ~~ posAffect2

    # intercepts
    Glad1  ~ 1
    Glad2  ~ 1
    Cheer1 ~ 1
    Cheer2 ~ 1
    Happy1 ~ 1
    Happy2 ~ 1

    # residual covariances
    Glad1  ~~ Glad2
    Cheer1 ~~ Cheer2
    Happy1 ~~ Happy2

    # latent means: fixed to zero
    posAffect1 ~ 0
    posAffect2 ~ 0
'

fit2 <- lavaan(model2, sample.cov = COV, sample.mean = MEAN,
               sample.nobs = 823, auto.var = TRUE)
# summary(fit2, standardized = TRUE)


###################################################
### code chunk number 4: longitudinalShort.Rtex:233-234
###################################################
anova(fit1, fit2)


###################################################
### code chunk number 5: longitudinalShort.Rtex:243-269
###################################################
model3 <- '
    posAffect1 =~ 1*Glad1 + ch*Cheer1 + ha*Happy1
    posAffect2 =~ 1*Glad2 + ch*Cheer2 + ha*Happy2
    posAffect1 ~~ posAffect2
    
    # intercepts
    Glad1  ~ igl*1
    Glad2  ~ igl*1
    Cheer1 ~ ich*1
    Cheer2 ~ ich*1
    Happy1 ~ iha*1
    Happy2 ~ iha*1

    # residual covariances
    Glad1  ~~ Glad2
    Cheer1 ~~ Cheer2
    Happy1 ~~ Happy2

    # latent means: fixed to zero
    posAffect1 ~ 0*1 # baseline
    posAffect2 ~ 1   # difference compared to baseline
'

fit3 <- lavaan(model3, sample.cov = COV, sample.mean = MEAN,
               sample.nobs = 823, auto.var = TRUE)
summary(fit3, standardized = TRUE)


###################################################
### code chunk number 6: longitudinalShort.Rtex:276-277
###################################################
anova(fit2, fit3)


###################################################
### code chunk number 7: longitudinalShort.Rtex:293-318
###################################################
model4 <- '
    posAffect1 =~ 1*Glad1 + ch*Cheer1 + ha*Happy1
    posAffect2 =~ 1*Glad2 + ch*Cheer2 + ha*Happy2
    posAffect1 ~~ posAffect2

    # intercepts
    Glad1  ~ igl*1
    Glad2  ~ igl*1
    Cheer1 ~ ich*1
    Cheer2 ~ ich*1
    Happy1 ~ iha*1
    Happy2 ~ iha*1

    # residual covariances
    Glad1  ~~ Glad2
    Cheer1 ~~ Cheer2
    Happy1 ~~ Happy2

    # latent means: fixed to zero
    posAffect1 ~ 0*1 # baseline
    posAffect2 ~ 0*1 # equal means, both equal to zero
'

fit4 <- lavaan(model4, sample.cov = COV, sample.mean = MEAN,
               sample.nobs = 823, auto.var = TRUE)


###################################################
### code chunk number 8: longitudinalShort.Rtex:324-325
###################################################
anova(fit3, fit4)


###################################################
### code chunk number 9: longitudinalShort.Rtex:632-646
###################################################
lower <- '
 2.926
 1.390 4.257
 1.698 2.781 4.536
 1.628 2.437 2.979 5.605
 1.240 0.789 0.903 1.278 3.208
 0.592 1.890 1.419 1.004 1.706 3.994
 0.929 1.278 1.900 1.000 1.567 1.654 3.583
 0.659 0.949 1.731 2.420 0.988 1.170 1.146 3.649 '

COV <- getCov(lower, names=c("anti1", "anti2", "anti3", "anti4",
                             "dep1", "dep2", "dep3", "dep4"))

MEANS <- c(1.750, 1.928, 1.978, 2.322, 2.178, 2.489, 2.294, 2.222)


###################################################
### code chunk number 10: longitudinalShort.Rtex:661-675
###################################################
model <- '
  anti2 ~ a21*anti1
  anti3 ~ a32*anti2
  anti4 ~ a43*anti3

  # one variance
  anti1 ~~ anti1

  # three (unqequal) residual variances
  anti2 ~~ anti2; anti3 ~~ anti3; anti4 ~~ anti4  
'
fit <- lavaan(model, sample.cov=COV, sample.mean=MEANS, sample.nobs=180,
              sample.cov.rescale=FALSE, mimic="EQS")
summary(fit)


###################################################
### code chunk number 11: longitudinalShort.Rtex:685-717
###################################################
model <- '
  # antisocial behavior
  anti2 ~ a*anti1
  anti3 ~ a*anti2
  anti4 ~ a*anti3

  # variances + residuals
  anti1 ~~ anti1; anti2 ~~ ra*anti2; anti3 ~~ ra*anti3; anti4 ~~ ra*anti4

  # depressive symptomatology
  dep2 ~ d*dep1
  dep3 ~ d*dep2
  dep4 ~ d*dep3

  # variances + residuals
  dep1 ~~ dep1; dep2 ~~ rd*dep2; dep3 ~~ rd*dep3; dep4 ~~ rd*dep4

  # crosslagged effects
  anti2 ~ ad*dep1
  anti3 ~ ad*dep2
  anti4 ~ ad*dep3

  dep2 ~ da*anti1
  dep3 ~ da*anti2
  dep4 ~ da*anti3

  # correlated residuals within time
  anti1 ~~ dep1; anti2 ~~ c2*dep2; anti3 ~~ c2*dep3; anti4 ~~ c2*dep4
'
fit <- lavaan(model, sample.cov=COV, sample.mean=MEANS, sample.nobs=180,
              sample.cov.rescale=FALSE, mimic="EQS")
summary(fit)


###################################################
### code chunk number 12: longitudinalShort.Rtex:848-858
###################################################
model <- ' 
    # random intercept
    int =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5

    # zero intercepts
    y1 + y2 + y3 + y4 + y5 ~ 0*1

    # free latent intercept
    int ~ 1
'


###################################################
### code chunk number 13: longitudinalShort.Rtex:862-866
###################################################
model <- ' 
    # random intercept
    int =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
'


###################################################
### code chunk number 14: longitudinalShort.Rtex:871-882
###################################################
model <- ' 
    # random intercept
    int =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5

    # free latent intercept and variance
    int ~ 1
    int ~~ int

    # add residual variances
    y1 ~~ y1; y2 ~~ y2; y3 ~~ y3; y4 ~~ y4; y5 ~~ y5
'


###################################################
### code chunk number 15: longitudinalShort.Rtex:998-1018
###################################################
model <- '
  # intercept
  i =~ 1*anti1 + 1*anti2 + 1*anti3 + 1*anti4
  i ~ 1  # mean intercept (fixed effect)
  i ~~ i # variance random intercept

  # slope
  s= ~ 0*anti1 + 1*anti2 + 2*anti3 + 3*anti4
  s ~ 1  # mean slope (fixed effect)
  s ~~ s # variance random slope

  # unequal residual variances
  anti1 ~~ anti1
  anti2 ~~ anti2
  anti3 ~~ anti3
  anti4 ~~ anti4
'
fit <- lavaan(model, sample.cov = COV, sample.mean = MEANS,
              sample.nobs = 180, mimic = "EQS")
summary(fit, fit.measures = TRUE)


