#R code from vignette source 'lavaan_labs_zurich2019.Rtex'

###################################################
#code chunk number 1: lavaan_labs_zurich2019.Rtex:56-58 (eval = FALSE)
###################################################
library(lavaan)
?HolzingerSwineford1939


###################################################
#code chunk number 2: lavaan_labs_zurich2019.Rtex:80-85 (eval = FALSE)
###################################################
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
            '
fit <- cfa(HS.model, data=HolzingerSwineford1939)


###################################################
#code chunk number 3: lavaan_labs_zurich2019.Rtex:89-90 (eval = FALSE)
###################################################
summary(fit, fit.measures = TRUE)


###################################################
#code chunk number 4: lavaan_labs_zurich2019.Rtex:100-103 (eval = FALSE)
###################################################
Data9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9")]
S <- cov(Data9)
round(S, 3)


###################################################
#code chunk number 5: lavaan_labs_zurich2019.Rtex:107-108 (eval = FALSE)
###################################################
inspect(fit, "sampstat") # alias for lavInspect()


###################################################
#code chunk number 6: lavaan_labs_zurich2019.Rtex:111-112 (eval = FALSE)
###################################################
fitted(fit)


###################################################
#code chunk number 7: lavaan_labs_zurich2019.Rtex:120-122 (eval = FALSE)
###################################################
attach(inspect(fit, "est"))
# Sigma.hat <- ??


###################################################
#code chunk number 8: lavaan_labs_zurich2019.Rtex:126-128 (eval = FALSE)
###################################################
fit.ortho <- cfa(HS.model, data=HolzingerSwineford1939, orthogonal=TRUE)
anova(fit, fit.ortho)


###################################################
#code chunk number 9: lavaan_labs_zurich2019.Rtex:131-135 (eval = FALSE)
###################################################
parameterEstimates(fit)
coef(fit)
resid(fit)
resid(fit, type="standardized")


###################################################
#code chunk number 10: lavaan_labs_zurich2019.Rtex:138-141 (eval = FALSE)
###################################################
inspect(fit)           # free paraeters
inspect(fit, "start")  # starting values
parTable(fit)          # or parameterTable(fit)


###################################################
#code chunk number 11: lavaan_labs_zurich2019.Rtex:158-178 (eval = FALSE)
###################################################
model <- ' 
# latent variable definitions
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

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
fit <- sem(model, data=PoliticalDemocracy)
summary(fit)
summary(fit, standardized=TRUE, rsquare=TRUE, fit.measures=TRUE)


###################################################
#code chunk number 12: lavaan_labs_zurich2019.Rtex:181-183 (eval = FALSE)
###################################################
parameterTable(fit)
# alias: parTable(fit)


###################################################
#code chunk number 13: lavaan_labs_zurich2019.Rtex:188-189 (eval = FALSE)
###################################################
lavaanify(model)


###################################################
#code chunk number 14: lavaan_labs_zurich2019.Rtex:195-199 (eval = FALSE)
###################################################
# first specify the model in a model object `model.equal'
# next, fit the model and call the fitted model `fit.equal'
# lastly, compare the two models as follows
anova(fit.equal, fit)


###################################################
#code chunk number 15: lavaan_labs_zurich2019.Rtex:203-205 (eval = FALSE)
###################################################
# after the modified model has been specified and fitted:
modindices(fit, sort = TRUE, max = 5)


###################################################
#code chunk number 16: lavaan_labs_zurich2019.Rtex:209-210 (eval = FALSE)
###################################################
?inspect


###################################################
#code chunk number 17: lavaan_labs_zurich2019.Rtex:218-222 (eval = FALSE)
###################################################
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
            '


###################################################
#code chunk number 18: lavaan_labs_zurich2019.Rtex:227-233 (eval = FALSE)
###################################################
library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
            '
fit <- cfa(HS.model, data=HolzingerSwineford1939)


###################################################
#code chunk number 19: lavaan_labs_zurich2019.Rtex:237-238 (eval = FALSE)
###################################################
fit <- cfa(HS.model, data=HolzingerSwineford1939, meanstructure=TRUE)


###################################################
#code chunk number 20: lavaan_labs_zurich2019.Rtex:242-244 (eval = FALSE)
###################################################
fit <- cfa(HS.model, data=HolzingerSwineford1939, group="school")
summary(fit)


###################################################
#code chunk number 21: lavaan_labs_zurich2019.Rtex:249-253 (eval = FALSE)
###################################################
Group1 <- subset(HolzingerSwineford1939, school=="Pasteur")
Group2 <- subset(HolzingerSwineford1939, school=="Grant-White")
fit1 <- cfa(HS.model, data=Group1)
fit2 <- cfa(HS.model, data=Group2)


###################################################
#code chunk number 22: lavaan_labs_zurich2019.Rtex:258-261 (eval = FALSE)
###################################################
fit <- cfa(HS.model, data=HolzingerSwineford1939, group="school", 
           group.equal=c("loadings", "intercepts"))
summary(fit)


###################################################
#code chunk number 23: lavaan_labs_zurich2019.Rtex:269-273 (eval = FALSE)
###################################################
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
            '


###################################################
#code chunk number 24: lavaan_labs_zurich2019.Rtex:277-280 (eval = FALSE)
###################################################
set.seed(1234)
HS.missing <- as.data.frame(lapply(HolzingerSwineford1939, function(x) {
                  idx <- sample(1:length(x), 5); x[idx] <- NA; x}))


###################################################
#code chunk number 25: lavaan_labs_zurich2019.Rtex:286-287 (eval = FALSE)
###################################################
fit <- cfa(HS.model, data=HS.missing)


###################################################
#code chunk number 26: lavaan_labs_zurich2019.Rtex:293-295 (eval = FALSE)
###################################################
fit <- cfa(HS.model, data=HS.missing, missing="ml", verbose=TRUE)
summary(fit)


###################################################
#code chunk number 27: lavaan_labs_zurich2019.Rtex:298-299 (eval = FALSE)
###################################################
inspect(fit, "patterns")


###################################################
#code chunk number 28: lavaan_labs_zurich2019.Rtex:302-303 (eval = FALSE)
###################################################
inspect(fit, "coverage")


###################################################
#code chunk number 29: lavaan_labs_zurich2019.Rtex:312-329 (eval = FALSE)
###################################################
model <- '
# latent variable definitions
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8

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


###################################################
#code chunk number 30: lavaan_labs_zurich2019.Rtex:335-337 (eval = FALSE)
###################################################
fit <- sem(model, data=PoliticalDemocracy, se="robust")
summary(fit)


###################################################
#code chunk number 31: lavaan_labs_zurich2019.Rtex:341-344 (eval = FALSE)
###################################################
fit <- sem(model, data=PoliticalDemocracy, se="bootstrap", bootstrap=200,
           verbose=TRUE)
summary(fit)


###################################################
#code chunk number 32: lavaan_labs_zurich2019.Rtex:349-351 (eval = FALSE)
###################################################
fit <- sem(model, data=PoliticalDemocracy, test="Satorra-Bentler")
summary(fit, fit.measures=TRUE)


###################################################
#code chunk number 33: lavaan_labs_zurich2019.Rtex:354-357 (eval = FALSE)
###################################################
fit <- sem(model, data=PoliticalDemocracy, test="bootstrap", bootstrap=200,
           verbose=TRUE)
fit


###################################################
#code chunk number 34: lavaan_labs_zurich2019.Rtex:361-363 (eval = FALSE)
###################################################
fit <- sem(model, data=PoliticalDemocracy, estimator="MLR")
summary(fit)


###################################################
#code chunk number 35: lavaan_labs_zurich2019.Rtex:375-377 (eval = FALSE)
###################################################
HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5","x6","x7","x8","x9")]
HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )


###################################################
#code chunk number 36: lavaan_labs_zurich2019.Rtex:384-387 (eval = FALSE)
###################################################
model <- ' trait =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 '
fit <- cfa(model, data=HSbinary, ordered=names(HSbinary), std.lv=TRUE)
summary(fit)


###################################################
#code chunk number 37: lavaan_labs_zurich2019.Rtex:391-393 (eval = FALSE)
###################################################
inspect(fit)
fitted(fit)


###################################################
#code chunk number 38: lavaan_labs_zurich2019.Rtex:397-403 (eval = FALSE)
###################################################
x <- HSbinary$x1
N <- length(x)
prop <- table(x)/N
ncat <- length(prop)
prop.cumulative <- cumsum(prop)
thresholds <- qnorm(prop.cumulative)[-ncat]


###################################################
#code chunk number 39: lavaan_labs_zurich2019.Rtex:411-417 (eval = FALSE)
###################################################
pe <- parameterEstimates(fit)
lambda <- pe$est[ pe$op == "=~" ]
tau <- pe$est[ pe$op == "|" ]
rvar <- pe$est[ pe$op == "~~" & pe$lhs != "trait"]
item.discrimination <- lambda/sqrt(rvar)
item.difficulty <- tau/lambda


###################################################
#code chunk number 40: lavaan_labs_zurich2019.Rtex:428-433 (eval = FALSE)
###################################################
HS9 <- HolzingerSwineford1939[,c("x1","x2","x3","x4","x5",
                                 "x6","x7","x8","x9")]
HSbinary <- as.data.frame( lapply(HS9, cut, 2, labels=FALSE) )
HSbinary$school <- HolzingerSwineford1939$school
head(HSbinary)


###################################################
#code chunk number 41: lavaan_labs_zurich2019.Rtex:438-439 (eval = FALSE)
###################################################
varTable(HSbinary)


###################################################
#code chunk number 42: lavaan_labs_zurich2019.Rtex:442-451 (eval = FALSE)
###################################################
# single factor model
model <- ' visual  =~ x1 + x2 + x3
           textual =~ x4 + x5 + x6
           speed   =~ x7 + x8 + x9 '

# binary CFA
fit <- cfa(model, data=HSbinary,
           ordered=c("x1","x2","x3","x4","x5","x6","x7", "x8","x9"))
summary(fit, fit.measures=TRUE)


###################################################
#code chunk number 43: lavaan_labs_zurich2019.Rtex:455-458 (eval = FALSE)
###################################################
fitted(fit)
parTable(fit)
inspect(fit)


###################################################
#code chunk number 44: lavaan_labs_zurich2019.Rtex:469-480 (eval = FALSE)
###################################################
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


###################################################
#code chunk number 45: lavaan_labs_zurich2019.Rtex:488-513 (eval = FALSE)
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
summary(fit1, standardized = TRUE)


###################################################
#code chunk number 46: lavaan_labs_zurich2019.Rtex:532-546 (eval = FALSE)
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
#code chunk number 47: lavaan_labs_zurich2019.Rtex:570-575 (eval = FALSE)
###################################################
# change the working directory to where you stored the datafile:
# setwd("c:/temp/")
FamIQData <- read.table("FamIQData.dat")
names(FamIQData) <- c("family", "child", "wordlist", "cards", "matrices",
                      "figures", "animals", "occupats")


###################################################
#code chunk number 48: lavaan_labs_zurich2019.Rtex:581-583 (eval = FALSE)
###################################################
dim(FamIQData)
summary(FamIQData)


###################################################
#code chunk number 49: lavaan_labs_zurich2019.Rtex:586-587 (eval = FALSE)
###################################################
length(table(FamIQData$family))


###################################################
#code chunk number 50: lavaan_labs_zurich2019.Rtex:590-591 (eval = FALSE)
###################################################
table(table(FamIQData$family))


###################################################
#code chunk number 51: lavaan_labs_zurich2019.Rtex:595-596 (eval = FALSE)
###################################################
hist(FamIQData$wordlist)


###################################################
#code chunk number 52: lavaan_labs_zurich2019.Rtex:609-627 (eval = FALSE)
###################################################
library(lavaan)
model1 <- '
   level: 1
     numeric    =~ wordlist + cards + matrices
     perception =~ figures + animals + occupats
   level: 2
     wordlist ~~ 0*wordlist
     cards    ~~ 0*cards
     matrices ~~ 0*matrices
     figures  ~~ 0*figures
     animals  ~~ 0*animals
     occupats ~~ 0*occupats
 '

fit1 <- sem(model1, data = FamIQData, cluster = "family",
           std.lv = TRUE, verbose = TRUE)

summary(fit1)


###################################################
#code chunk number 53: lavaan_labs_zurich2019.Rtex:649-655 (eval = FALSE)
###################################################
# change the working directory to where you stored the datafile:
# setwd("c:/temp/")
Galo <- read.table("Galo.dat")
names(Galo) <- c("school", "sex", "galo", "advice", "feduc", "meduc",
                 "focc", "denom")
Galo[Galo == 999] <- NA


###################################################
#code chunk number 54: lavaan_labs_zurich2019.Rtex:661-663 (eval = FALSE)
###################################################
dim(Galo)
summary(Galo)


###################################################
#code chunk number 55: lavaan_labs_zurich2019.Rtex:666-667 (eval = FALSE)
###################################################
length(table(Galo$school))


###################################################
#code chunk number 56: lavaan_labs_zurich2019.Rtex:670-671 (eval = FALSE)
###################################################
table(table(Galo$school))


###################################################
#code chunk number 57: lavaan_labs_zurich2019.Rtex:678-701 (eval = FALSE)
###################################################
model <- '
    level: within
        wses =~ a*focc + b*meduc + c*feduc
        # residual correlation
        focc ~~ feduc

        advice ~ wc*wses + wb*galo
        galo   ~ wa*wses

    level: between
        bses =~ a*focc + b*meduc + c*feduc
        feduc ~~ 0*feduc

        advice ~ bc*bses + bb*galo
        galo   ~ ba*bses + denom

    # defined parameters
    wi := wa * wb
    bi := ba * bb
'
fit <- sem(model, data = Galo, cluster = "school", fixed.x = FALSE,
           verbose = FALSE, std.lv = TRUE, h1 = TRUE)
summary(fit)


