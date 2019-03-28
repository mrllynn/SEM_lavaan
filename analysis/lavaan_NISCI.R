

rm(list=ls())

library("EMSCI")
library("lavaan")

data("EMSCI0123")
head(EMSCI0123)

HS.model <- ' UEMS0  =~ sex + ageatDOI + segblw + trtmt
              UEMS3  =~ sex + ageatDOI + segblw + trtmt
              UEMS3 ~~ * UEMS3
              UEMS0 ~~ * UEMS0
              speed   =~ x7 + x8 + x9
            '
fit <- cfa(HS.model, data=HolzingerSwineford1939)


###################################################
#code chunk number 3: lavaan_labs_zurich2019.Rtex:89-90 (eval = FALSE)
###################################################
summary(fit, fit.measures = TRUE)