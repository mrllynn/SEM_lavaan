### R code from vignette source '03missingNonNormal.Rtex'

###################################################
### code chunk number 1: 03missingNonNormal.Rtex:79-88
###################################################
library(lavaan)
set.seed(1234)
HS.missing <- as.data.frame(lapply(HolzingerSwineford1939, function(x) {
                            idx <- sample(1:length(x), 5); x[idx] <- NA; x}))
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
'



###################################################
### code chunk number 2: 03missingNonNormal.Rtex:93-101
###################################################
fit <- cfa(HS.model, data = HS.missing, missing = "fiml")
fit
# missing patterns
lavInspect(fit, "patterns")
# percentage complete cases per pair
lavInspect(fit, "coverage")
# sample statistics unrestricted (h1) model
lavInspect(fit, "sampstat.h1")


###################################################
### code chunk number 3: 03missingNonNormal.Rtex:114-116
###################################################
fit <- cfa(HS.model, data = HS.missing, missing = "two.stage")
fit


###################################################
### code chunk number 4: 03missingNonNormal.Rtex:227-231
###################################################
fit <- cfa(HS.model, data = HolzingerSwineford1939,
           estimator = "MLM")

summary(fit, fit.measures = TRUE, estimates = FALSE)


