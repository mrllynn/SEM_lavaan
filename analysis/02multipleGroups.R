### R code from vignette source '02multipleGroups.Rtex'

###################################################
### code chunk number 1: 02multipleGroups.Rtex:3-7
###################################################
library(lavaan)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '


###################################################
### code chunk number 2: 02multipleGroups.Rtex:34-36
###################################################
fit <- cfa(HS.model, data = HolzingerSwineford1939, 
           meanstructure = TRUE)


###################################################
### code chunk number 3: 02multipleGroups.Rtex:48-49
###################################################
summary(fit)


###################################################
### code chunk number 4: 02multipleGroups.Rtex:187-189
###################################################
fit1 <- cfa(HS.model, data = HolzingerSwineford1939, group = "school")
fitMeasures(fit1, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))


###################################################
### code chunk number 5: 02multipleGroups.Rtex:192-195
###################################################
fit2 <- cfa(HS.model, data = HolzingerSwineford1939, group = "school",
            group.equal = "loadings")
fitMeasures(fit2, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))


###################################################
### code chunk number 6: 02multipleGroups.Rtex:198-199
###################################################
anova(fit1, fit2)


###################################################
### code chunk number 7: 02multipleGroups.Rtex:202-205
###################################################
fit3 <- cfa(HS.model, data = HolzingerSwineford1939, group = "school",
            group.equal = c("loadings", "intercepts"))
fitMeasures(fit3, c("chisq", "df", "pvalue", "cfi", "rmsea", "srmr"))


###################################################
### code chunk number 8: 02multipleGroups.Rtex:208-209
###################################################
anova(fit2, fit3)


###################################################
### code chunk number 9: 02multipleGroups.Rtex:215-296
###################################################
# configural model (manual)
HS.model.configural <- '
    visual  =~ c(1,1)*x1 + c(l2.1, l2.2)*x2 + c(l3.1, l3.2)*x3
    textual =~ c(1,1)*x4 + c(l5.1, l5.2)*x5 + c(l6.1, l6.2)*x6
    speed   =~ c(1,1)*x7 + c(l8.1, l8.2)*x8 + c(l9.1, l9.2)*x9

    # ov intercepts
    x1 ~ c(i1.1, i1.2)*1
    x2 ~ c(i2.1, i2.2)*1
    x3 ~ c(i3.1, i3.2)*1
    x4 ~ c(i4.1, i4.2)*1
    x5 ~ c(i5.1, i5.2)*1
    x6 ~ c(i6.1, i6.2)*1
    x7 ~ c(i7.1, i7.2)*1
    x8 ~ c(i8.1, i8.2)*1
    x9 ~ c(i9.1, i9.2)*1

    # lv means (optional, zero by default)
    visual  ~ c(0,0)*1
    textual ~ c(0,0)*1
    speed   ~ c(0,0)*1
'

fit1b <- cfa(HS.model.configural, data = HolzingerSwineford1939,
             group = "school")
# weak invariance model (manual)
# equal factor loadings
HS.model.weak <- '
    visual  =~ c(1,1)*x1 + c(l2, l2)*x2 + c(l3, l3)*x3
    textual =~ c(1,1)*x4 + c(l5, l5)*x5 + c(l6, l6)*x6
    speed   =~ c(1,1)*x7 + c(l8, l8)*x8 + c(l9, l9)*x9

    # ov intercepts
    x1 ~ c(i1.1, i1.2)*1
    x2 ~ c(i2.1, i2.2)*1
    x3 ~ c(i3.1, i3.2)*1
    x4 ~ c(i4.1, i4.2)*1
    x5 ~ c(i5.1, i5.2)*1
    x6 ~ c(i6.1, i6.2)*1
    x7 ~ c(i7.1, i7.2)*1
    x8 ~ c(i8.1, i8.2)*1
    x9 ~ c(i9.1, i9.2)*1

    # lv means (optional, zero by default)
    visual  ~ c(0,0)*1
    textual ~ c(0,0)*1
    speed   ~ c(0,0)*1
'

fit2b <- cfa(HS.model.weak, data = HolzingerSwineford1939,
             group = "school")


# strong invariance model (manual)
#  - equal factor loadings
#  - equal intercepts
#  - free latent means for the second group
HS.model.strong <- '
    visual  =~ c(1,1)*x1 + c(l2, l2)*x2 + c(l3, l3)*x3
    textual =~ c(1,1)*x4 + c(l5, l5)*x5 + c(l6, l6)*x6
    speed   =~ c(1,1)*x7 + c(l8, l8)*x8 + c(l9, l9)*x9

    # ov intercepts
    x1 ~ c(i1, i1)*1
    x2 ~ c(i2, i2)*1
    x3 ~ c(i3, i3)*1
    x4 ~ c(i4, i4)*1
    x5 ~ c(i5, i5)*1
    x6 ~ c(i6, i6)*1
    x7 ~ c(i7, i7)*1
    x8 ~ c(i8, i8)*1
    x9 ~ c(i9, i9)*1

    # lv means
    visual  ~ c(0, NA)*1
    textual ~ c(0, NA)*1
    speed   ~ c(0, NA)*1
'

fit3b <- cfa(HS.model.strong, data = HolzingerSwineford1939,
             group = "school")


###################################################
### code chunk number 10: 02multipleGroups.Rtex:301-302
###################################################
summary(fit3b)


###################################################
### code chunk number 11: 02multipleGroups.Rtex:358-361
###################################################
fit.strong <- cfa(HS.model, data = HolzingerSwineford1939,
                  group = "school",
                  group.equal = c("loadings", "intercepts"))


###################################################
### code chunk number 12: 02multipleGroups.Rtex:364-369
###################################################
PE <- parameterEstimates(fit.strong)
idx <- with(PE, which(op == "~1" &
                      lhs %in% c("visual","textual","speed") &
                      group == 2))
PE[idx,]


###################################################
### code chunk number 13: 02multipleGroups.Rtex:373-378
###################################################
EPC <- lavTestScore(fit.strong, epc = TRUE)$epc
idx <- with(EPC, which(op == "~1" &
                       lhs %in% c("visual","textual","speed") &
                       group == 2))
EPC[idx,]


