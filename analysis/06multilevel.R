### R code from vignette source 'multilevelShort.Rtex'

###################################################
### code chunk number 1: multilevelShort.Rtex:160-162
###################################################
library(lavaan)
head(round(Demo.twolevel[,c(1:4,7:12)], 3), n = 10)


###################################################
### code chunk number 2: multilevelShort.Rtex:188-189
###################################################
options(prompt = "  ", continue = "  ")


###################################################
### code chunk number 3: multilevelShort.Rtex:191-192
###################################################
library(lavaan)


###################################################
### code chunk number 4: multilevelShort.Rtex:194-205
###################################################
model <- '

  level: 1

    y1 ~~ y1

  level: 2

    y1 ~~ y1

'


###################################################
### code chunk number 5: multilevelShort.Rtex:207-210
###################################################
fit <- sem(model, 
           data = Demo.twolevel, 
           cluster = "cluster")


###################################################
### code chunk number 6: multilevelShort.Rtex:212-213 (eval = FALSE)
###################################################
## summary(fit, nd = 4)


###################################################
### code chunk number 7: multilevelShort.Rtex:215-216
###################################################
options(prompt = "> ", continue = "  ")


###################################################
### code chunk number 8: multilevelShort.Rtex:223-225
###################################################
print(parameterEstimates(fit, add.attributes = TRUE, ci = FALSE, 
                         header = FALSE), nd = 4)


###################################################
### code chunk number 9: multilevelShort.Rtex:231-234
###################################################
library(lme4)
fit.lmer <- lmer(y1 ~ 1 + (1 | cluster), data = Demo.twolevel, REML = FALSE)
summary(fit.lmer)


###################################################
### code chunk number 10: multilevelShort.Rtex:259-260
###################################################
options(prompt = "  ", continue = "  ")


###################################################
### code chunk number 11: multilevelShort.Rtex:262-273
###################################################
model <- '

  level: 1

    y1 ~ x1

  level: 2

    y1 ~~ y1

'


###################################################
### code chunk number 12: multilevelShort.Rtex:275-278
###################################################
fit <- sem(model, 
           data = Demo.twolevel, 
           cluster = "cluster")


###################################################
### code chunk number 13: multilevelShort.Rtex:280-281 (eval = FALSE)
###################################################
## summary(fit, nd = 4)


###################################################
### code chunk number 14: multilevelShort.Rtex:283-284
###################################################
options(prompt = "> ", continue = "  ")


###################################################
### code chunk number 15: multilevelShort.Rtex:291-293
###################################################
print(parameterEstimates(fit, add.attributes = TRUE, ci = FALSE,
                         header = FALSE), nd = 4)


###################################################
### code chunk number 16: multilevelShort.Rtex:322-323
###################################################
options(prompt = "  ", continue = "  ")


###################################################
### code chunk number 17: multilevelShort.Rtex:325-336
###################################################
model <- '

  level: 1

    y1 ~ x1

  level: 2

    y1 ~ w1

'


###################################################
### code chunk number 18: multilevelShort.Rtex:338-341
###################################################
fit <- sem(model, 
           data = Demo.twolevel, 
           cluster = "cluster")


###################################################
### code chunk number 19: multilevelShort.Rtex:343-344 (eval = FALSE)
###################################################
## summary(fit, nd = 4)


###################################################
### code chunk number 20: multilevelShort.Rtex:346-347
###################################################
options(prompt = "> ", continue = "  ")


###################################################
### code chunk number 21: multilevelShort.Rtex:354-355
###################################################
summary(fit, nd = 4)


###################################################
### code chunk number 22: multilevelShort.Rtex:398-399
###################################################
options(prompt = "  ", continue = "  ")


###################################################
### code chunk number 23: multilevelShort.Rtex:401-411
###################################################
model <- '

    level: 1

        fw =~ y1 + y2 + y3 + y4

    level: 2

        fb =~ y1 + y2 + y3 + y4
'


###################################################
### code chunk number 24: multilevelShort.Rtex:413-416
###################################################
fit <- sem(model, 
           data = Demo.twolevel, 
           cluster = "cluster")


###################################################
### code chunk number 25: multilevelShort.Rtex:418-419
###################################################
options(prompt = "> ", continue = "  ")


###################################################
### code chunk number 26: multilevelShort.Rtex:426-427
###################################################
summary(fit)


###################################################
### code chunk number 27: multilevelShort.Rtex:432-436
###################################################
fitMeasures(fit)
lavInspect(fit, "h1")
lavInspect(fit, "implied")
lavInspect(fit, "icc")


###################################################
### code chunk number 28: multilevelShort.Rtex:489-490
###################################################
options(prompt = "  ", continue = "  ")


###################################################
### code chunk number 29: multilevelShort.Rtex:492-504
###################################################
model <- '

    level: 1

        fw =~ y1 + y2 + y3 + y4
        fw ~ x1 + x2

    level: 2

        fb =~ y1 + y2 + y3 + y4
        fb ~ w1
'


###################################################
### code chunk number 30: multilevelShort.Rtex:506-509
###################################################
fit <- sem(model,
           data = Demo.twolevel,
           cluster = "cluster")


###################################################
### code chunk number 31: multilevelShort.Rtex:511-512
###################################################
options(prompt = "> ", continue = "  ")


###################################################
### code chunk number 32: multilevelShort.Rtex:653-660
###################################################
Galo <- read.table("Galo.dat")
names(Galo) <- c("school", "sex", "galo", "advice", "feduc", "meduc",
                 "focc", "denom")
Galo[Galo == 999] <- NA
Galo$denom1 <- ifelse(Galo$denom == 1, 1, 0)
Galo$denom2 <- ifelse(Galo$denom == 2, 1, 0)
summary(Galo)


###################################################
### code chunk number 33: multilevelShort.Rtex:663-664
###################################################
table(table(Galo$school))


###################################################
### code chunk number 34: multilevelShort.Rtex:670-692
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
        galo   ~ ba*bses + denom1 + denom2

    # defined parameters
    wi := wa * wb
    bi := ba * bb
'
fit <- sem(model, data = Galo, cluster = "school", std.lv = TRUE)
# summary(fit)


###################################################
### code chunk number 35: multilevelShort.Rtex:795-817
###################################################
library(lavaan)
head(Demo.growth[,c("t1","t2","t3","t4")], n = 4)
model.slope <- '
    int   =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
    slope =~ 0*t1 + 1*t2 + 2*t3 + 3*t4

    # intercepts (fixed effects)
    int   ~ 1
    slope ~ 1

    # random intercept, random slope
    int   ~~ int
    slope ~~ slope
    int   ~~ slope
    # force same variance for all (compound symmetry)
    t1 ~~ v1*t1
    t2 ~~ v1*t2
    t3 ~~ v1*t3
    t4 ~~ v1*t4
'
fit.slope <- lavaan(model.slope, data = Demo.growth)
summary(fit.slope, header = FALSE, nd = 4)


###################################################
### code chunk number 36: multilevelShort.Rtex:822-828
###################################################
# wide to long
id    <- rep(1:400,  each = 4)
score <- lav_matrix_vecr(Demo.growth[,1:4])
time  <- rep(0:3, times = 400)
growth.long <- data.frame(id = id, score = score, time = time)
head(growth.long)


###################################################
### code chunk number 37: multilevelShort.Rtex:830-834
###################################################
library(lme4)
fit.lmer <- lmer(score ~ 1 + time + (1 + time | id), data = growth.long,
                 REML = FALSE)
summary(fit.lmer, correlation = FALSE)


###################################################
### code chunk number 38: multilevelShort.Rtex:840-847
###################################################
# create balanced (n = 3) version of Demo.twolevel
cluster.idx <- Demo.twolevel$cluster
idx1 <- match(unique(cluster.idx), cluster.idx)
idx2 <- idx1 + 1
idx3 <- idx1 + 2
idx <- sort(c(idx1, idx2, idx3))
Demo.twolevel3 <- Demo.twolevel[idx ,c(1,2,3,4, 12)]


###################################################
### code chunk number 39: multilevelShort.Rtex:885-886
###################################################
options(prompt = "  ", continue = "  ")


###################################################
### code chunk number 40: multilevelShort.Rtex:888-898
###################################################
model <- '

    level: 1

        fw =~ y1 + y2 + y3 + y4

    level: 2

        fb =~ y1 + y2 + y3 + y4
'


###################################################
### code chunk number 41: multilevelShort.Rtex:900-903
###################################################
fit <- sem(model,
           data = Demo.twolevel3,
           cluster = "cluster")


###################################################
### code chunk number 42: multilevelShort.Rtex:905-906
###################################################
options(prompt = "> ", continue = "  ")


###################################################
### code chunk number 43: multilevelShort.Rtex:914-915
###################################################
summary(fit)


###################################################
### code chunk number 44: multilevelShort.Rtex:920-930
###################################################
nvar <- 4
cluster.size <- 3
nclusters <- 200
wideData <- matrix(lav_matrix_vecr(Demo.twolevel3[,1:nvar]),
                   nrow = nclusters,
                   ncol = cluster.size*nvar, byrow = TRUE)
wideData <- as.data.frame(wideData)
names(wideData) <- paste(rep(c("y1","y2","y3","y4"), cluster.size),
                         rep(1:cluster.size, each = nvar), sep = ".")
head(wideData)


###################################################
### code chunk number 45: multilevelShort.Rtex:936-980
###################################################
model.wide <- '
    # WITHIN #

    # within factors, common loadings, common (zero) means, common variance
    fw1 =~ 1*y1.1 + lw2*y2.1 + lw3*y3.1 + lw4*y4.1
    fw2 =~ 1*y1.2 + lw2*y2.2 + lw3*y3.2 + lw4*y4.2
    fw3 =~ 1*y1.3 + lw2*y2.3 + lw3*y3.3 + lw4*y4.3
    fw1 ~~ fvw*fw1
    fw2 ~~ fvw*fw2
    fw3 ~~ fvw*fw3

    # uncorrelated fw1, fw2, fw3
    fw1 ~~ 0*fw2 + 0*fw3; fw2 ~~ 0*fw3

    # within intercepts (fixed to zero)
    y1.1 + y2.1 + y3.1 + y4.1 ~ 0*1
    y1.2 + y2.2 + y3.2 + y4.2 ~ 0*1
    y1.3 + y2.3 + y3.3 + y4.3 ~ 0*1

    # common residual variances
    y1.1 ~~ rw1*y1.1; y1.2 ~~ rw1*y1.2; y1.3 ~~ rw1*y1.3
    y2.1 ~~ rw2*y2.1; y2.2 ~~ rw2*y2.2; y2.3 ~~ rw2*y2.3
    y3.1 ~~ rw3*y3.1; y3.2 ~~ rw3*y3.2; y3.3 ~~ rw3*y3.3
    y4.1 ~~ rw4*y4.1; y4.2 ~~ rw4*y4.2; y4.3 ~~ rw4*y4.3

    # BETWEEN #
    # between version of y1,y2,y3,y4
    by1 =~ 1*y1.1 + 1*y1.2 + 1*y1.3
    by2 =~ 1*y2.1 + 1*y2.2 + 1*y2.3
    by3 =~ 1*y3.1 + 1*y3.2 + 1*y3.3
    by4 =~ 1*y4.1 + 1*y4.2 + 1*y4.3

    # between intercepts
    by1 + by2 + by3 + by4 ~ 1

    # between factor
    fb =~ by1 + by2 + by3 + by4

    # not correlated with the within lvs
    fb ~~ 0*fw1 + 0*fw2 + 0*fw3

'
fit.wide <- sem(model.wide, data = wideData, information = "observed")
summary(fit.wide)


###################################################
### code chunk number 46: multilevelShort.Rtex:1011-1017
###################################################
model <- ' # no levels!
    fw1 =~ y1 + y2 + y3
    fw2 =~ y4 + y5 + y6
'
fit.robust <- sem(model, data = Demo.twolevel, cluster = "cluster")
summary(fit.robust, header = FALSE)


