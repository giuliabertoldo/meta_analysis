# Load packages
library(metafor)


# (1) Create dataframe
study <- c('Goldham', 'Graham', 'Madison', 'Manning', 'Moyer')
t_died <- c(8, 12, 8, 80, 20)
t_total <- c(100, 100, 100, 1000, 250)
c_died <- c(14, 16, 12, 200, 24)
c_total <- c(100, 100, 100, 1000, 250)

df1 <- data.frame(study, t_died, t_total, c_died, c_total)
df1

# (1a) Inverse-variance weighted meta-analysis on the log odds ratio (FEM)
## Calculate log-odds ratio and sampling variance
df2 <- escalc(ai=df1[,2], ci=df1[,4], n1i=df1[,3], n2i=df1[,5],
              measure = 'OR',
              data=df1,
              append=TRUE)
df2

## FEM
## Weighted estimation (with inverse-variance weights) is used by default
FEM <- rma(yi=df2$yi, vi=df2$vi, data=df2, method="FE")
summary(FEM)

# (1b) What would you conclude about the mean effect size?
## The mean log-odds is about -0.82 meaning that the log-odds of
## dying in treatment condition are lower than in the control ***

# (1c) Is there evidence for study heterogeneity?
## The Q statistics is not significant at 5% alpha level
## so we cannot reject the null hypothesis of homogeneity
## However, Q-statistics and stat. power

# (1d) Remove Manning study
## My hypothesis: the standard error of the estimate would be greater
## than the one we have now (0.1153)
df3 <-df2[-4,]
df3
FEM3 <- rma(yi=df3$yi, vi=df3$vi, data=df3, method="FE")
summary(FEM3)
## Indeed standard error is larger and the effect is not significant
## at 5% alpha level




# MY PROGRAMMING EXERCISES
# (2a) Inverse-variance weighted meta-analysis on the log odds ratio (FEM)
## Build function that returns a vector with the log odds
log_odds_ratio = function(v) {
  num <- v[1]*(v[4]-v[3])
  den <- v[3]*(v[2]-v[1])
  or <- (num)/(den)
  log_or <- log(or)
}
## Apply to all dataset
df1_log_or <- apply(df1[,2:5], 1, log_odds_ratio)

## Create new column in dataframe with log-odds-ratio
df1$log_or <- df1_log_or
df1



