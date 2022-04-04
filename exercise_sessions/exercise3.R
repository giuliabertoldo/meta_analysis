# Load packages
library(readxl)
library(metafor)

# Import data
df1 <- read_excel('lectures/exercise_session/3_exercises_r/Exercise3.xlsx')

# (3a) Perform a FEM and a REM for DG1 (panic attacks) and for DG4(depression)

## DG1 FEM
FEM1 <- rma(yi=DG1, vi=vdg1, data=df1, method='FE')
summary(FEM1)
REM1 <- rma(yi=DG1, vi=vdg1, data=df1, method='REML')
summary(REM1)

## DG4 FEM
FEM4 <- rma(yi=DG4, vi=vdg4, data=df1, method='FE')
summary(FEM4)
REM4 <- rma(yi=DG4, vi=vdg4, data=df1, method='REML')
summary(REM4)

# For which outcome variable the effect of the model on the standard error
# of the mean is largest? Why?
model <- c('FEM1', 'REM1', 'FEM4', 'REM4')
se <- c(FEM1$se, REM1$se, FEM4$se, REM4$se)

df2 <- data.frame(model, se)
df2

# For panic attacks, the standard error increases of about 0.04 (vs. 0.02 for depr.)
# This may be because there is more heterogeneity in studies for panic attacks
# In fact tau^2 (variance between population effect sizes) are:
model <- c('REM1', 'REM4')
tau <- c(REM1$tau2, REM4$tau2 )
df3 <- data.frame(model, tau)
df3


# (3b) Make a funnle plot for both outcome variables
# Use the trim and fill method.

## Panick attacks
funnel(REM1)
## There is asymmetry in the funnel plot
trimfill(REM1)

## Depression
funnel(REM4)



