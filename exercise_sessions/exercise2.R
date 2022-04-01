# Load packages
library(readxl)
library(metafor)
# Import data
df1 <- read_excel('lectures/exercise_session/3_exercises_r/Exercise2.xlsx')

# (2a) Perform a FEM
FEM1 <- rma (yi=g, vi=SE^2, data=df1, method='FE')
summary(FEM1)

## The overall effect is 0.122 (sig. at 5%) meaning
## that males have larger conformity than females

## Is there evidence for heterogeneity?
## Yes, Q statistics, we reject H0


# (2b) Make a forest plot to explore a possible moderating effect of
# the percentage of authors that are male

x <- cumul(FEM1, order=order(df1$Male))
forest(x)
## forest(FEM1)
## As the number of %male authors increases, the effect size increases


# (2c) Is there evidence (alpha=1%) for a moderating effect of
# the percentage of authors that are male?
FEMR <- rma(yi=g, vi=SE^2, data=df1, mods=Male, method='FE')
summary(FEMR)
# Intercept is the expected difference in conformity for 0% male authors
# For a unit increase in %male author there is a 0.0076 increase in conformity

## (2d) Is it OK to use FEM?
## No, also based on previous Q statistics



