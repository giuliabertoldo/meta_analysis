# DOING META-ANALYSIS WITH R: A HANDS-ON GUIDE
# CHAPTER 2: DISCOVERING R

# Load packages
library(tidyverse)

# Load data
load(file='doing_meta_analysis_with_R/data/suicideprevention.rda')
# Rename data
df1 <- SuicidePrevention

# Data manipulation

## Glimpse at data
glimpse(df1)

## Convert character to numeric
df1$n.e <- as.numeric(df1$n.e)
df1$mean.e <- as.numeric(df1$mean.e)
df1$sd.e <- as.numeric(df1$sd.e)
df1$n.c <- as.numeric(df1$n.c)
df1$mean.c <- as.numeric(df1$mean.c)
df1$sd.c <- as.numeric(df1$sd.c)
df1$n.c <- as.numeric(df1$n.c)

## Convert characters to factors
df1$age_group <- as.factor(df1$age_group)
df1$control <- as.factor(df1$control)

## Check labels of factor age_group
levels(df1$age_group)
## Check number of levels  of factor age_group
nlevels(df1$age_group)

## Change name of factor levels
new_levels_factor1 <- c('gen', 'older')
levels(df1$age_group) <- new_labels_factor1
levels(df1$age_group)

## pub_binary is new variable: if study was published after 2009
df1$pub_binary <- as.logical(df1$pubyear  >= 2010)
class(df1$pub_binary)
df1$pub_binary
