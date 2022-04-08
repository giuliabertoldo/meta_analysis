# DOING META-ANALYSIS WITH R: A HANDS-ON GUIDE
# CHAPTER 4: POOLING EFFECT SIZES
# healthwellbeing.rda DATA

# Load packages
library(tidyverse)
library(meta)

# Import data
load(file='doing_meta_analysis_with_R/data/healthwellbeing.rda')
df1 <- HealthWellbeing

# Glimpse data
glimpse(df1)

# REM, REML estimator of tau^2
m.cor <- metacor(cor = cor,
                 n = n,
                 studlab = author,
                 data= df1,
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = 'REML',
                 hakn = TRUE,
                 title = "Health and Wellbeing")
m.cor
