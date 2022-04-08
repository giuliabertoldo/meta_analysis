# DOING META-ANALYSIS WITH R: A HANDS-ON GUIDE
# CHAPTER 4: POOLING EFFECT SIZES
# ThirdWave.xlsx DATA

# Load packages
library(tidyverse)
library(meta)
library(readxl)

# Load data
ThirdWave <- read_excel('doing_meta_analysis_with_r/data/ThirdWave.xlsx')

# Glimpse
glimpse(ThirdWave )

# Fit REM, with REML estimator and Knapp-Hartung adjustment
m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = author,
                 data = ThirdWave,
                 sm = 'SMD',
                 com.fixed = FALSE,
                 com.random = TRUE,
                 method.tau = 'REML',
                 hakn = TRUE,
                 title = 'Third Wave Psychotherapies')
m.gen
