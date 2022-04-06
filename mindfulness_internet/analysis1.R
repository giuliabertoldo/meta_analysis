library(readxl)
library(metafor)

# Import data
df1 <- read_excel('mindfulness_internet/data_stress.xlsx')
str(df1)

# REM
model1<- rma(yi = g, vi = se^2, data = df1, method='REML')
summary(model1)

funnel(model1)

trimfill(model1)

# Three-level model
multilevel3 <- rma.mv(yi = g,
                      V = se^2,
                      random = list(~1 | studyID/effectsizeID),
                      data = df1)
summary(multilevel3)
