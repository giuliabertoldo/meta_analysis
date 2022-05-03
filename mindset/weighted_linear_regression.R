getAnywhere(regtest.default)
X <- cbind(1, sei = sqrt(df3$vi))

tmp <- lm(df3$yi ~ X - 1)
summary(tmp)

fit <- lm(df3$yi ~ X - 1, weights = 1/df3$vi)
summary(fit)

fit2 <- lm(df3$yi ~ sqrt(df3$vi), weights = 1/df3$vi)
summary(fit2)

# Multilevel analysis
# Line 318 from RealData-Lehtonen-FinalTables.R
eggers_multi_ado <- rma.mv(yi ~ 1 + sqrt(vi),
                          V = vi,
                          random = ~ 1 | study_id/es_id,
                          data = df3_develop,
                          subset = (development_stage=="Adolescents"))
summary(eggers_multi_ado)

eggers_multi_adu <- rma.mv(yi ~ 1 + sqrt(vi),
                           V = vi,
                           random = ~ 1 | study_id/es_id,
                           data = df3_develop,
                           subset = (development_stage=="Adults"))
summary(eggers_multi_adu)

eggers_multi_chi <- rma.mv(yi ~ 1 + sqrt(vi),
                           V = vi,
                           random = ~ 1 | study_id/es_id,
                           data = df3_develop,
                           subset = (development_stage=="Children"))
summary(eggers_multi_chi)
