
# Load packages -----------------------------------------------------------
library(tidyverse)
library(meta)
library(metafor)
library(readxl)
library(gridExtra) # for arranging qqplots
library(multcomp) # for multiple comparisons


# Local function(s) -------------------------------------------------------

# * Function to calculate I^2 in multilevel model -------------------------
# Formulae:
# Sampling variance: Chen(2015), formula (4.29), Higgins and Thompson statistics
# Multilevel I^2: Cheung(2014), formulae (10), (11)

# Function Inputs:
# tau2_level2: multilevel within-study study variance
# tau2_level3: multilevel between-study variance
# vect_var: vector of variances (e.g. df$vi)

multilevel_i2 <- function (tau2_level2, tau2_level3, vect_var) {
  k <- length(vect_var)
  numerator <- (k-1)*sum(1/vect_var)
  denominator <- (sum(1/vect_var)^2) - sum(1/(vect_var)^2)
  v <- numerator / denominator

  i2_denominator <- tau2_level2 + tau2_level3 + v

  i2_level1 <- v / i2_denominator

  i2_level2 <- tau2_level2 / i2_denominator

  i2_level3 <- tau2_level3 / i2_denominator

  I2_1 <- round((amountvariancelevel1 <- i2_level1 * 100),2)
  I2_2 <- round((amountvariancelevel2 <- i2_level2 * 100),2)
  I2_3 <- round((amountvariancelevel3 <- i2_level3 * 100),2)

  col1 <- rbind(I2_1, I2_2, I2_3)
  col2 <- rbind('Level 1', 'Level 2', 'Level 3')
  I2_partition <- data.frame(col1, col2)
  names(I2_partition) <- c('I2', 'Level')
  print(I2_partition)

  # Barplot
  ggplot(I2_partition, aes(x="", y=I2, fill=Level))+
    geom_bar(width = 1, stat = "identity") +
    labs(title = "Distribution of variance across levels") +
    theme_light()

}

# Import data -------------------------------------------------------------
df1 <- read_excel('mindset/data/mindset.xlsx', sheet = 'Meta-analysis 1')

# Data cleaning -----------------------------------------------------------
# Glimpse data
glimpse(df1)

# Rename columns
df2 <- rename(df1,
              document_id = 'Document #',
              study_id = 'Study #',
              sample_id = 'Sample #',
              sample_country = 'Sample Country',
              es_id = 'ES #',
              reference = 'Reference',
              n = N,
              adjusted_n = 'Adjusted N',
              student_description = 'Student Description',
              school_level = 'School Level',
              development_stage = 'Development Stage',
              risk_status = 'Risk status',
              ses = SES,
              ms_measure = 'MS Measure',
              ms_measure_description = 'MS Measure Description',
              mindset_type = 'Mindset Type',
              achievement_measure_description = 'Achievement Measure Description',
              academic_achievement_measure_type = 'Academic Achievement Measure Type',
              lab_based = 'Lab-based',
              published = 'Published',
              es_type = 'ES type',
              calculation = 'Calculation',
              variance = 'Variance',
              adjusted_variance = 'Adjusted Variance',
              is_significant = 'Significant?',
              growth_m = 'Growth M',
              growth_sd = 'Growth SD',
              other_m = 'Other M',
              other_sd = 'Other SD',
              cohen_d = "Cohen's d",
              calculated_r = 'Calculated r',
              notes = Notes)

# Check that variable types is correct
glimpse(df2)

# Change school_level from character to factor
df2$school_level <- as.factor(df2$school_level)
levels(df2$school_level)

# Change development_stage from character to factor
df2$development_stage <- as.factor(df2$development_stage)
levels(df2$development_stage)
# Convert all "Wide range" level to "Wide Range"
df2$development_stage <- recode_factor(df2$development_stage,
                                       'Wide range' = 'Wide Range')
levels(df2$development_stage)

# Change risk_status from character to factor
df2$risk_status <- as.factor(df2$risk_status)
levels(df2$risk_status)
## Note: The category '.' applies to 4 rows
## These are studies from which it was not possible to determine the risk status
## df2 %>%
##   filter(risk_status == '.')

# Change ses from character to factor
df2$ses <- as.factor(df2$ses)
levels(df2$ses)

# Change mindset_type from character to factor
df2$mindset_type <- as.factor(df2$mindset_type)
levels(df2$mindset_type)

# Change academic_achievement_measure_type from character to factor
df2$academic_achievement_measure_type <- as.factor(df2$academic_achievement_measure_type)
levels(df2$academic_achievement_measure_type)

# Change lab_based from character to factor
df2$lab_based <- as.factor(df2$lab_based)
levels(df2$lab_based)

# Change published from character to factor
df2$published <- as.factor(df2$published)
levels(df2$published)

# Change es_type from character to factor
df2$es_type <- as.factor(df2$es_type)
levels(df2$es_type)

# Change is_significant from character to factor
df2$is_significant <- as.factor(df2$is_significant)
levels(df2$is_significant)

# Create dataframe for metafor:
# Calculate r-to-z transformed correlations and corresponding sampling variances
df3 <- escalc(measure="ZCOR", ri=r, ni=n, data=df2)


# Exploratory Data Analysis -----------------------------------------------
# How many effect sizes?
length(df3$study_id)

# How many studies?
length(unique(df3$study_id))

# How many samples?
length(unique(df3$sample_id))


# * Count effect sizes per study ------------------------------------------
# How many effect sizes per study?
df_groub_by_study <- df3 %>%
  group_by(study_id) %>%
  summarize(n_es = n())

summarize(df_groub_by_study,
          min_n_es = min(n_es),
          max_n_es = max(n_es),
          median_n_es = median(n_es))

ggplot(df_groub_by_study, aes(x=n_es)) +
  geom_bar() +
  labs(title = 'Number of effect sizes per study',
       x = 'Number of effect sizes',
       y = 'Count of studies') +
  scale_x_continuous(breaks = seq(0:27)) +
  scale_y_continuous(breaks = seq(from = 0, to =80, by=5)) +
  theme_light()

## Percentage of studies reporting 1 effect size
df_percentage_es <- df_groub_by_study %>%
  group_by(n_es) %>%
  summarize(total = n()) %>%
  mutate(percentage = round(total/sum(total), 3))

# Check that sums to 100
sum(df_percentage_es$percentage)

## Percentage of studies with more than 2 effect sizes
df_percentage_es %>%
  filter(n_es > 2 ) %>%
  mutate(sum = sum(percentage))

# Sample sizes
## Minimum sample size
min(df3$n)
## Maximum sample size
max(df3$n)
## Mean sample size
mean(df3$n)
## Median sample size
median(df3$n)

# Which study has the largest sample size?
df3 %>%
  filter(n == max(df3$n))
# Claro et al. (2016)


# * Fisher's Z vs Correlation Coefficient ---------------------------------
# Explore graphically Fisher's Z
density_z <- ggplot(df3, aes(x=yi)) +
  geom_histogram(aes(y=..density..), colour = 'black', fill = "white") +
  geom_density(alpha=.2, fill="#FF6666") +
  labs(x = "Fisher's Z Transformed Correlation Coefficient",
       y = "Density") +
  theme_classic()

# Fisher's Z
qqplot_z <- ggplot(df3, aes(sample=yi)) +
  stat_qq(distribution = stats::qnorm) +
  stat_qq_line(distribution = stats::qnorm,) +
  labs(x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_classic()

# Original r
# qqplot_r <- ggplot(df3, aes(sample=r))+
#   stat_qq() +
#  stat_qq_line() +
#  theme_classic()

grid.arrange(density_z, qqplot_z, ncol=2)


# Meta-analysis 1: Random-effects model (REM) -----------------------------
meta1 <- rma (yi = yi,
              vi = vi,
              measure = 'ZCOR',
              data = df3,
              slab = es_id,
              method = 'REML')
summary(meta1, digits = 3)

# Confidence interval around tau^2
## Interval for tau^2 is obtained iteratively either via the Q-profile method
## or via the generalized Q-statistic method
## The square root of the interval bounds is also returned for easier interpretation.
## Confidence intervals for I2 and H2 are also provided (Higgins & Thompson, 2002).
## Since I2 and H2 are just monotonic transformations of tau2 the confidence intervals
## for I2 and H2 are also exact.
confint.rma.uni(meta1, digits = 2)

# Transform from z to r
predict(meta1, digits=2, transf=transf.ztor)


# Forest plot -------------------------------------------------------------
pdf(file='forestplot_metafor.pdf', width = 8, height = 30)
forest(meta1,
       header = TRUE,
       transf = transf.ztor,
       showweights = TRUE,
       order = 'obs',
       efac = 0.2)

### add text with Q-value, dfs, p-value, and I^2 statistic
# https://www.metafor-project.org/doku.php/plots:forest_plot
text(-4.6, -3, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                                             .(formatC(meta1$QE, digits=2, format="f")), ", df = ", .(meta1$k - meta1$p),
                                             ", p = ", .(formatC(meta1$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(meta1$I2, digits=1, format="f")), "%)")))
dev.off()


# Caterpillar plot --------------------------------------------------------
# Open jpeg file
jpeg("caterpillar.jpeg", quality = 100)

# Source code:
# http://www.metafor-project.org/doku.php/plots:caterpillar_plot
### create plot
forest(df3$yi, df3$vi,
       xlim=c(-2.5,3.5),        ### adjust horizontal plot region limits
       order="obs",             ### order by size of yi
       slab=NA, annotate=FALSE, ### remove study labels and annotations
       efac=0,                  ### remove vertical bars at end of CIs
       pch=19,                  ### changing point symbol to filled circle
       col="gray40",            ### change color of points/CIs
       psize=2,                 ### increase point size
       cex.lab=1, cex.axis=1,   ### increase size of x-axis title/labels
       lty=c("solid","blank"),  ### remove horizontal line at top of plot
       transf = transf.ztor)

### draw points one more time to make them easier to see
points(sort(df3$yi), length(df3$vi):1, pch=19, cex=0.5)

### add summary polygon at bottom and text
addpoly(meta1, mlab="", cex=1, col = 'red')
text(-2, -2, "RE Model", pos=4, offset=0, cex=1)

# Close jpeg file
dev.off()

# Find minimum and maximum value of point estimates
min(df2$r)
max(df2$r)


# Subgroup analysis -------------------------------------------------------
# Prepare dataset
levels(df3$development_stage)
# Subset to exclude "Wide Range"
df3_develop <- filter(df3,
                      (development_stage == "Adolescents") |
                        (development_stage == "Adults") |
                        (development_stage == "Children"))
# Adjust labels
df3_develop$development_stage <- droplevels(df3_develop$development_stage)
levels(df3_develop$development_stage)

# Open a jpeg file
jpeg("subgroup.jpeg", width = 800, height = 400, quality = 100)
# Inspect visually the relationship between Fisher's z and the categories
# x = developmental_stage y = Fisher's z
ggplot(data=df3_develop, mapping = aes(x=development_stage, y = yi)) +
  geom_dotplot(binaxis='y',
               stackdir='center',
               stackratio=1.5,
               dotsize=0.5,
               fill="#3399ff") +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="#ff5050")+
  labs(x = "Developmental Stage",
       y = "Fisher's Z Transformed Correlation Coefficient")
theme_classic()
# Close jpeg file
dev.off()

# How many studies and how many effect sizes not included?
check <- df3 %>%
  filter(development_stage == "Wide Range")
# Number of studies
length(unique(check$study_id))
# Number of effect sizes
length(unique(check$es_id))


# * Common tau^2 ----------------------------------------------------------
## Same between-study variance within each subgroup (Adolescents, Adults, Children)
meta1_develop1 <- rma (yi = yi,
                       vi = vi,
                       measure = 'ZCOR',
                       data = df3_develop,
                       mods = ~ development_stage-1)
summary(meta1_develop1, digits = 3)

# Transform the estimates of the differences from z to r
estimate <- round(transf.ztor(meta1_develop1$b),2)
stand_er <- round(transf.ztor(meta1_develop1$se),2)
cilb <- round(transf.ztor(meta1_develop1$ci.lb),2)
ciup <- round(transf.ztor(meta1_develop1$ci.ub),2)

(groups <- data.frame(cbind(estimate, stand_er,cilb, ciup)))

# Multiple comparisons
# https://wviechtb.github.io/metafor/reference/rma.uni.html
multcomp_develop1 <- glht(meta1_develop1,
                          linfct=contrMat(c("Adolescents"=1,"Adults"=1,"Children"=1),
                                          type="Tukey"),test=adjusted("bonferroni"))

summary(multcomp_develop1)
# Note that the estimate of the differences in the output are Fisher z
# So I need to transform them
estimate <- round(transf.ztor(c(-0.13025, 0.05042, 0.18066)),2)
stand_er <- round(transf.ztor(c(0.02061, 0.02798, 0.02840)),2)
(groups <- data.frame(cbind(estimate, stand_er)))

# Confidence intervals for tau^2
confint.rma.uni(meta1_develop1, digits = 3)

# * Different tau^2 per subgroup ------------------------------------------
## Different between-study variance within each subgroup (Adolescents, Adults, Children)
# https://wviechtb.github.io/metafor/reference/rma.uni.html

# We have to fit 3 models: ado, adu, chi
meta1_develop2_ado <- rma (yi = yi,
                           vi = vi,
                           measure = 'ZCOR',
                           data = df3_develop,
                           mods = ~ development_stage-1,
                           subset = (development_stage=="Adolescents"))
summary(meta1_develop2_ado, digits = 3)
confint.rma.uni(meta1_develop2_ado, digits = 3)

meta1_develop2_adu <- rma (yi = yi,
                           vi = vi,
                           measure = 'ZCOR',
                           data = df3_develop,
                           mods = ~ development_stage-1,
                           subset = (development_stage=="Adults"))
summary(meta1_develop2_adu,  digits = 3)
confint.rma.uni(meta1_develop2_adu, digits = 3)

meta1_develop2_chi <- rma (yi = yi,
                           vi = vi,
                           measure = 'ZCOR',
                           data = df3_develop,
                           mods = ~ development_stage-1,
                           subset = (development_stage=="Children"))
summary(meta1_develop2_chi, digits =3)
confint.rma.uni(meta1_develop2_chi, digits = 3)

# Compare tau^2
tau2_comp <- data.frame(rbind(meta1_develop1$tau2,
                              meta1_develop2_ado$tau2,
                              meta1_develop2_adu$tau2,
                              meta1_develop2_chi$tau2))
names(tau2_comp) <- 'tau^2'
rownames(tau2_comp) <- c('Common', 'Adolescents', 'Adults', 'Children')
round(tau2_comp, 3)

# Compare I^2
i2_comp <- data.frame(rbind(meta1_develop1$I2,
                            meta1_develop2_ado$I2,
                            meta1_develop2_adu$I2,
                            meta1_develop2_chi$I2))
names(i2_comp) <- 'I^2'
rownames(i2_comp) <- c('Common', 'Adolescents', 'Adults', 'Children')
round(i2_comp,2)


# Publication bias  -------------------------------------------------------

# * Funnel plot overall ---------------------------------------------------
# Reference: https://wviechtb.github.io/metafor/reference/funnel.html
funnel(meta1,
       level=c(90, 95, 99),
       shade=c("white", "gray55", "gray75"),
       legend=TRUE)

# * Egger's regression test overall ---------------------------------------
# Reference: https://wviechtb.github.io/metafor/reference/regtest.html
meta1_regtst <- regtest(meta1,
                        model = 'lm',
                        predictor = 'sei')
meta1_regtst

meta1_regtst$fit

# * Funnel plot adolescents -----------------------------------------------
 funnel(meta1_develop2_ado,
       level=c(90, 95, 99),
       shade=c("white", "gray55", "gray75"),
       legend=TRUE)

# * Egger's regression test adolescents -----------------------------------
meta1_regtst_ado <- regtest(meta1_develop2_ado,
                            model = 'lm',
                            predictor = 'sei')
meta1_regtst_ado

meta1_regtst_ado$fit

# * Funnel plot adults ----------------------------------------------------
funnel(meta1_develop2_adu,
       level=c(90, 95, 99),
       shade=c("white", "gray55", "gray75"),
       legend=TRUE)

# * Egger's regression test adults  ---------------------------------------
meta1_regtst_adu <- regtest(meta1_develop2_adu,
                            model = 'lm',
                            predictor = 'sei')
meta1_regtst_adu

meta1_regtst_adu$fit

# * Funnel plot children --------------------------------------------------
funnel(meta1_develop2_chi,
       level=c(90, 95, 99),
       shade=c("white", "gray55", "gray75"),
       legend=TRUE)

# * Egger's regression test children --------------------------------------
meta1_regtst_chi <- regtest(meta1_develop2_chi,
                            model = 'lm',
                            predictor = 'sei')
meta1_regtst_chi

meta1_regtst_chi$fit


# Three-level meta-analysis -----------------------------------------------
meta1_multi1 <- rma.mv(yi = yi,
                       V = vi,
                       random = list(~1 | study_id/es_id),
                       data = df3,
                       slab = es_id)
summary(meta1_multi1, digits =3)

round(transf.ztor(meta1_multi1$b), 2)
round(transf.ztor(meta1_multi1$ci.lb), 2)
round(transf.ztor(meta1_multi1$ci.ub), 2)

# Transform from z to r
predict(meta1_multi1, digits=2, transf=transf.ztor)

# Confidence intervals
confint.rma.mv(meta1_multi1)

# * I^2: Distribution of variance at different levels ---------------------
# See function description under 'Load packages' at the top
multilevel_i2(tau2_level2 = meta1_multi1$sigma2[2],
              tau2_level3 = meta1_multi1$sigma2[1],
              vect_var = df3$vi)

# * Subgroup analysis -----------------------------------------------------
# Fit 3-level model with developmental_stage as moderator
meta1_multi1_develop1 <- rma.mv(yi = yi,
                                V = vi,
                                random = list(~1 | study_id/es_id),
                                data = df3_develop,
                                slab = es_id,
                                mods = ~ development_stage -1)

summary(meta1_multi1_develop1, digits =3)

# Transform the estimates of the differences from z to r
estimate_m <- round(transf.ztor(meta1_multi1_develop1$b),2)
se_m <- round(transf.ztor(meta1_multi1_develop1$se),2)
cilb_m <- round(transf.ztor(meta1_multi1_develop1$ci.lb),2)
ciup_m <- round(transf.ztor(meta1_multi1_develop1$ci.ub),2)

(group_multi <- data.frame(cbind(estimate_m, se_m, cilb_m, ciup_m)))

# Multiple comparisons
multcomp_multi1_develop1 <- glht(meta1_multi1_develop1,
                                 linfct=contrMat(c("Adolescents"=1,"Adults"=1,"Children"=1),
                                                 type="Tukey"),test=adjusted("bonferroni"))

summary(multcomp_multi1_develop1)
# Note that the estimate of the differences in the output are Fisher z

# So I need to transform them
estimate <- round(transf.ztor(c(-0.14067, 0.04808, 0.18875)),2)
stand_er <- round(transf.ztor(c(0.02523, 0.03825, 0.03913)),2)
(groups <- data.frame(cbind(estimate, stand_er)))






