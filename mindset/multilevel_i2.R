# Formulae 4.29 Chen 2015 + Paper
# Higgins and Thompson (2002) preferred to define the typical within-study
# sampling variance ... using the Q statistic:


# Inputs:
# v: variance (e.g. df$vi)
# k: number of studies (e.g. length(df$vi))
# rma_obj: meta-analysis object (e.g. multilevel1)


multilevel_i2 <- function(v, k, rma_obj)
{
  k <- length(v)
  list.inverse.variances <- 1 /(v)
  sum.inverse.variances <- sum(list.inverse.variances)
  numerator <- (k - 1) * sum.inverse.variances

  squared.sum.inverse.variances <- (sum.inverse.variances)^2

  list.inverse.variances.square <- 1 / (v^2)
  sum.inverse.variances.square <- sum(list.inverse.variances.square)
  denominator <- squared.sum.inverse.variances - sum.inverse.variances.square

  estimated.sampling.variance <- numerator / denominator

  I2_1 <- (estimated.sampling.variance) / (rma_obj$sigma2[1] + rma_obj$sigma2[2] + estimated.sampling.variance)

  I2_2 <- (rma_obj$sigma2[1]) / (rma_obj$sigma2[1] + rma_obj$sigma2[2] + estimated.sampling.variance)

  I2_3 <- (rma_obj$sigma2[2]) / (rma_obj$sigma2[1] + rma_obj$sigma2[2] + estimated.sampling.variance)

  I2_1 <- round((amountvariancelevel1 <- I2_1 * 100),2)
  I2_2 <- round((amountvariancelevel2 <- I2_2 * 100),2)
  I2_3 <- round((amountvariancelevel3 <- I2_3 * 100),2)

  col1 <- rbind(I2_1, I2_2, I2_3)
  col2 <- rbind('Level 1', 'Level 2', 'Level 3')
  I2_partition <- data.frame(col1, col2)
  names(I2_partition) <- c('I2', 'Level')
  print(I2_partition)

  # Barplot
  ggplot(I2_partition, aes(x="", y=I2, fill=Level))+
    geom_bar(width = 1, stat = "identity") +
    theme_light()

}

