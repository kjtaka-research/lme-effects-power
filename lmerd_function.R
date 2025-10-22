#### File information
# File name: lmerd_function_6-17-25.R
# Author: Koji J. Takahashi
# Updated: 6-17-2025

### General Comments
# This function takes an LME model with multiple random factors (e.g., participants and stimuli)
# Prints a Cohen's d estimate, lower and upper CI bounds, and the equivalent of a pooled SD
# The "SD" equivalent to a standard deviation pooled across levels of random and fixed factors
# See Westfall, Kenny, and Judd, 2014 for equations and explanation of the estimate

lmer_d = function (model = model, contrast = c(-0.5, 0.5), formatted = F) {
  require(lme4)
  require(dplyr)
  require(stringr)
  library(lme4)
  library(dplyr)
  lmerd.output = list()
  ba = max(contrast) - min(contrast) # For transformations needed to make calculations insensitive to contrast weights
  c2 = (ba/2)^2 # For transforming random slope variance
  contr.est = lme4::fixef(model) * ba # Extracting fixed effects, transformed for contrast weights
  contr.SE = dplyr::pull(as.data.frame(summary(model)$coefficients), `Std. Error`) * ba
  contr.CI.low = contr.est - (1.96 * contr.SE)
  contr.CI.high = contr.est + (1.96 * contr.SE)
  unweighted.variances = list()
  variance.list = list()
  for (j in c(1:length(lme4::VarCorr(model)))) {
    variance.list[[j]] = diag(as.matrix(lme4::VarCorr(model)[[j]])) #to extract variances and not covariances
    unweighted.variances[[j]] = variance.list[[j]]
    variance.list[[j]][stringr::str_detect(names(variance.list[[j]]), "Intercept") == F] = 
      variance.list[[j]][stringr::str_detect(names(variance.list[[j]]), "Intercept") == F] * c2 #adjusting random slopes for contrast weights
  }
  variance.list[[(length(lme4::VarCorr(model)) + 1)]] = sigma(model)^2 # to add residual variance
  unweighted.variances[[(length(lme4::VarCorr(model)) + 1)]] = sigma(model)^2 # to add residual variance
  lmerd.output$sd_op = sqrt(sum(unlist(variance.list)))
  lmerd.output$d_op = contr.est / lmerd.output$sd_op
  lmerd.output$d.CI.low = contr.CI.low / lmerd.output$sd_op
  lmerd.output$d.CI.high = contr.CI.high / lmerd.output$sd_op
  lmerd.output$weighted.variances =  unlist(variance.list)
  lmerd.output$unweighted.variances = unlist(unweighted.variances)
  if (formatted == T) {
    formatted.lmerd.output = data.frame(Terms = names(lmerd.output$d_op), 
                                        d_95ci = paste0(format(round(lmerd.output$d_op, 2), nsmall = 2, trim = T), 
                                                        " [", format(round(lmerd.output$d.CI.low, 2), nsmall = 2, trim = T),
                                                        ", ", format(round(lmerd.output$d.CI.high, 2), nsmall = 2, trim = T), "]"))
    formatted.lmerd.output
  } else {lmerd.output}
}
