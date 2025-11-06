#### File information
# File name: lmerd_function.R
# Author: Koji J. Takahashi
# Updated: 11-6-2025

### General Comments
# This function takes an LME model with multiple random factors (e.g., participants and stimuli)
# Prints a Cohen's d estimate, lower and upper CI bounds, and the equivalent of a pooled SD
# The "SD" equivalent to a standard deviation pooled across levels of random and fixed factors
# See Westfall, Kenny, and Judd, 2014 for equations and explanation of the estimate
# Results are not interpretable if random slopes for continuous variables are included

lmer_d <- function (model = model, contrast = c(-0.5, 0.5), formatted = F) {
  require(lme4)
  require(dplyr) # current version uses dplyr piping
  lmerd_output = list()
  ba = max(contrast) - min(contrast) # For transformations needed to make calculations insensitive to contrast weights
  c2 = (ba/2)^2 # For transforming random slope variance
  contr_est = lme4::fixef(model) * ba # Extracting fixed effects, transformed for contrast weights
  contr_SE = summary(model)$coefficients %>% as.data.frame() %>% pull(`Std. Error`) * ba
  contr_CI_low = contr_est - (1.96 * contr_SE)
  contr_CI_high = contr_est + (1.96 * contr_SE)
  unweighted_variances = list()
  variance_list = list()
  RE_names = list()
  for (j in c(1:length(lme4::VarCorr(model)))) {
    variance_list[[j]] = lme4::VarCorr(model)[[j]] %>% as.data.frame() %>% 
      as.matrix() %>% diag() #to extract variances and not covariances
    unweighted_variances[[j]] = variance_list[[j]]
    variance_list[[j]][names(variance_list[[j]]) %>% str_detect("Intercept") == F] =
      variance_list[[j]][names(variance_list[[j]]) %>% str_detect("Intercept") == F] * c2
    RE_names[[j]] = paste0(names(variance_list[[j]]), " | ",
      rep(names(lme4::VarCorr(model))[j], times = length(variance_list[[j]])))
  }
  variance_list[[(length(lme4::VarCorr(model)) + 1)]] = sigma(model)^2 # to add residual variance
  unweighted_variances[[(length(lme4::VarCorr(model)) + 1)]] = sigma(model)^2 # to add residual variance
  lmerd_output$SD_op = variance_list %>% unlist %>% sum() %>% sqrt()
  lmerd_output$d_op = contr_est / lmerd_output$SD_op
  lmerd_output$d_CI_low = contr_CI_low / lmerd_output$SD_op
  lmerd_output$d_CI_high = contr_CI_high / lmerd_output$SD_op
  lmerd_output$weighted_variances = variance_list %>% unlist()
  lmerd_output$unweighted_variances = unweighted_variances %>% unlist()
  lmerd_output$var_components = lmerd_output$weighted_variances/sum(lmerd_output$weighted_variances)
  names(lmerd_output$var_components) = c(unlist(RE_names), "Residual")
  if (formatted == T) {
    formatted_lmerd_output = data.frame(Terms = names(lmerd_output$d_op),
                                        d_95ci = paste0(format(round(lmerd_output$d_op, 2), nsmall = 2, trim = T),
                                                        " [", format(round(lmerd_output$d_CI_low, 2), nsmall = 2, trim = T),
                                                        ", ", format(round(lmerd_output$d_CI_high, 2), nsmall = 2, trim = T), "]"))
    formatted_lmerd_output
  } else {lmerd_output}
}
