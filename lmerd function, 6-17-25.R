
lmer.d <- function (model = model, contrast = c(-0.5, 0.5), formatted = F) {
  require(lme4)
  require(dplyr) # current version uses dplyr piping
  lmerd.output = list()
  ba = max(contrast) - min(contrast) # For transformations needed to make calculations insensitive to contrast weights
  c2 = (ba/2)^2 # For transforming random slope variance
  contr.est = lme4::fixef(model) * ba # Extracting fixed effects, transformed for contrast weights
  contr.SE = summary(model)$coefficients %>% as.data.frame() %>% pull(`Std. Error`) * ba
  contr.CI.low = contr.est - (1.96 * contr.SE)
  contr.CI.high = contr.est + (1.96 * contr.SE)
  unweighted.variances = list()
  variance.list = list()
  for (j in c(1:length(lme4::VarCorr(model)))) {
    variance.list[[j]] = lme4::VarCorr(model)[[j]] %>% as.matrix() %>% diag() #to extract variances and not covariances
    unweighted.variances[[j]] = variance.list[[j]]
    variance.list[[j]][names(variance.list[[j]]) %>% str_detect("Intercept") == F] = 
      variance.list[[j]][names(variance.list[[j]]) %>% str_detect("Intercept") == F] * c2
  }
  variance.list[[(length(lme4::VarCorr(model)) + 1)]] = sigma(model)^2 # to add residual variance
  unweighted.variances[[(length(lme4::VarCorr(model)) + 1)]] = sigma(model)^2 # to add residual variance
  lmerd.output$sd.pooled = variance.list %>% unlist %>% sum() %>% sqrt()
  lmerd.output$d.est = contr.est / lmerd.output$sd.pooled
  lmerd.output$d.CI.low = contr.CI.low / lmerd.output$sd.pooled
  lmerd.output$d.CI.high = contr.CI.high / lmerd.output$sd.pooled
  lmerd.output$weighted.variances = variance.list %>% unlist()
  lmerd.output$unweighted.variances = unweighted.variances %>% unlist()
  if (formatted == T) {
    formatted.lmerd.output = data.frame(Terms = names(lmerd.output$d.est), 
                                        d_95ci = paste0(format(round(lmerd.output$d.est, 2), nsmall = 2, trim = T), 
                                                        " [", format(round(lmerd.output$d.CI.low, 2), nsmall = 2, trim = T),
                                                        ", ", format(round(lmerd.output$d.CI.high, 2), nsmall = 2, trim = T), "]"))
    formatted.lmerd.output
  } else {lmerd.output}
}