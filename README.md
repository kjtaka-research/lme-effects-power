# lme-effects-power
R functions to estimate standardized effect sizes and conduct power analyses for LME models

# Functions
Below are notes on the various functions in this repository

## lmer.d
This is a custom function for estimating operative Cohen's d and relevant variance estimates for linear mixed-effect models with multiple random factors. The code is based on on formulas from Westfall, Kenny, & Judd, 2014. Refer to that citation for details about the method of calculating Cohen's d for these types of models. 
The function below can accommodate between-subjects fixed effects in the model in addition to within-subject fixed effects. The code will run even if there is a continuous predictor in the model, but these should be removed beforehand (it would be equivalent to residualizing a DV before estimating the pooled standard deviation for effect size estimates). 
The default is to assume that each fixed effect in the model uses a simple contrast code or any contrast where the difference between contrast weights is 1 (e.g., 0.5 and -0.5). Must set the contrast if variables are effect coded (1, -1) or has some other contrast.
The output will give a Cohen's d estimate for each coefficient as well as the equivalent of the pooled standard deviation used for a Cohen's d estimate. There is also an option to format the effect sizes to have the d estimate and the 95% confidence interval around it. 
