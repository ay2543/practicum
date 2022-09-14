# Clear all objects from environment
rm(list = ls())

# Load needed packages
library(tidyverse)
library(writexl)
library(survey)


# Load data
setwd("/Users/amyyeung/Dropbox/Practicum/nsduh")

load("./nsduh.Rdata")
nsduh_full <- nsduh

# Create yearly subset
nsduh_2015_full <- nsduh_full[nsduh_full$year == "2015",]
nsduh_2016_full <- nsduh_full[nsduh_full$year == "2016",]
nsduh_2017_full <- nsduh_full[nsduh_full$year == "2017",]
nsduh_2018_full <- nsduh_full[nsduh_full$year == "2018",]
nsduh_2019_full <- nsduh_full[nsduh_full$year == "2019",]
nsduh_2015_2019_full <- nsduh_full[nsduh_full$year <= "2019",]
nsduh_2020_full <- nsduh_full[nsduh_full$year == "2020",]


skimr::skim(nsduh_full)

# Create survey design object
design_obj <- svydesign(ids = ~verep, strata = ~vestr, weights = ~analwt_c, data = nsduh_full, nest = T)

# I am using an alternative package (srvyr) to create the design object. 
# With this package I can compute statistics using the survey design and tidyverse coding
library(srvyr)

# recode variables as factors. What does each variable mean?
nsduh_full <- nsduh_full %>%
  mutate(txyrrecvd2 = factor(txyrrecvd2, labels = c("No", "Yes")), 
         txyrndilal = factor(txyrndilal, labels = c("No", "Yes")))

# creating survey design 
nsduh_design <- nsduh_full %>%
  as_survey_design(strata = vestr, 
                   ids = verep, 
                   weights = analwt_c / 6, 
                   nest = T)

# check how both variables are basically the same thing. 81% of both variables are Yes and 93% of both variables
# are no. My suspicion is that they are highly collinear and that is why you get massive ORs. 
nsduh_design %>%
  group_by(txyrrecvd2, txyrndilal) %>%
  summarise(prop = survey_mean(proportion = T))


# use the design object to run the logistic regression
m1 <- svyglm(txyrrecvd2 ~ factor(txyrndilal), design = design_obj, family = "quasibinomial")

# getting model summary and ORs. OR is 58!
m1 %>%
  broom::tidy(exponentiate = T, conf.int = T)

#-------------------------------------------------------------------#

unadjusted <- glm(txyrrecvd2 ~ txyrndilal, data = nsduh_full, family = "binomial", weights = analwt_c)

summary(unadjusted)



reg_received_vs_need_2015 <- glm(txyrrecvd2 ~ factor(txyrndilal) +
                            factor(catage) + factor(income) + factor(newrace2) + factor(irsex),
                           data = nsduh_2015_full, family = "quasibinomial", weights = analwt_c)
summary(reg_received_vs_need_2015)
# Coefficients:
#                      Estimate   Std. Error  t value   Pr(>|t|)    
# (Intercept)         -3.394e+15  9.458e+12 -358.880  < 2e-16 ***
#   factor(txyrndilal)1  6.889e+14  8.078e+12   85.283  < 2e-16 ***
#   factor(catage)2      5.696e+14  9.425e+12   60.433  < 2e-16 ***
#   factor(catage)3      1.819e+15  9.206e+12  197.551  < 2e-16 ***
#   factor(catage)4      1.870e+15  7.683e+12  243.378  < 2e-16 ***
#   factor(income)2      5.373e+13  6.573e+12    8.174 3.05e-16 ***
#   factor(income)3     -1.087e+12  7.563e+12   -0.144   0.8857    
# factor(income)4      4.555e+13  6.560e+12    6.944 3.85e-12 ***
#   factor(newrace2)2   -1.927e+14  6.995e+12  -27.545  < 2e-16 ***
#   factor(newrace2)3    2.376e+14  3.013e+13    7.887 3.14e-15 ***
#   factor(newrace2)4    1.674e+14  4.116e+13    4.067 4.76e-05 ***
#   factor(newrace2)5    2.062e+13  9.740e+12    2.117   0.0343 *  
#   factor(newrace2)6    2.773e+14  1.658e+13   16.722  < 2e-16 ***
#   factor(newrace2)7    2.911e+13  6.184e+12    4.707 2.52e-06 ***
#   factor(irsex)2       8.338e+13  4.386e+12   19.013  < 2e-16 ***


reg_received_vs_need_2016 <- glm(txyrrecvd2 ~ factor(txyrndilal) +
                                   factor(catage) + factor(income) + factor(newrace2) + factor(irsex),
                                 data = nsduh_2016_full, family = "quasibinomial", weights = analwt_c)
summary(reg_received_vs_need_2016)


# Coefficients:
#                       Estimate  Std. Error  t value Pr(>|t|)    
# (Intercept)         -2.903e+15  9.849e+12 -294.747  < 2e-16 ***
#   factor(txyrndilal)1  5.346e+14  8.442e+12   63.331  < 2e-16 ***
#   factor(catage)2      6.886e+14  9.758e+12   70.567  < 2e-16 ***
#   factor(catage)3      1.487e+15  9.486e+12  156.697  < 2e-16 ***
#   factor(catage)4      1.419e+15  7.930e+12  178.900  < 2e-16 ***
#   factor(income)2     -5.339e+13  6.882e+12   -7.758 8.77e-15 ***
#   factor(income)3      2.338e+14  7.963e+12   29.358  < 2e-16 ***
#   factor(income)4      1.047e+14  6.825e+12   15.343  < 2e-16 ***
#   factor(newrace2)2   -9.396e+14  7.212e+12 -130.296  < 2e-16 ***
#   factor(newrace2)3    2.405e+14  2.986e+13    8.054 8.19e-16 ***
#   factor(newrace2)4   -3.992e+14  3.355e+13  -11.898  < 2e-16 ***
#   factor(newrace2)5   -1.732e+14  1.009e+13  -17.167  < 2e-16 ***
#   factor(newrace2)6    1.339e+14  1.694e+13    7.899 2.86e-15 ***
#   factor(newrace2)7   -8.167e+13  6.347e+12  -12.867  < 2e-16 ***
#   factor(irsex)2       4.843e+13  4.508e+12   10.743  < 2e-16 ***


reg_received_vs_need_2017 <- glm(txyrrecvd2 ~ factor(txyrndilal) +
                                   factor(catage) + factor(income) + factor(newrace2) + factor(irsex),
                                 data = nsduh_2017_full, family = "quasibinomial", weights = analwt_c)
summary(reg_received_vs_need_2017)
# did not converge

reg_received_vs_need_2018 <- glm(txyrrecvd2 ~ factor(txyrndilal) +
                                   factor(catage) + factor(income) + factor(newrace2) + factor(irsex),
                                 data = nsduh_2018_full, family = "quasibinomial", weights = analwt_c)
summary(reg_received_vs_need_2018)

# Coefficients:
#                       Estimate  Std. Error  t value   Pr(>|t|)    
# (Intercept)         -1.694e+15  9.913e+12 -170.914  < 2e-16 ***
#   factor(txyrndilal)1  2.074e+14  8.429e+12   24.601  < 2e-16 ***
#   factor(catage)2     -1.120e+15  9.777e+12 -114.558  < 2e-16 ***
#   factor(catage)3      1.341e+14  9.420e+12   14.235  < 2e-16 ***
#   factor(catage)4      1.445e+14  7.908e+12   18.267  < 2e-16 ***
#   factor(income)2      9.289e+13  7.000e+12   13.269  < 2e-16 ***
#   factor(income)3      2.010e+13  8.079e+12    2.488   0.0129 *  
#   factor(income)4      8.697e+13  6.866e+12   12.668  < 2e-16 ***
#   factor(newrace2)2   -4.608e+13  7.140e+12   -6.454  1.1e-10 ***
#   factor(newrace2)3    3.435e+13  2.912e+13    1.179   0.2382    
# factor(newrace2)4   -3.563e+14  3.641e+13   -9.786  < 2e-16 ***
#   factor(newrace2)5   -2.779e+14  9.753e+12  -28.497  < 2e-16 ***
#   factor(newrace2)6   -1.668e+14  1.631e+13  -10.224  < 2e-16 ***
#   factor(newrace2)7   -2.423e+14  6.215e+12  -38.985  < 2e-16 ***
#   factor(irsex)2       1.334e+14  4.471e+12   29.839  < 2e-16 ***


reg_received_vs_need_2019 <- glm(txyrrecvd2 ~ factor(txyrndilal) +
                                   factor(catage) + factor(income) + factor(newrace2) + factor(irsex),
                                 data = nsduh_2019_full, family = "quasibinomial", weights = analwt_c)
summary(reg_received_vs_need_2019)

# Coefficients:
#                       Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)         -2.636e+15  1.048e+13 -251.489  < 2e-16 ***
#   factor(txyrndilal)1  8.935e+14  8.752e+12  102.089  < 2e-16 ***
#   factor(catage)2      7.276e+14  1.026e+13   70.915  < 2e-16 ***
#   factor(catage)3      1.095e+15  9.865e+12  111.039  < 2e-16 ***
#   factor(catage)4      1.085e+15  8.297e+12  130.812  < 2e-16 ***
#   factor(income)2     -2.792e+14  7.522e+12  -37.117  < 2e-16 ***
#   factor(income)3     -1.860e+13  8.519e+12   -2.184  0.02898 *  
#   factor(income)4      1.154e+14  7.296e+12   15.818  < 2e-16 ***
#   factor(newrace2)2   -1.577e+14  7.450e+12  -21.175  < 2e-16 ***
#   factor(newrace2)3    8.399e+13  3.113e+13    2.698  0.00698 ** 
#   factor(newrace2)4    4.458e+14  3.742e+13   11.912  < 2e-16 ***
#   factor(newrace2)5   -1.698e+14  1.014e+13  -16.746  < 2e-16 ***
#   factor(newrace2)6    2.851e+14  1.697e+13   16.797  < 2e-16 ***
#   factor(newrace2)7    2.124e+13  6.464e+12    3.286  0.00102 ** 
#   factor(irsex)2      -7.528e+13  4.680e+12  -16.086  < 2e-16 ***

reg_received_vs_need_2015_2019 <- glm(txyrrecvd2 ~ factor(txyrndilal) +
                                   factor(catage) + factor(income) + factor(newrace2) + factor(irsex),
                                 data = nsduh_2015_2019_full, family = "quasibinomial", weights = analwt_c)
summary(reg_received_vs_need_2015_2019)

# Coefficients:
#                       Estimate  Std. Error t value   Pr(>|t|)    
# (Intercept)         -1.142e+15  4.461e+12 -256.10   <2e-16 ***
#   factor(txyrndilal)1  3.882e+14  3.801e+12  102.13   <2e-16 ***
#   factor(catage)2     -3.599e+14  4.412e+12  -81.57   <2e-16 ***
#   factor(catage)3      8.290e+13  4.272e+12   19.41   <2e-16 ***
#   factor(catage)4      1.116e+14  3.580e+12   31.18   <2e-16 ***
#   factor(income)2     -9.510e+13  3.144e+12  -30.25   <2e-16 ***
#   factor(income)3     -1.358e+15  3.612e+12 -376.14   <2e-16 ***
#   factor(income)4     -3.024e+14  3.094e+12  -97.73   <2e-16 ***
#   factor(newrace2)2   -4.254e+14  3.239e+12 -131.33   <2e-16 ***
#   factor(newrace2)3    3.122e+14  1.357e+13   23.01   <2e-16 ***
#   factor(newrace2)4    2.960e+14  1.646e+13   17.99   <2e-16 ***
#   factor(newrace2)5   -1.861e+14  4.464e+12  -41.68   <2e-16 ***
#   factor(newrace2)6    1.597e+14  7.538e+12   21.19   <2e-16 ***
#   factor(newrace2)7   -1.293e+14  2.834e+12  -45.62   <2e-16 ***
#   factor(irsex)2      -1.333e+14  2.030e+12  -65.69   <2e-16 ***


reg_received_vs_need_2020 <- glm(txyrrecvd2 ~ factor(txyrndilal) +
                                   factor(catage) + factor(income) + factor(newrace2) + factor(irsex),
                                 data = nsduh_2020_full, family = "quasibinomial", weights = analwt_c)
summary(reg_received_vs_need_2020)

# Coefficients:
#                       Estimate  Std. Error  t value   Pr(>|t|)    
# (Intercept)         -2.092e+15  1.364e+13 -153.409  < 2e-16 ***
#   factor(txyrndilal)1  1.303e+15  1.123e+13  116.034  < 2e-16 ***
#   factor(catage)2     -8.437e+14  1.338e+13  -63.038  < 2e-16 ***
#   factor(catage)3      1.596e+13  1.282e+13    1.245 0.213110    
# factor(catage)4      9.968e+13  1.079e+13    9.239  < 2e-16 ***
#   factor(income)2     -3.281e+14  9.554e+12  -34.345  < 2e-16 ***
#   factor(income)3     -3.762e+14  1.098e+13  -34.253  < 2e-16 ***
#   factor(income)4     -2.721e+14  9.231e+12  -29.481  < 2e-16 ***
#   factor(newrace2)2   -2.150e+14  9.671e+12  -22.234  < 2e-16 ***
#   factor(newrace2)3    3.253e+14  3.921e+13    8.297  < 2e-16 ***
#   factor(newrace2)4    4.468e+14  5.359e+13    8.339  < 2e-16 ***
#   factor(newrace2)5   -3.057e+14  1.296e+13  -23.591  < 2e-16 ***
#   factor(newrace2)6    5.607e+14  2.254e+13   24.878  < 2e-16 ***
#   factor(newrace2)7   -1.137e+14  8.362e+12  -13.592  < 2e-16 ***
#   factor(irsex)2       2.204e+13  6.056e+12    3.640 0.000274 ***



