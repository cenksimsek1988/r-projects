library(foreign)
library(readxl)


library(dynlm)

library(timeSeries)

dat = read_excel(file.choose())

# inf_2 is quarterly inflation rate from one year ago
reg1=lm(inf_2 ~ un, data = dat)
summary(reg1)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.13633    0.19755  20.938  < 2e-16 ***
#   un          -0.27911    0.03236  -8.625 1.33e-15 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.502 on 218 degrees of freedom
# Multiple R-squared:  0.2544,	Adjusted R-squared:  0.251 
# F-statistic:  74.4 on 1 and 218 DF,  p-value: 1.332e-15
# anlamlı, negatif (makro teori ile uyumlu)
# in conventinal old form of phillips curve
# we can detect really a negative correlation between unemployment and inflation
# even without any lagged impact of unemployment
# but our data is not seasonally adjusted, lets deseason inflation
# and unemployment data, first

un_ts = ts(dat$un, start = c(1963,1), frequency = 4)
inf_1_ts = ts(dat$inf_1, start = c(1963,1), frequency = 4)
inf_2_ts = ts(dat$inf_2, start = c(1963,1), frequency = 4)

# we used additive method since they already are percentual growth rates
# does not show any explorential growth over time
comp_un = decompose(un_ts, "additive")
comp_inf1 = decompose(inf_1_ts, "additive")
comp_inf2 = decompose(inf_2_ts, "additive")

plot(comp_un)
plot(comp_inf2)

# here we are seasonally adjusting our time series
un = un_ts - comp_un$seasonal
inf1 = inf_1_ts - comp_inf1$seasonal
inf4 = inf_2_ts - comp_inf2$seasonal

# adding also period columns
per = dat$period


dat_tmp = cbind(inf1,inf4,un,per)
dat_ts = ts(dat_tmp, start = c(1963,1), frequency = 4)
# now we have adjusted time data

# lets try without season effect if we still observe same relation
reg2 = dynlm(data=dat_ts, inf4 ~ un)
summary(reg2)
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.13646    0.19755  20.939  < 2e-16 ***
#   un          -0.27913    0.03236  -8.626 1.32e-15 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.502 on 218 degrees of freedom
# Multiple R-squared:  0.2545,	Adjusted R-squared:  0.2511 
# F-statistic: 74.41 on 1 and 218 DF,  p-value: 1.325e-15
# yes, quite same numbers occured
# our coefficent in longh term is -0,28
# Now, we are managing to explain about 15 percent of inflation's changments
# by unemployment rates


# lets check if we see some significant impact of different periods
dat_p1 = subset(dat_ts, per == 'period_1')
dat_p1 = ts(dat_p1, start = c(1963,1), frequency = 4)
dat_p2 = subset(dat_ts, per == 'period_2')
dat_p2 = ts(dat_p2, start = c(1973,2), frequency = 4)

dat_p3 = subset(dat_ts, per == 'period_3')
dat_p3 = ts(dat_p3, start = c(1984,1), frequency = 4)

dat_p4 = subset(dat_ts, per == 'period_2')
dat_p4 = ts(dat_p4, start = c(2007,4), frequency = 4)


reg3 = dynlm(data=dat_p1, inf4 ~ un)
summary(reg3)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.2609     0.5063   8.416 2.66e-10 ***
#   un           -1.3660     0.7042  -1.940   0.0597 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.358 on 39 degrees of freedom
# Multiple R-squared:  0.08799,	Adjusted R-squared:  0.06461 
# F-statistic: 3.763 on 1 and 39 DF,  p-value: 0.05966
# R square is too small. there is a highly significant intercept
# which we can comment as sucj-h period, the inflation ratew were arround some
# definite levels and not changing too much
# we have negative correlation between unemployment and inflation
# but loosing its significancy below %5 percent level


#lets check for other periods
reg4 = dynlm(data=dat_p2, inf4 ~ un)
summary(reg4)
# (Intercept)   5.8387     0.4568  12.780  6.9e-16 ***
#   un           -0.3269     0.1259  -2.595    0.013 *

reg5 = dynlm(data=dat_p3, inf4 ~ un)
summary(reg5)
# (Intercept)  4.30419    0.63728   6.754 1.23e-09 ***
#   un          -0.28447    0.07881  -3.610 0.000496 ***
# Multiple R-squared:  0.1229,	Adjusted R-squared:  0.1135 
# best correlation, but still small R square
# our coefficent is about -0.28 which is not so high
# but so meaningful

reg6 = dynlm(data=dat_p4, inf4 ~ un)
summary(reg6)
# (Intercept)   5.8387     0.4568  12.780  6.9e-16 ***
#   un           -0.3269     0.1259  -2.595    0.013 *  

# As we see, in each period we have different intercept,
# which points to differentunemployment and inflation equilibrium
# and we see (as negative) higher coefficents compare to the regression
# where we used all data as whole
# so we can think in Germany, inflation rates were even more elastical to
# the employment levels

# what about  lagged impact of unemployment? Since teory tells a process
# beyond the phillips model, where the wages would play an intermediate role


# we choose to focus on third period, where we found more correlation
# between infation and unemployment
reg7 = dynlm(data=dat_p3, inf4 ~ L(un, c(10)))
summary(reg7)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   7.27439    0.53095   13.70  < 2e-16 ***
#   L(un, c(10)) -0.68272    0.06803  -10.04 5.57e-16 ***
#   Multiple R-squared:  0.5482,	Adjusted R-squared:  0.5428 
# after many trial we noticed that all lags, alone were significant
# and all of the lags between fifth and fifteenth lags were giving both
# higher t values and R squares
# but they are not meaningfull together
# why?
# lets try with quarter to quarter inflation changment data


reg8 = dynlm(data=dat_p3, d(inf1) ~ L(un, c(7,8,9,10)))
summary(reg8)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             -0.06766    0.27900  -0.242 0.809013    
# L(un, c(7, 8, 9, 10))7  -0.66370    0.33381  -1.988 0.050203 .  
# L(un, c(7, 8, 9, 10))8   2.00171    0.61524   3.254 0.001671 ** 
#   L(un, c(7, 8, 9, 10))9  -2.33666    0.61853  -3.778 0.000303 ***
#   L(un, c(7, 8, 9, 10))10  1.01043    0.34535   2.926 0.004469 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4964 on 80 degrees of freedom
# Multiple R-squared:  0.1681,	Adjusted R-squared:  0.1265 
# F-statistic: 4.042 on 4 and 80 DF,  p-value: 0.0049
# this was one of the best regression which returned all significant t values for
# unemployment levels lagged effect
# but they have all different signs
# are they compoundly meaningfull?

library(car)
linearHypothesis(reg8, c("L(un, c(7, 8, 9, 10))7+ L(un, c(7, 8, 9, 10))8 
+L(un, c(7, 8, 9, 10))9+ L(un, c(7, 8, 9, 10))10 "))
# Model 1: restricted model
# Model 2: d(inf1) ~ L(un, c(7, 8, 9, 10))
# 
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     81 19.740                           
# 2     80 19.714  1  0.026715 0.1084 0.7428
# unfortunetly they are not meaningfull together
# means no need to sum up their coefficents to derive a total
# coefficent to consider as lagged impact of unemployment changes on inflation changes



#lets try whole adjusted data set with lagged impact of unemployment
reg9 = dynlm(data=dat_ts, d(inf1) ~ L(un, c(8,9,10)))
summary(reg9)
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)          -0.01022    0.06674  -0.153  0.87848   
# L(un, c(8, 9, 10))8   0.41978    0.20629   2.035  0.04314 * 
#   L(un, c(8, 9, 10))9  -0.97828    0.37721  -2.593  0.01018 * 
#   L(un, c(8, 9, 10))10  0.56024    0.20506   2.732  0.00684 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4908 on 206 degrees of freedom
# Multiple R-squared:  0.03516,	Adjusted R-squared:  0.02111 
# F-statistic: 2.503 on 3 and 206 DF,  p-value: 0.0604
# we tried many versions and
# this regression seems the best but
# too small R square and different signs

linearHypothesis(reg9, c("L(un, c(8, 9, 10))8 
+L(un, c(8, 9, 10))9+ L(un, c(8, 9, 10))10 "))
# Model 1: restricted model
# Model 2: d(inf1) ~ L(un, c(8, 9, 10))
# 
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    207 49.634                           
# 2    206 49.628  1 0.0063628 0.0264 0.8711
# once again, they are not meaningful together

# we showed that, expectation augmented model was not helpful but
# classical one worked fine on Germany's quarterly data set
# but we didn't care about stationarity
# lets examine it

library(fpp)
adf.test(un_ts)
# Augmented Dickey-Fuller Test
# 
# data:  un_ts
# Dickey-Fuller = -0.82653, Lag order = 6, p-value = 0.958
# alternative hypothesis: stationary
# p value is not below 5% so;
# un_ts time series is not derived from a unique root
# we dont have a stationary time series which means
# our regressions will not be biased
# after checking if other two time series are also not stationary

adf.test(inf_1_ts)
# Dickey-Fuller = -3.5789, Lag order = 6, p-value = 0.0363
# alternative hypothesis: stationary
# oops, we have a stationary data set here
# we have auto-correlation till 6th lagged term of inflation from
# one quarter ago
# so we need to model our regressions with inf1
# with their at least six lags!

adf.test(inf_2_ts)
# Dickey-Fuller = -4.0562, Lag order = 6, p-value = 0.01
# alternative hypothesis: stationary
# same is true for time series of quarterly inflation from
# one year ago

# so we try our chance to check out another classical phillips curve model
# which is auto correlated
reg10 = dynlm(data = dat_p2, inf4 ~ un + L(inf4, c(1,2,3,4,5,6)))
summary(reg10)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.73154    0.28816   2.539  0.01675 *  
#   un                            -0.05668    0.04735  -1.197  0.24100    
# L(inf4, c(1, 2, 3, 4, 5, 6))1  1.37541    0.16671   8.250 4.27e-09 ***
#   L(inf4, c(1, 2, 3, 4, 5, 6))2 -0.50613    0.25918  -1.953  0.06055 .  
# L(inf4, c(1, 2, 3, 4, 5, 6))3  0.27070    0.25121   1.078  0.29010    
# L(inf4, c(1, 2, 3, 4, 5, 6))4 -0.74987    0.25572  -2.932  0.00651 ** 
#   L(inf4, c(1, 2, 3, 4, 5, 6))5  0.91586    0.26806   3.417  0.00190 ** 
#   L(inf4, c(1, 2, 3, 4, 5, 6))6 -0.43073    0.15618  -2.758  0.00996 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.349 on 29 degrees of freedom
# Multiple R-squared:  0.9271,	Adjusted R-squared:  0.9094 
# F-statistic: 52.65 on 7 and 29 DF,  p-value: 8.743e-15
# so hi R square!
# coefficent of unemployment is still negative
# it is now smaller
# and lost its significancy

linearHypothesis(reg10, c("
L(inf4, c(1, 2, 3, 4, 5, 6))1 + L(inf4, c(1, 2, 3, 4, 5, 6))2 + L(inf4, c(1, 2, 3, 4, 5, 6))3 + 
L(inf4, c(1, 2, 3, 4, 5, 6))4 + L(inf4, c(1, 2, 3, 4, 5, 6))5 + L(inf4, c(1, 2, 3, 4, 5, 6))6
                         "))
# Model 1: restricted model
# Model 2: inf4 ~ un + L(inf4, c(1, 2, 3, 4, 5, 6))
# 
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     30 33.232                                  
# 2     29  3.532  1      29.7 243.85 1.185e-15 ***
# yes the lags are meaningful!
# so, we notice that we over estimated our coefficent in reg4

# but we know that in periods before, the inflation rates were correlated by such periods
# unemployment rate from reg4
# so how to check the direction of the relation?


# as a last regression lets use our best-fit period
# p3, which is between two crises
reg11 = dynlm(data = dat_p3, inf4 ~ un + L(un, c(1,2,3,4,5,6)) + L(inf4, c(1,2,3,4,5,6)))
summary(reg11)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    0.60617    0.33792   1.794 0.076873 .  
# un                            -0.32339    0.27163  -1.191 0.237580    
# L(un, c(1, 2, 3, 4, 5, 6))1    0.62216    0.50788   1.225 0.224398    
# L(un, c(1, 2, 3, 4, 5, 6))2   -0.01152    0.51633  -0.022 0.982259    
# L(un, c(1, 2, 3, 4, 5, 6))3   -0.64488    0.54930  -1.174 0.244103    
# L(un, c(1, 2, 3, 4, 5, 6))4    0.44167    0.56901   0.776 0.440070    
# L(un, c(1, 2, 3, 4, 5, 6))5   -0.39731    0.58540  -0.679 0.499423    
# L(un, c(1, 2, 3, 4, 5, 6))6    0.26210    0.32143   0.815 0.417416    
# L(inf4, c(1, 2, 3, 4, 5, 6))1  1.13455    0.10921  10.389 3.57e-16 ***
#   L(inf4, c(1, 2, 3, 4, 5, 6))2 -0.06415    0.15298  -0.419 0.676186    
# L(inf4, c(1, 2, 3, 4, 5, 6))3 -0.19030    0.15057  -1.264 0.210188    
# L(inf4, c(1, 2, 3, 4, 5, 6))4 -0.24960    0.14648  -1.704 0.092527 .  
# L(inf4, c(1, 2, 3, 4, 5, 6))5  0.59076    0.14793   3.993 0.000151 ***
#   L(inf4, c(1, 2, 3, 4, 5, 6))6 -0.31633    0.10355  -3.055 0.003118 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4254 on 75 degrees of freedom
# Multiple R-squared:  0.9219,	Adjusted R-squared:  0.9084 
# F-statistic: 68.09 on 13 and 75 DF,  p-value: < 2.2e-16
# But all the significancies of unemployment levels are lost
# while the the lags of inflation itself, is explaining
# much more better our current inflation