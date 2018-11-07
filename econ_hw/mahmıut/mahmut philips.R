library(foreign)
library(readxl)


library(dplyr)
library(dynlm)

library(timeSeries)

dat = read_excel(file.choose())
reg1=lm(cpi ~ unem, data = dat)
summary(reg1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  37.2338     3.0022   12.40   <2e-16 ***
#   unem          5.2779     0.3989   13.23   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 17.17 on 195 degrees of freedom
# Multiple R-squared:  0.4731,	Adjusted R-squared:  0.4704 
# F-statistic: 175.1 on 1 and 195 DF,  p-value: < 2.2e-16


#önceki senelerdeki harcamalar ömür uzatmış mı
dat_ts=ts(dat,start=1970,freq=1)

reg2 = dynlm(cpi ~ unem + L(unem, c(1,2,3,4,5,6,7,8,9,10)), data = dat_ts)
summary(reg2)

# Coefficients:
#   Estimate
# (Intercept)                                 40.53594
# unem                                        -2.50012
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))1   1.57580
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))2   2.32191
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))3  -1.03615
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))4  -0.47429
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))5   0.47830
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))6   0.66324
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))7  -0.06818
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))8  -2.75149
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))9  -5.17107
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))10 12.07949
# Std. Error
# (Intercept)                                    3.07438
# unem                                           5.00600
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))1     8.75408
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))2     8.80710
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))3     8.81832
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))4     8.82109
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))5     8.79794
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))6     8.81778
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))7     8.81267
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))8     8.80180
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))9     8.74153
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))10    4.94563
# t value Pr(>|t|)
# (Intercept)                                  13.185   <2e-16
# unem                                         -0.499   0.6181
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))1    0.180   0.8574
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))2    0.264   0.7924
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))3   -0.117   0.9066
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))4   -0.054   0.9572
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))5    0.054   0.9567
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))6    0.075   0.9401
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))7   -0.008   0.9938
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))8   -0.313   0.7550
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))9   -0.592   0.5549
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))10   2.442   0.0156
# 
# (Intercept)                                 ***
#   unem                                           
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))1     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))2     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))3     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))4     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))5     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))6     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))7     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))8     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))9     
# L(unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))10 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 14.97 on 175 degrees of freedom
# Multiple R-squared:  0.567,	Adjusted R-squared:  0.5398 
# F-statistic: 20.84 on 11 and 175 DF,  p-value: < 2.2e-16

# az lagla deneyelim
reg3 = dynlm(cpi ~ unem + L(unem, c(1,2)), data = dat_ts)
summary(reg3)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         38.845      2.977  13.048   <2e-16 ***
#   unem                -3.102      5.335  -0.581   0.5616    
# L(unem, c(1, 2))1   -4.876      9.367  -0.521   0.6033    
# L(unem, c(1, 2))2   13.114      5.284   2.482   0.0139 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 16.5 on 191 degrees of freedom
# Multiple R-squared:  0.5063,	Adjusted R-squared:  0.4986 
# F-statistic:  65.3 on 3 and 191 DF,  p-value: < 2.2e-16


# çok değişkenli
reg4 = dynlm(data = dat_ts, cpi ~ unem +  bond)
summary(reg4)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  87.0681     2.4287   35.85   <2e-16 ***
#   unem          3.1338     0.2093   14.97   <2e-16 ***
#   bond         -6.1181     0.2399  -25.50   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 8.25 on 194 degrees of freedom
# Multiple R-squared:  0.879,	Adjusted R-squared:  0.8777 
# F-statistic: 704.4 on 2 and 194 DF,  p-value: < 2.2e-16


# sağlık harcamalarıyla doktor vs oranlarını regresyona sokuyorum,
reg5 = dynlm(data = dat_ts, cpi ~ bond)
summary(reg5)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 116.8842     2.0364    57.4   <2e-16 ***
#   bond         -7.5608     0.3217   -23.5   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 12.08 on 195 degrees of freedom
# Multiple R-squared:  0.7391,	Adjusted R-squared:  0.7377 
# F-statistic: 552.3 on 1 and 195 DF,  p-value: < 2.2e-16


