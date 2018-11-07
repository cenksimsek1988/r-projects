library(foreign)
library(readxl)


library(dplyr)
library(dynlm)

library(timeSeries)

dat = read_excel(file.choose())
reg1=lm(inf ~ d_unem, data = dat)
summary(reg1)





# canadadaki şablondan fgaydalanabilirsin


# inf değil d(inf)'i regresyona sok.
# d_unem değil, unem ya da g_unemi kullan
# dönemleri bölerken, 2008 krizi ve sonrasını da ayır
# ozan hoca g_unem diye işszilikte gap kolonu yapmıştım
# onu beğenmemiş, doğrudan işszilik oranını kullansaydın demiş
# o yüzden d_unem değil g_unem değil direkt unemi kullan








# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.89429    0.13829  28.161  < 2e-16 ***
#   d_unem       0.07987    0.01001   7.979 8.27e-15 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.285 on 565 degrees of freedom
# Multiple R-squared:  0.1013,	Adjusted R-squared:  0.09967 
# F-statistic: 63.66 on 1 and 565 DF,  p-value: 8.27e-15
# anlamlı binde birde


#önceki senelerdeki harcamalar ömür uzatmış mı
dat_ts=ts(dat,start=1988,freq=1)

reg2 = dynlm(inf ~ d_unem + L(d_unem, c(1,2,3,4,5,6,7,8,9,10)), data = dat_ts)
summary(reg2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                    3.9473937  0.1407048  28.054   <2e-16 ***
#   d_unem                                         0.0987670  0.0366122   2.698   0.0072 ** 
#   L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))1  -0.0032575  0.0515268  -0.063   0.9496    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))2  -0.0036078  0.0514370  -0.070   0.9441    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))3   0.0032841  0.0513727   0.064   0.9491    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))4  -0.0078459  0.0513961  -0.153   0.8787    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))5  -0.0055330  0.0514919  -0.107   0.9145    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))6   0.0047084  0.0513624   0.092   0.9270    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))7  -0.0164615  0.0508968  -0.323   0.7465    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))8   0.0102442  0.0507666   0.202   0.8402    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))9  -0.0001797  0.0507758  -0.004   0.9972    
# L(d_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))10  0.0046324  0.0356471   0.130   0.8967    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.305 on 545 degrees of freedom
# Multiple R-squared:  0.1143,	Adjusted R-squared:  0.09647 
# F-statistic: 6.397 on 11 and 545 DF,  p-value: 5.111e-10
# çok laglı anlamsız
# yani gecikmeli etkiyi bulamadık (işsizlikte değişimin enflasyona gecikmeli etkisi)
# az lagla deneyelim
reg3 = dynlm(inf ~ d_unem + L(d_unem, c(1,2)), data = dat_ts)
summary(reg3)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          3.9176192  0.1381264  28.363  < 2e-16 ***
#   d_unem               0.1036714  0.0342780   3.024  0.00261 ** 
#   L(d_unem, c(1, 2))1  0.0008825  0.0499966   0.018  0.98592    
# L(d_unem, c(1, 2))2 -0.0233630  0.0339834  -0.687  0.49206    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.274 on 561 degrees of freedom
# Multiple R-squared:  0.1102,	Adjusted R-squared:  0.1054 
# F-statistic: 23.15 on 3 and 561 DF,  p-value: 3.835e-14
# 

# çok değişkenli
reg4 = dynlm(data = dat_ts, inf ~ d_unem +  real_exc + crisis)
summary(reg4)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.022089   0.588155  -1.738   0.0828 .  
# d_unem      -0.002079   0.005258  -0.395   0.6927    
# real_exc     8.637522   1.465939   5.892 6.57e-09 ***
#   crisis       7.167918   0.185386  38.665  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.603 on 563 degrees of freedom
# Multiple R-squared:  0.7867,	Adjusted R-squared:  0.7855 
# F-statistic:   692 on 3 and 563 DF,  p-value: < 2.2e-16


# sağlık harcamalarıyla doktor vs oranlarını regresyona sokuyorum,
reg5 = dynlm(data = dat_ts, inf ~ d_unem + crisis)
summary(reg5)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 2.4142783  0.0783328  30.821   <2e-16 ***
  d_unem      0.0009862  0.0053866   0.183    0.855    
crisis      7.4810139  0.1828396  40.916   <2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.65 on 564 degrees of freedom
Multiple R-squared:  0.7735,	Adjusted R-squared:  0.7727 
F-statistic: 963.1 on 2 and 564 DF,  p-value: < 2.2e-16


reg6=lm(inf ~ observation_date + d_unem, data = dat)
summary(reg6)

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)       8.129e+00  2.067e-01  39.335  < 2e-16 ***
  observation_date -5.422e-09  2.324e-10 -23.326  < 2e-16 ***
  d_unem            5.018e-02  7.261e-03   6.911 1.31e-11 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 2.345 on 564 degrees of freedom
Multiple R-squared:  0.5426,	Adjusted R-squared:  0.5409 
F-statistic: 334.5 on 2 and 564 DF,  p-value: < 2.2e-16


