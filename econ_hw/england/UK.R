# excel dosyasÄ±nÄ± iÃ§e aktarabilmek iÃ§in gereken paket
library(readxl)

# panel data regresyonu kurmak iÃ§in gereken paket
library(plm)


# datayÄ± R'a aktardÄ±m
dat = read_excel(file.choose())

# panel data oluÅŸturdum
pdat = pdata.frame(dat)

# ilk regresyon; eÄŸitim ve yÄ±l
reg = plm(re ~ unem, data = pdat, model = "fd")
summary(reg)
# Coefficients:
#   Estimate Std. Error t-value Pr(>|t|)
# unem  0.42182    1.63885  0.2574   0.7979
# 
# Total Sum of Squares:    6479
# Residual Sum of Squares: 6470.8
# R-Squared:      0.0051996
# Adj. R-Squared: 0.0051996
# F-statistic: Inf on 0 and 51 DF, p-value: NA

# 2. regresyon; cinsiyet dahil
reg2 = plm(re ~ exp + hdi + sex, data = pdat, model = "fd")
summary(reg2)
# Coefficients:
#   Estimate Std. Error t-value Pr(>|t|)
# exp  -0.50083    1.31195 -0.3817   0.7043
# hdi -40.62177  578.71452 -0.0702   0.9443
# 
# Total Sum of Squares:    6479
# Residual Sum of Squares: 6456.3
# R-Squared:      0.0037795
# Adj. R-Squared: -0.016145
# F-statistic: 0.175998 on 1 and 50 DF, p-value: 0.67663
# 3. regresyon; yaÅŸ
reg3 = plm(re ~ year + sex + age, data = pdat, model = "fd")
summary(reg3)
# Coefficients:
#   Estimate Std. Error t-value Pr(>|t|)
# year2011  0.29775    5.68389  0.0524   0.9584
# year2012 -4.75300    8.03824 -0.5913   0.5572
# year2013  2.83675    9.84479  0.2881   0.7745
# year2014  1.29260   10.16765  0.1271   0.8994
# year2015  2.41130   10.48058  0.2301   0.8190
# 
# Total Sum of Squares:    6479
# Residual Sum of Squares: 6073.6
# R-Squared:      0.062567
# Adj. R-Squared: -0.017214
# F-statistic: 0.784234 on 4 and 47 DF, p-value: 0.54119

# 4.regresyon; pooling yÃ¶ntemi ile eÄŸitim ve yaÅŸ birlikte
reg12 = lm(re ~ educ*age, data = dat)
summary(reg12)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          75.839      3.455  21.952  < 2e-16 ***
#   educL3               24.161      5.984   4.038 0.000151 ***
#   educL5               41.953      5.984   7.011 2.06e-09 ***
#   educL6               70.442      5.984  11.772  < 2e-16 ***
#   educL7T8             85.713      5.984  14.324  < 2e-16 ***
#   ageY55T64             2.771      4.886   0.567 0.572678    
# educL3:ageY55T64     -2.771      8.462  -0.327 0.744447    
# educL5:ageY55T64     11.577      8.462   1.368 0.176234    
# educL6:ageY55T64     18.320      8.462   2.165 0.034254 *  
#   educL7T8:ageY55T64   43.647      8.462   5.158 2.78e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 11.97 on 62 degrees of freedom
# Multiple R-squared:  0.9332,	Adjusted R-squared:  0.9235 
# F-statistic: 96.25 on 9 and 62 DF,  p-value: < 2.2e-16
#bütün eðitim seviyelerindeki farklýlaþma anlamlý. educ 3(temel lise) educ2 (ilk öðretim)den %24 daha fazla maaþ
#alýrken bu oran educ 5(ön lisans) için %41 ve educ 6 (lisans) içinse bu oran %70
#son olarak educ 7T8(master ve doktora) içinse %85 maaþlarý daha fazladýr. lisans ve 
#dokotora ve master yapmýþ yaþlý grubunun maaþlarýnýn ayný eðitim seviyesindeki
#gençlerden daha yüksektir.


# 13.regresyon; pooling yÃ¶ntemi ile eÄŸitim ve cinsiyet birlikte
reg13 = lm(re ~ educ*sex, data = dat)
summary(reg13)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     81.646      4.340  18.812  < 2e-16 ***
#   educL3          18.354      7.517   2.441   0.0175 *  
#   educL5          44.770      7.517   5.955 1.32e-07 ***
#   educL6          76.858      7.517  10.224 6.30e-15 ***
#   educL7T8       118.386      7.517  15.748  < 2e-16 ***
#   sexM            -8.844      6.138  -1.441   0.1547    
# educL3:sexM      8.844     10.631   0.832   0.4087    
# educL5:sexM      5.943     10.631   0.559   0.5781    
# educL6:sexM      5.487     10.631   0.516   0.6076    
# educL7T8:sexM  -21.699     10.631  -2.041   0.0455 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 15.03 on 62 degrees of freedom
# Multiple R-squared:  0.8946,	Adjusted R-squared:  0.8793 
# F-statistic: 58.46 on 9 and 62 DF,  p-value: < 2.2e-16
 
#Bu seferki oranlar ise cinsiyet etkisinden arýndýrýlmýþ
#olup ayný zamanda educ 3(temel lise) educ2 (ilk öðretim)den %18 daha fazla maaþ
#alýrken bu oran educ 5(ön lisans) için %44 ve educ 6 (lisans) içinse bu oran %76
#son olarak educ 7T8(master ve doktora) içinse %118 maaþlarý daha fazladýr.
#Burada ise cinsiyet etkisi katýlarak baktýðýmýzda akademi ve yönetici kesimde kadýnlara ayrýmcýlýk var -21 oranýnda erkekler daha fazla kazanýyor.
# 14.regresyon; pooling yÃ¶ntemi ile eÄŸitim, cinsiyet, yaÅŸ 3'Ã¼ birlikte
reg14 = lm(re ~ educ*sex*age, data = dat)
summary(reg14)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)              80.25350    3.11485  25.765  < 2e-16 ***
#   educL3                   19.74650    5.39507   3.660  0.00059 ***
#   educL5                   34.99517    5.39507   6.487 3.26e-08 ***
#   educL6                   66.63317    5.39507  12.351  < 2e-16 ***
#   educL7T8                 82.66317    5.39507  15.322  < 2e-16 ***
#   sexM                     -8.82917    4.40506  -2.004  0.05026 .  
# ageY55T64                 2.78550    4.40506   0.632  0.52993    
# educL3:sexM               8.82917    7.62978   1.157  0.25248    
# educL5:sexM              13.91583    7.62978   1.824  0.07392 .  
# educL6:sexM               7.61750    7.62978   0.998  0.32271    
# educL7T8:sexM             6.09950    7.62978   0.799  0.42768    
# educL3:ageY55T64         -2.78550    7.62978  -0.365  0.71653    
# educL5:ageY55T64         19.54950    7.62978   2.562  0.01334 *  
#   educL6:ageY55T64         20.45017    7.62978   2.680  0.00983 ** 
#   educL7T8:ageY55T64       71.44517    7.62978   9.364 9.56e-13 ***
#   sexM:ageY55T64           -0.02933    6.22969  -0.005  0.99626    
# educL3:sexM:ageY55T64     0.02933   10.79015   0.003  0.99784    
# educL5:sexM:ageY55T64   -15.94500   10.79015  -1.478  0.14551    
# educL6:sexM:ageY55T64    -4.26000   10.79015  -0.395  0.69460    
# educL7T8:sexM:ageY55T64 -55.59700   10.79015  -5.153 4.03e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.63 on 52 degrees of freedom
# Multiple R-squared:  0.9772,	Adjusted R-squared:  0.9689 
# F-statistic: 117.5 on 19 and 52 DF,  p-value: < 2.2e-16
#educ5 (ön lisans) genç mezunlarý yine ayný grup educ 5 önlisans yaþlý mezunlarýndan  %19 daha az maaþ alýyorlar yani yailý grubu genç grubundan
#%19 daha fazla kazanýyor. ayný þekilde educ6 (lisans) genç mezunlarý yine ayný grup educ 6 lisans yaþlý mezunlarýndan %20 daha az maaþ alýyorlar yani yailý grubu genç grubundan
#%20 daha fazla kazanýyor.ayrýca educ7T8 (master ve doktora) genç mezunlarý  yine ayný grup educ 7T8 master ve doktora yaþlý mezunlarýndan  %71 daha az maaþ alýyorlar yani yailý grubu genç grubundan
#%71 daha fazla kazanýyor.educL7T8:sexM:ageY55T64 için ise educ 7T8 deki genç erkek master ve doktora mezunlarýnýn, yaþlý erkek master ve doktora mezunlarýndan
#%55 daha fazla kazandýklarýný gösteriyor.
# 15.regresyon; pooling yÃ¶ntemi ile eÄŸitim ve macro data
reg15 = lm(re ~ educ + unem + exp + hdi, data = dat)
summary(reg15)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -979.38812 1896.84922  -0.516  0.60741    
# educL3        21.19415    6.59221   3.215  0.00205 ** 
#   educL5        46.16007    6.59221   7.002 1.84e-09 ***
#   educL6        78.02049    6.59221  11.835  < 2e-16 ***
#   educL7T8     105.95474    6.59221  16.073  < 2e-16 ***
#   unem           3.67745    7.31209   0.503  0.61674    
# exp           -0.08137    2.53408  -0.032  0.97448    
# hdi         1143.10647 2028.26116   0.564  0.57500    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 16.41 on 64 degrees of freedom
# Multiple R-squared:  0.8703,	Adjusted R-squared:  0.8561 
# F-statistic: 61.36 on 7 and 64 DF,  p-value: < 2.2e-16
# Cinsiyet ve yaþtan arýndýrýlmamýþ genel haliyle educ 3(temel lise) educ2 (ilk öðretim)den %21 daha fazla maaþ
#alýrken bu oran educ 5(ön lisans) için %46 ve educ 6 (lisans) içinse bu oran %78
#son olarak educ 7T8(master ve doktora) içinse %105 maaþlarý daha fazladýr.
