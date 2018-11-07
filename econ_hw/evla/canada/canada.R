library(foreign)
library(readxl)


library(dplyr)
library(dynlm)

library(timeSeries)

dat = read_excel(file.choose())
reg1=lm(inf ~ g_unem, data = dat)
summary(reg1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.9721     0.1428  27.811  < 2e-16 ***
#   g_unem       -0.5047     0.1091  -4.624 4.66e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.401 on 565 degrees of freedom
# Multiple R-squared:  0.03647,	Adjusted R-squared:  0.03476 
# F-statistic: 21.38 on 1 and 565 DF,  p-value: 4.664e-06

# işsizlikteki gap ile enflasyon arasında güçlü bir ilişki gördük.
# ve negatif, makro teoriyle uyumlu
# işsizliğin ani bir etkisinden çok gecikmeli etkisini de görebiliriz
# fakat işsizlikteki değişime bakamadık, işsiziliğin kendisine baktık
# bu yüzden time series'a çevirelim


dat_ts=ts(dat,start=c(1971,1),freq=12)

# uzun dönem ortalama işsizliğini doğal işsizlik olarak aldım
# yaklaşık 7.5
reg1 = dynlm(d(inf) ~ g_unem, data = dat_ts)
summary(reg1)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.001898   0.022931   0.083  0.93407   
# g_unem      -0.046307   0.017526  -2.642  0.00847 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5455 on 564 degrees of freedom
# Multiple R-squared:  0.01223,	Adjusted R-squared:  0.01048 
# F-statistic: 6.981 on 1 and 564 DF,  p-value: 0.008466

# yine anlamlı çıktı, fakat güven seviyemiz binde birden yüzde bire düştü
# belki de naif beklentiler gerçekçi değildir

#acaba krizler? küresel rekabet? işsizlikteki gap'in gecikmeli etkisi? bunlar açıklayıcı olabilir mi?

# petrol krizi ve reel döviz kurunu dahil ettik
reg3 = dynlm(data = dat_ts, d(inf) ~ g_unem + L(g_unem, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)) +  real_exc + crisis)
summary(reg3)
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24))10 -0.165373   0.096397  -1.716   0.0868 .
# bir lagımız %10 seviyesinde anlamlı
# ve teorinin söylediği üzere negatif
# yani işsizlikte artış, enflasyonu laglı şekilde aşağıya çekiyor olabilir
# şimdi laglarımızı azaltalım ve 10. lag çevresiyle ilgilenelim

reg3 = dynlm(data = dat_ts, d(inf) ~ g_unem + L(g_unem, c(1)) +  real_exc + crisis)
summary(reg3)


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)     -0.029827   0.199827  -0.149    0.881
# g_unem          -0.006695   0.043576  -0.154    0.878
# L(g_unem, c(1)) -0.046566   0.043809  -1.063    0.288
# real_exc         0.099977   0.498895   0.200    0.841
# crisis          -0.041498   0.062461  -0.664    0.507
# 
# Residual standard error: 0.5463 on 561 degrees of freedom
# Multiple R-squared:  0.01474,	Adjusted R-squared:  0.007715 
# F-statistic: 2.098 on 4 and 561 DF,  p-value: 0.0797
# yukarıdaki regresyonun bir çok versiyonunu denedik fakat laglar için anlamlı bir aralık bulamadık

# sorunlar belki mevsimsellik yüzünden olabilir mi?

# datamızı mevsim etkisinden arınmış halde tekrar oluşturuyorum
# trendden kasten ayırmıyorum, çünkü aralarında ters trendli lineer bir ilişki olduğuna inanıyoruz
inf_ts = ts(dat$inf, start = c(1971,1), frequency = 12)
comp_inf = decompose(inf_ts, "additive")
plot(comp_inf)
inf = inf_ts - comp_inf$seasonal

g_unem_ts = ts(dat$g_unem, start = c(1971,1), frequency = 12)
comp_g_unem = decompose(g_unem_ts, "additive")
plot(comp_g_unem)
g_unem = g_unem_ts - comp_g_unem$seasonal

r_exc = dat$real_exc

bond = dat$bond

crisis = dat$crisis

period = dat$period

adj_dat = cbind(crisis, bond, unem, r_exc, inf, period)

adj_ts = ts(adj_dat, start = c(1971,1), frequency = 12)
dat_p1 = subset(adj_ts, period == "p1")
dat_p1 = ts(dat_p1, start = c(1971,1), frequency = 12)
dat_c1 = subset(adj_ts, period == "c1")
dat_c1 = ts(dat_c1, start = c(1976,1), frequency = 12)

dat_p2 = subset(adj_ts, period == "p2")
dat_p2 = ts(dat_p2, start = c(1999,6), frequency = 12)

dat_c2 = subset(adj_ts, period == "c2")
dat_c2 = ts(dat_c2, start = c(2009,1), frequency = 12)

reg4 = dynlm(data = dat_p1, d(inf) ~ L(g_unem, c(2,3,4,5)) + bond + d(r_exc) + d(L(r_exc)))
summary(reg4)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                 0.35823    4.84769   0.074   0.9414  
# L(g_unem, c(2, 3, 4, 5))2  -0.35466    6.76966  -0.052   0.9584  
# L(g_unem, c(2, 3, 4, 5))3  -4.33940    8.98295  -0.483   0.6313  
# L(g_unem, c(2, 3, 4, 5))4  21.89210    8.99772   2.433   0.0188 *
#   L(g_unem, c(2, 3, 4, 5))5 -15.17034    7.20201  -2.106   0.0405 *
#   bond                        0.02845    0.15516   0.183   0.8553  
# d(r_exc)                   -0.08945    0.32402  -0.276   0.7837  
# d(L(r_exc))                 0.15258    0.30868   0.494   0.6234  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 16.71 on 47 degrees of freedom
# Multiple R-squared:  0.1442,	Adjusted R-squared:  0.01677 
# F-statistic: 1.132 on 7 and 47 DF,  p-value: 0.3599

# R karem hala düşük, ilk period için gözlem sayım çok düşmesine rağmen
# reel döviz kurunu da laglı aldık, çünkü J-curve etkisini hesaba kattık
# küresel rekabette güç değişimi, etkisini önce fiyatlar sonra miktarlar üzerinden ters gösterebilir
# hem tahvil faiz oranları, hem de reel döviz kuru ve lagı
# enflasyondaki değişimi açıklamıyor görünüyor
# işsizlik gap'i ise bazı laglarında anlamlı çıksa da, toplamları
# pozitif ve makro teoriyle uyuşmuyor
# tek denediğimiz regresyon bu değildi
# bunun farklı laglarını ve kendisini dahil ederek yaptığımız diğer regresyonlar da
# bunun gibi ya da daha kötü sonuçlar verdi
# belki de en üstte olduğunu düşündüğümüz ilişki
# mevsimsel etkiden kaynaklı bir yanılsamaymış
# örneğin kışın hem gıda ve hammadde fiyatları yükselirken
# hem işsizlik kimi sektörler daraldığı için
# -turizm ve tarım gibi-
# iki değişkenimiz arasında negatif ilişki varmış yanılsamasına yol açmış olabilir

reg5 = dynlm(data = dat_c1, d(inf) ~ g_unem+L(g_unem, c(1,2,3,4,5,6,7,8)) + bond + d(r_exc) + d(L(r_exc)))
summary(reg5)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                           -1.001011   3.764499  -0.266    0.791
# g_unem                                 5.189037   6.057531   0.857    0.392
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))1 -6.891075   8.602528  -0.801    0.424
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))2  8.658286   8.043894   1.076    0.283
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))3 -2.592854   7.501974  -0.346    0.730
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))4  2.129840   7.532041   0.283    0.778
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))5 -4.658249   7.499245  -0.621    0.535
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))6 -7.171897   7.485557  -0.958    0.339
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))7  4.527185   7.482797   0.605    0.546
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))8  1.124909   5.247277   0.214    0.830
# bond                                   0.003226   0.024270   0.133    0.894
# d(r_exc)                               0.217124   0.163244   1.330    0.185
# d(L(r_exc))                           -0.058409   0.163136  -0.358    0.721
# 
# Residual standard error: 30.54 on 266 degrees of freedom
# Multiple R-squared:  0.02965,	Adjusted R-squared:  -0.01413 
# F-statistic: 0.6773 on 12 and 266 DF,  p-value: 0.7729
# petrol krizi döneminde de işsizlik enflasyon ilişkisi kuramadık
# Rkare çok düşük


reg7 = dynlm(data = dat_p2, d(inf) ~ g_unem+L(g_unem, c(1,2,3,4,5,6,7,8)) + bond + d(r_exc) + d(L(r_exc)))
summary(reg7)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                            -4.82552    4.83638  -0.998   0.3208  
# g_unem                                -11.17933   10.15541  -1.101   0.2736  
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))1  16.80394   13.49070   1.246   0.2158  
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))2 -18.68455   12.35158  -1.513   0.1335  
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))3  14.51376   10.75891   1.349   0.1804  
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))4  -4.65526   10.62588  -0.438   0.6623  
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))5 -12.03719   10.30645  -1.168   0.2456  
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))6   8.53048   10.07756   0.846   0.3993  
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))7  -2.43716   10.12484  -0.241   0.8103  
# L(g_unem, c(1, 2, 3, 4, 5, 6, 7, 8))8   6.74547    7.68849   0.877   0.3824  
# bond                                    0.07838    0.07755   1.011   0.3146  
# d(r_exc)                                0.82839    0.36224   2.287   0.0243 *
#   d(L(r_exc))                            -0.05515    0.36104  -0.153   0.8789  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 21.93 on 100 degrees of freedom
# Multiple R-squared:  0.1213,	Adjusted R-squared:  0.01586 
# F-statistic:  1.15 on 12 and 100 DF,  p-value: 0.3294
# petrol krizini takip eden dönemde de bir ilişki tespit edemedik
# R karemiz hala düşük
# reel döviz kuru bu dönemde enflasyon artışını ters yönde etkilemiş
# Kanada için, bu dönemde kıymetli mallar, enflasyon frenleyici görünüyor

reg8 = dynlm(data = dat_c2, d(inf) ~ g_unem + L(g_unem, c(1,2,3)) + bond + d(r_exc) + d(L(r_exc)))
summary(reg8)
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept)                                   4.7228     7.5614   0.625   0.5335  
# g_unem                                        4.5185    13.1144   0.345   0.7311  
# L(g_unem, c(1, 2, 3))zoo(coredata(x), tt).1  12.1949    16.8907   0.722   0.4718  
# L(g_unem, c(1, 2, 3))zoo(coredata(x), tt).2 -39.6499    17.3580  -2.284   0.0243 *
#   L(g_unem, c(1, 2, 3))zoo(coredata(x), tt).3  25.4493    13.6888   1.859   0.0657 .
# bond                                         -0.0823     0.1100  -0.748   0.4561  
# d(r_exc)                                      0.1888     0.1051   1.797   0.0750 .
# d(L(r_exc))                                   0.1894     0.1059   1.789   0.0763 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 39.19 on 111 degrees of freedom
# (0 observations deleted due to missingness)
# Multiple R-squared:  0.08809,	Adjusted R-squared:  0.03059 
# F-statistic: 1.532 on 7 and 111 DF,  p-value: 0.1638
# 2008 krizi ve sonrası
# güvenilir bir açıklayacı değişkenimiz bu dönemde de yok
# reel döviz kuru ile işsizlik değişimi %10 seviyesinde birşeyler ifade ediyor
# bu sefer küresel piyasada pahallı mallar enflasyonist etki yaratmış gibi
# işsizlik değişiminin bir lagının en anlamlı görünen lagının
# katsayısı negatif, bu da makro teoriyle uyuşuyor
# fakat r kare çok düşük
# laglar farklı işaretliler
# buradan da bir yargı üretemiyoruz

reg9 = dynlm(data = adj_ts, d(inf) ~ period + L(g_unem, c(3,4,5,6,7,8,9,10,11,12)) + bond + d(r_exc) + d(L(r_exc)))
summary(reg9)
# Coefficients:
# (Intercept)                                      -2.100777   7.318273  -0.287    0.774  
# period                                            0.319850   2.313827   0.138    0.890  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))3    5.115537   8.507798   0.601    0.548  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))4   14.053490  11.944204   1.177    0.240  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))5  -27.785118  11.907329  -2.333    0.020 *
#   L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))6   -3.716914  11.975848  -0.310    0.756  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))7    6.777125  12.016230   0.564    0.573  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))8   19.425806  12.056319   1.611    0.108  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))9  -15.127312  11.976729  -1.263    0.207  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))10   2.251105  11.855629   0.190    0.849  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))11   3.954820  11.843279   0.334    0.739  
# L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))12  -4.398055   8.479375  -0.519    0.604  
# bond                                              0.004151   0.017510   0.237    0.813  
# d(r_exc)                                          0.185476   0.126145   1.470    0.142  
# d(L(r_exc))                                       0.034036   0.126148   0.270    0.787  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 64.69 on 540 degrees of freedom
# Multiple R-squared:  0.02663,	Adjusted R-squared:  0.001392 
# F-statistic: 1.055 on 14 and 540 DF,  p-value: 0.3965

# alt dönemlere ayırmadığımızda denediğimiz laglar arasından en iyi görüneni bu çıktı
# 5. lag %5 seviyesinde anlamlı
# rkare çok düşük

# en iyisi toplu f-teste sokup kontrol edelim

linearHypothesis(reg9, c("L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))3+ L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))4 
+L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))5+ L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))6 
                         + L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))7 + L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))8
                         + L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))9 + L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))10
                         + L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))11 + L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))12"))
#Model 1: restricted model
# Model 2: d(inf) ~ period + L(g_unem, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)) + 
#   bond + d(r_exc) + d(L(r_exc))
# 
# Res.Df     RSS Df Sum of Sq      F Pr(>F)
# 1    541 2259774                           
# 2    540 2259571  1    203.35 0.0486 0.8256
# F Pr(>F) değerine bakıyoruz ve %5ten küçük değil, hatta çok yüksek
# yani f değerimiz anlamsız çıktı (toplamın sıfırdan farklı olmama ihtimali çok yüksek)
# bu yüzden lagların coefficientlarının toplam etkisinin pozitif ya da negatif olacağına dair yorum yapamıyoruz
