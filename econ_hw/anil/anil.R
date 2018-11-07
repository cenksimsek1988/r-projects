library(foreign)
library(readxl)


library(dplyr)
library(dynlm)

library(timeSeries)

datazz = read_excel(file.choose())
reg1=lm(life_exp ~ per_capita, data = datazz)
summary(reg1)

# Call:
#   lm(formula = life_exp ~ per_capita, data = datazz)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4703 -0.4601 -0.1210  0.2976  1.9490 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.346e+01  3.630e-01  174.80  < 2e-16 ***
#   per_capita  1.063e-02  7.145e-04   14.88 1.55e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8466 on 27 degrees of freedom
# Multiple R-squared:  0.8914,	Adjusted R-squared:  0.8873 
# F-statistic: 221.5 on 1 and 27 DF,  p-value: 1.555e-14
# ilişki tespit edildi R yüksek. negatif olması uygun
# hiç harcama olmasa 63 sene öngörüyor


#tsye çevirdim
#önceki senelerdeki harcamalar ömür uzatmış mı
datazz_ts=ts(datazz,start=1988,freq=1)

reg2 = dynlm(life_exp ~ per_capita + L(per_capita, c(1,2,3,4,5,6,7,8,9,10)), data = datazz_ts)
summary(reg2)
# Time series regression with "ts" data:
#   Start = 1998, End = 2016
# 
# Call:
#   dynlm(formula = life_exp ~ per_capita + L(per_capita, c(1, 2, 
#                                                           3, 4, 5, 6, 7, 8, 9, 10)), data = datazz_ts)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.39981 -0.18710 -0.00582  0.13672  0.67938 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                        6.597e+01  5.773e-01 114.265 1.04e-12 ***
#   per_capita                                        -4.578e-04  5.325e-03  -0.086    0.934    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))1   1.008e-03  7.408e-03   0.136    0.896    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))2   7.787e-04  6.651e-03   0.117    0.910    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))3  -5.980e-05  6.088e-03  -0.010    0.992    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))4  -1.317e-03  6.082e-03  -0.216    0.835    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))5   7.526e-03  6.126e-03   1.229    0.259    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))6   5.622e-05  6.896e-03   0.008    0.994    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))7  -2.786e-03  7.657e-03  -0.364    0.727    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))8   8.210e-03  8.944e-03   0.918    0.389    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))9   4.913e-04  9.692e-03   0.051    0.961    
# L(per_capita, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))10 -5.795e-03  6.690e-03  -0.866    0.415    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4094 on 7 degrees of freedom
# Multiple R-squared:  0.9704,	Adjusted R-squared:  0.9239 
# F-statistic: 20.87 on 11 and 7 DF,  p-value: 0.0002659
# hayır en ufak bşr işaret yok


# az lagla deneyelim
reg3 = dynlm(life_exp ~ per_capita + L(per_capita, c(1,2)), data = datazz_ts)
summary(reg3)
# Time series regression with "ts" data:
#   Start = 1990, End = 2016
# 
# Call:
#   dynlm(formula = life_exp ~ per_capita + L(per_capita, c(1, 2)), 
#         data = datazz_ts)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.40581 -0.37527  0.00403  0.26042  1.95609 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             6.423e+01  4.023e-01 159.664   <2e-16 ***
#   per_capita              3.763e-03  5.104e-03   0.737    0.468    
# L(per_capita, c(1, 2))1 1.541e-04  6.809e-03   0.023    0.982    
# L(per_capita, c(1, 2))2 5.860e-03  5.057e-03   1.159    0.258    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7481 on 23 degrees of freedom
# Multiple R-squared:  0.8997,	Adjusted R-squared:  0.8866 
# F-statistic: 68.74 on 3 and 23 DF,  p-value: 1.237e-11
# yine yok
# belki de harcamalar önleyici değil, hastalık oluştuktan sonra müdahale edici harcamadır


# çok değişkenli
reg4 = dynlm(data = datazz_ts, life_exp ~ per_capita + inf + med_ins_r + phy_r + nur_r + mid_w_r + phar_r)
summary(reg4)
# Time series regression with "ts" data:
#   Start = 1988, End = 2016
# 
# Call:
#   dynlm(formula = life_exp ~ per_capita + inf + med_ins_r + phy_r + 
#           nur_r + mid_w_r + phar_r, data = datazz_ts)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.88119 -0.11823  0.00447  0.14903  1.01192 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 53.7537502  1.8517139  29.029  < 2e-16 ***
#   per_capita   0.0004156  0.0023050   0.180 0.858640    
# inf          0.0100362  0.0059121   1.698 0.104364    
# med_ins_r   -0.0335750  0.0156869  -2.140 0.044223 *  
#   phy_r        0.0918533  0.0212280   4.327 0.000297 ***
#   nur_r        0.0090048  0.0039617   2.273 0.033653 *  
#   mid_w_r      0.0089567  0.0318890   0.281 0.781559    
# phar_r       0.0022299  0.1016781   0.022 0.982710    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3867 on 21 degrees of freedom
# Multiple R-squared:  0.9824,	Adjusted R-squared:  0.9765 
# F-statistic: 167.2 on 7 and 21 DF,  p-value: < 2.2e-16
# enflasyonun anlamsız çıkması normal. doğrudan bir etkisi olamaz. sadece yüksek enflasyon genel olarak zorlu yaşam koşullarıyla özdeş yıllar
# bu yüzden az değişkenimiz olsaydı rakam anlamlı çıkabilirdi ki anlamlı olmaya yakın değeri var
# enflasyonun yarattığı etkiyi, harcama rakamlarını 2010 sabit fiyatlarına çevirerek yok ettik, etmeseydik bu yüzden de
# anlamlı bir sayı dönebilirdi
# kişi başına düşen reel sağlık harcaması ile ortalama yaşam beklentisi arasındaki bağ koptu
# hemşire ve doktor sayıları anlamlı çıktılar. 1 doktor bir hemşireden ortalama on kat daha fazla yaşam arttıran etkide
# sağlık harcaması anlamını yitirdi
# neden?
# seçtiğiniz data setten


# sağlık harcamalarıyla doktor vs oranlarını regresyona sokuyorum,
reg5 = dynlm(data = datazz_ts, phy_r ~ per_capita + inf + med_ins_r + phar_r + nur_r + mid_w_r)
summary(reg5)
# Time series regression with "ts" data:
#   Start = 1988, End = 2016
# 
# Call:
#   dynlm(formula = phy_r ~ per_capita + inf + med_ins_r + phar_r + 
#           nur_r + mid_w_r, data = datazz_ts)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -5.475 -2.549 -1.296  2.272  7.204 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -3.759e+01  1.678e+01  -2.240 0.035540 *  
#   per_capita   8.043e-02  1.555e-02   5.172 3.48e-05 ***
#   inf          2.388e-02  5.916e-02   0.404 0.690410    
# med_ins_r    3.397e-01  1.399e-01   2.427 0.023841 *  
#   phar_r       3.065e+00  7.847e-01   3.906 0.000758 ***
#   nur_r       -2.745e-04  3.979e-02  -0.007 0.994557    
# mid_w_r      4.704e-01  3.042e-01   1.547 0.136241    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.884 on 22 degrees of freedom
# Multiple R-squared:  0.9869,	Adjusted R-squared:  0.9834 
# F-statistic:   277 on 6 and 22 DF,  p-value: < 2.2e-16
# çok yüksek r karemiz var
# iddia edebiliriz ki elimizdeki değişkenerden, kişi başına düşen doktor sayısını
# kişi başına devlet sağlık harcaması oldukça belirliyor, en yüksek t değeri de ona ait
# o zaman hatamız şu:
# sağlık harcaması ile ömür arasındaki ilişki
# doktor sayısı ile ömür arasındaki ilişkiden daha dolaylı olduğu için
# ikincil kalmıştı, dolaylı etkisinin çoğu diğer değişkenlerin katsayılarına dağıldı
# ve ilişki yokmuş gibi göründü
# aslında bir önceki regresyondan dönen sonuç bize tam da şunu söylüyordu:
# eğer doktor hemşire ebe sağlık kurumu enflasyon vs yi hepsini sabit tutmayı başararak
# sağlık harcaması yaparsam, ortalama ömür uzamayabilir
# demekki devlet sağlık harcamalarının, ana ve en etkili kaynağı, buralara yapılan harcamalarmış
# spor tesisleri, sağlıklı beslenme için turlar, çevre teftişi vs gibi şeyler de olabilirdi
# bunları yapsayı ve yöntemler etkili olsaydı, harcamalar kısmında hala anlamlı bir t değeri görmeyi umardık

# sizin regresyonlardan bi de bunu beğendim
reg6=lm(r_h_e ~ year, data = datazz)
summary(reg6)
# Call:
#   lm(formula = r_h_e ~ year, data = datazz)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -4.967e+09 -2.380e+09  2.921e+07  2.267e+09  6.535e+09 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -4.212e+12  1.399e+11  -30.11   <2e-16 ***
#   year         2.120e+09  6.987e+07   30.34   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.148e+09 on 27 degrees of freedom
# Multiple R-squared:  0.9715,	Adjusted R-squared:  0.9705 
# F-statistic: 920.6 on 1 and 27 DF,  p-value: < 2.2e-16
# yıllar içinde 2010 sabit fiyatlarıyla, yani gerçek anlamda artan bir trend var. bu basit trend, frekansımız olmadığı için
# timeseries ile daha iyi bir trend oluşturamıyorduk. (ts trendi moving average'la oluşturuyor, frekans kadar ileri geri
# gidiyor ve ortalamayı yerleştiriyor)
