library(readxl)

#datayı R'a aktardım
dat = read_excel(file.choose())

# zaman serisine çevirdim, datam bir zaman serisi
# DESTATIS'de ve World Bank'te yıllık dataya erişebildim
# World Bank'taki daha geniş bir data ve
# 1983'te başlıyor, 2016'da bitiyor

#pooling yöntemiyle regresyon kuruyorum. datam panel değil
#zaman serisi kullanmadım, çünkü datam yıllık

#Phillips'in ilk kuramına göre;
# enflasyon = beklenen enflasyon -B(işsizlikte değişim)
# regresyona kolay sokabilmek önce denklemi değiştiriyorum
# enflasyon - beklenti = -B(işsizlikte değişim)
# beklenti datasını bulmak çok zor, DESTATIS'de vs bulunmuyor. BLOOMBERG gibi paralı bir kaç data sunucudan alınabilir.
# onun yerine naif beklenti varsayımını yapıyorum
# beklenti = önceki senenin enflasyonu
# bu durumda denklemim:
# enflasyon - enflasyon(t-1) = -B(işsizlik - işsizlik(t-1))

#enflasyon değişimi ve işsizlik değişi için vektörler oluşturup tablomu düzenliyorum

laggedunemp = c(tail(dat$unemp, -1), NA)
laggedunemp
d_unemp = c(NA, head(dat$unemp - laggedunemp, -1))
d_unemp
dat = cbind(dat , d_unemp)
dat
laggedinf = c(tail(dat$inf, -1), NA)
laggedinf
d_inf = c(NA, head(dat$inf - laggedinf, -1))
d_inf
dat = cbind(dat , d_inf)
dat = na.omit(dat)

#lm fonksiyonu için ilgili kütüphane
library(foreign)

# şimdi "diff" fonksiyonu ve lm (pooling) yöntemiyle regresyon kurabilirim
reg = lm(d_inf ~ d_unemp, data = dat)


#sonuç
summary(reg)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.50494 -0.38597 -0.08454  0.68468  1.66814 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.06165    0.16283   0.379    0.708
# d_unemp     -0.24157    0.21005  -1.150    0.259
# 
# Residual standard error: 0.9316 on 31 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.04092,	Adjusted R-squared:  0.009982 
# F-statistic: 1.323 on 1 and 31 DF,  p-value: 0.2589

# interceptimiz sıfıra yakın ama anlamsız
# makro teori sıfır olması gerektiğini söylüyordu
# beta 1 negatif çıktı, t değerimiz %74lerde takılmış olmasına rağmen
# çok küçük değil, negatif çıkası da makro teoriyle uyumlu

#sebebi 2008 krizi sonrası likidite tuzağı olabilir mi?
# düşük işsizlik ve düşük enflasyon birlikte gözlemleniyor

# datayı ikiye ayırıyorum

dat_splitted = split(dat, dat$year < 2008)
dat_before = dat_splitted[[2]]
dat_before

reg_before = lm(d_inf ~ d_unemp, data = dat_before)
summary(reg_before)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6025 -0.3612 -0.1465  0.6488  1.6328 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.008703   0.205812   0.042    0.967
# d_unemp     -0.392030   0.253898  -1.544    0.137
# 
# Residual standard error: 1.001 on 22 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.09777,	Adjusted R-squared:  0.05676 
# F-statistic: 2.384 on 1 and 22 DF,  p-value: 0.1368

# t değerimiz arttı! 1 tipi hata seviyemiz %14'ün altında
# demekki 2008 krizi ve sonrası regresyonda kırılma yaratıyormuş

# makro teori bize değişimden bahsediyor, belki de bir önceki yılla sınırlanmamalıyız
# işsizlikteki değişim, daha uzun vadelerde sarkarak etki etmiş olabilir mi?
# elimdeki datanın darlığını da göz önünde bulundurarak (13 satır!!)
# işsizlik değişimi datama iki lag daha ekliyorum

d_unemp2 = c(NA, head(dat_before$d_unemp, -1))
d_unemp2
d_unemp3 = c(NA, head(d_unemp2, -1))
d_unemp3
dat_before = cbind(dat_before, d_unemp2, d_unemp3)
dat_before = na.omit(dat_before)
dat_before

reg_before_multi_lag = lm(d_inf ~ d_unemp + d_unemp2 + d_unemp3, data = dat_before)
summary(reg_before_multi_lag)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.76701 -0.58916 -0.06678  0.53149  1.42976 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -0.1911     0.2001  -0.955  0.35216   
# d_unemp      -0.7387     0.3150  -2.345  0.03070 * 
#   d_unemp2      0.4394     0.3829   1.148  0.26616   
# d_unemp3     -1.0082     0.3493  -2.886  0.00983 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8888 on 18 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.4025,	Adjusted R-squared:  0.3029 
# F-statistic: 4.041 on 3 and 18 DF,  p-value: 0.02323

# 1. ve 3. laglar anlamlı, 3.lag binde birden küçük hata payı bıraktı
# hem de elimizdeki küçücük datayla

# R2 %40! elimizdeki dar datada yer alan enflasyon değişimlerinin (varsayımsal olarak beklentilerden sapmalarının)
# varyansının %40'ını işsizlik değişimi ve 2 lagıyla açıklayabiliyoruz.

# ikinci lag ise hem pozitif hem de anlamsız
# toplu teste sokalım

library(car)


linearHypothesis(reg_before_multi_lag, c("d_unemp", "d_unemp2", "d_unemp3"))

# Hypothesis:
#   d_unemp = 0
# d_unemp2 = 0
# d_unemp3 = 0
# 
# Model 1: restricted model
# Model 2: d_inf ~ d_unemp + d_unemp2 + d_unemp3
# 
# Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1     21 23.797                              
# 2     18 14.220  3    9.5771 4.0411 0.02323 *

# F-testimizden hata olasılığı binde 2.3 çıktı
# bu da 3 lag'ın birden anlamsız olmadığına güçlü bir işaret

# sonuç: almanyada, enflasyon değişimi ile işsizlikte değişimin sarkmalı serisi arasında
# güçlü bir ilişki olduğunu gösteriyor.
# fakat bu ilişki 2008 ve sonrasında kopuyor (Likidite tuzağı)
# ilişki için sebep - sonuç kurmak için çok erken
# GSYH, parasal şoklar, 2008 öncesindeki kimi depresyon ve resesyonlar..
# bunlar, işsizlik oranlarını değiştirebilecek etmenler
# benzer şekilde enflasyonu da etkileyebilirler
# regresyonun bize işaret ettiği ilişki
# gayet yukarıda saydıklarımın (GSYH vs), işsizlik orannına etkileri üzerinden
# mevcut olan bir ilişki olabilir
# bunun için dataya bu değerleri ekleyebilirdim
# fakat datam yıllık ve çok az yılım var, bu yüzden, Phillips'in öngördüğü ilişkinin
# belli koşullar altında mevcut olduğunu, sebep - sonuç ilişkisine dair yorum yapmadan
# iddia edebiliriz.

# HOCAYA SORUN:
# 1) modellediğim Philips Curve'den memnun mu? Farklı bir model kurmamı ister mi?
# 2) Naif enflasyon beklentisi varsayımımdan memnun mu?
# 3) yıllık datada, yani mevsimlik etki gözlemleyemeyeceğimiz bir datada, zaman serileri kullanayım mı?
# 4) gdp, parasal şoklar vs gibi bir datayı, yıl gözlemim çok az olmasına rağmen, modelimize ekleyeyim mi?
# 5) STATISDE ve World Bank dışında, almanyanın aylık datasını bulabileceğim bir kaynak biliyor mu?
