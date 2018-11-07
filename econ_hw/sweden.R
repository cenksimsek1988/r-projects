# excel dosyasını içe aktarabilmek için gereken paket
library(readxl)

# panel data regresyonu kurmak için gereken paket
library(plm)


# datayı R'a aktardım
dat = read_excel(file.choose())

# panel data oluşturdum
pdat = pdata.frame(dat)

# ilk regresyon; eğitim ve yıl
reg = plm(pp ~ ed + time, data = pdat, model = "random")
summary(reg)

# 2. regresyon; cinsiyet dahil
reg2 = plm(pp ~ ed + time + gender, data = pdat, model = "random")
summary(reg2)

# 3. regresyon; yaş
reg3 = plm(pp ~ ed + time + gender + age, data = pdat, model = "random")
summary(reg3)

# 4. regresyon; argelere ayrılan bütçe payı
reg4 = plm(pp ~ ed + research, data = pdat, model = "random")
summary(reg4)

# 5. regresyon; human development index
reg5 = plm(pp ~ ed + hdi, data = pdat, model = "random")
summary(reg5)

# 6. regresyon; unemployment
reg6 = plm(pp ~ ed + unemp, data = pdat, model = "random")
summary(reg6)

# 7. regresyon; export to gdp
reg7 = plm(pp ~ ed + exp_gdp, data = pdat, model = "random")
summary(reg7)

# 8. regresyon; import to gdp
reg8 = plm(pp ~ ed + imp_gdp, data = pdat, model = "random")
summary(reg8)

# 9.regresyon; unit value index of export
reg9 = plm(pp ~ ed + exp_unit_index, data = pdat, model = "random")
summary(reg9)

# 10.regresyon; inflation
reg10 = plm(pp ~ ed + inf, data = pdat, model = "random")
summary(reg10)

# 11.regresyon; big mac index
reg11 = plm(pp ~ ed + big_mac, data = pdat, model = "random")
summary(reg11)

# 12.regresyon; pooling yöntemi ile eğitim ve yaş birlikte
reg12 = lm(pp ~ ed*age, data = dat)
summary(reg12)

# meslek lisesi üni gençler karşılaştırması
library(car)
linearHypothesis(reg12, c("edL4 = edL5T8"))

# 13.regresyon; pooling yöntemi ile eğitim ve cinsiyet birlikte
reg13 = lm(pp ~ ed*gender, data = dat)
summary(reg13)

# 13.regresyon; pooling yöntemi ile eğitim, cinsiyet, yaş 3'ü birlikte
reg13 = lm(pp ~ ed*gender*age, data = dat)
summary(reg13)

# 14.regresyon; pooling yöntemi ile eğitim ve macro data
reg14 = lm(pp ~ ed + research, data = dat)
summary(reg14)

