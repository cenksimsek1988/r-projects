# excel dosyasını içe aktarabilmek için gereken paket
library(readxl)

# datayı R'a aktardım
dat = read_excel(file.choose())

# 1. regresyon; en çok değişken
reg_too_many = lm(wage_diff ~ age*educ + sex*educ, data = dat)
summary(reg_too_many)

# 2. regresyon; tek değişkenli
reg_edu = lm(wage_diff ~ educ, data = dat)
summary(reg_edu)

# 3. regresyon; cinsiyet ve eğitim
reg_sex = lm(wage_diff ~ educ + sex, data = dat)
summary(reg_sex)

# 4.regresyon; cinsiyet, yaş ve eğitim
reg_sex_age = lm(wage_diff ~ educ + sex + age, data = dat)
summary(reg_sex_age)

# 5.regresyon; cinsiyet*yaş
regsexage = lm(wage_diff ~ sex*age, data = dat)
summary(regsexage)

# 6.regresyon; cinsiyet*eğitim
regsexeduc = lm(wage_diff ~ sex*educ, data = dat)
summary(regsexeduc)
