# excel dosyasını içe aktarabilmek için gereken paket
library(readxl)
# panel data regresyonu kurmak için gereken paket
library(plm)

# datayı R'a aktardım
dat = read_excel(file.choose())

# panel data oluşturdum
pdat = pdata.frame(dat)

# ilk regresyon; iki ülke birlikte, panel ve random walk yöntemiyle
preg = plm(percentage_diff ~ r_ex_r_ln + educ, data = pdat, index = c("id", "year"), model = "random")
summary(preg)

# ikinci regresyon; within yöntemiyle
pwreg = plm(percentage_diff ~ r_ex_r_ln + educ*year, data = pdat, index = c("id", "year"), model = "within")
summary(pwreg)

# üçüncü regresyon, pooling, iki ülke birlikte
reg = lm(percentage_diff ~ educ, data = dat)
summary(reg)

# datayı ülkelere bölüyorum
dat_splitted = split(dat, dat$country == "turkey")
dat_turkey = dat_splitted[[2]]
dat_malaysia = dat_splitted[[1]]

dat_turkey
dat_malaysia

# 4. regresyon; türkiye, pooling, çok değişkenli
reg_tr = lm(percentage_diff ~ r_ex_r_ln*educ + educ*sex + educ*year, data = dat_turkey)
summary(reg_tr)

# 5. regresyon; türkiye, pooling, tek değişkenli
reg_tr_educ = lm(percentage_diff ~ educ, data = dat_turkey)
summary(reg_tr_educ)

# 6. regresyon; malezya, pooling, tek değişkenli
reg_mal = lm(percentage_diff ~ educ, data = dat_malaysia)
summary(reg_mal)

# 7.regresyon; malezya, pooling, çok değişkenli
reg_mal_r = lm(percentage_diff ~ r_ex_r_ln*educ, data = dat_malaysia)
summary(reg_mal_r)

# Malezya reel kur trendi
real_exchange_trend = cbind(dat_malaysia$year, dat_malaysia$r_ex_r_ln)
head(real_exchange_trend, 4)
