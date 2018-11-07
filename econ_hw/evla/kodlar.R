library(readxl)

library(plm)



dat = read_excel(file.choose())

# panel data
pdat = pdata.frame(dat)

# dengeliye çevirdim. eksik yýlý olan ilçeler temizlendi.
pdat = make.pbalanced(pdat,"shared.individuals")

reg = plm(per_capita_inc ~ scr + jhr + shr + jcr + non_educ_rate, data = pdat, model = "within")
summary(reg)

reg = plm(per_capita_inc ~ scr, data = pdat, model = "within")
summary(reg)

reg = plm(scr ~ unempr, data = pdat, model = "within")
summary(reg)

reg = plm(scr ~ unemp, data = pdat, model = "pooling")
summary(reg)

reg = plm(per_capita_inc ~ scr, data = pdat, model = "pooling")
summary(reg)

reg = plm(per_capita_inc ~ jcr, data = pdat, model = "pooling")
summary(reg)

reg = plm(per_capita_inc ~ jcr + scr, data = pdat, model = "pooling")
summary(reg)

reg = plm(per_capita_inc ~ scr + year + inmr + ourmr + lffr + unempr + prim_rate + sec_rate + ter_rate, data = pdat, model = "pooling")
summary(reg)
