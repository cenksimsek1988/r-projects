# working directory yi gösterir
getwd()
# set et
setwd(....)
# ya da session menusünden seçebilirsin

# script yazarken il setwd(..) yapılmalı (script dili session based, yeri unutur)

help(sd)
# standard deviation ın documentasyonu
# diğer yollar
? sd
# sd çift tıkla f1 yap

args(sd)
# parametreleri ve boşsa ne döner

str(sd)
# bu daha kullanışlı

# na.rm = false na leri katayım mı demek

d = runif(n = 100, min = 0, max = 1)
d2 = runif(100, 0, 1)
sd(x = d)
sd(d2)

# r-blogger.com en güzel forum
# cran.r-project.org
# listendata.com yeni bakış açısıyla R desteği

# CheatSheet
# RStudio..../cheatSheet
# paketlerin grafiksel documentasyonları burada

#' github da cheatsheet projesi var bir sürü ayrı platform için
#'
#' öneri paketler:
#' dplyr
#' lubridate zaman tarih için
#' ggplot grafik
#' machinelearning
#'
#' türkçe karakter sorunu için tools global setting code saving
#'
c = (1:50)
# 1den 50ye sıralı vektör

ls()
# environmentta olan herşeyi listeler konsola

rm(d)
# siler (remove)

rm(list = ls())
# list yazmazsam vektör değil tek eleman arar ve yapamaz

n = available.packages()
# r.cran.project/web/views sayesinde tema tema paketleri görebiliriz

#bu yükler
install.packages('fOptions')
# bu scripte import eder
library(fOptions)

# import edilen kütüphaneleri listeler
search()

# .RProfile dosyası ile paketleri baştan configure edebiliyorum

# farklı aktif paketlerde aynı isimli fonksiyonlar varsa son paketin fonkisiyonu çalışır
fArma::armaFit()
# bu şekilde hangi paketin içndeki fonksiyon belirtilir

detach(package:fOptions)
# ktüphaneyi pasif eder
# sağdaki library penceresinden de hepsi yapılabilir

install.packages('devtools')
library(devtools)

# diğer repositoryler( CRAN dışında)
devtools::install_github("tidyverse/dplyr")

#•global options ile geri versiyonları seçebiliyoruz

install.packages('installr')

#güvenilirpaketler RMetrics f ile başlayan, QRM fln notlarda var

# bazı fonksiyonlar (c,, mean fln gibi, değişken isömi olarak kullanılabilir ama fonsiyon ölür)

num = numeric(5)
char = character(10)

# TRUE doğru true yanlış

as.integer(3.96)
# integer değerine çevirir
# try catch ile warning error message hepsi ayrı ayrı handle edilebiliyo

is.numeric('kjhj')
is.integer(3.4)
is.na(n)

x = vector(mode = "numeric", lenght = 10)
y = c(5, 4, 3)
z = c(2, 3, 4.5)
# 1 ilk elemanı temsil eder
y[1]
y[4]
# na getirir hata değil

# 2018-10-24 normal tarih formatı

seq(from = 10, to = 22, by = 3)
# bununla tarih de yapabilirim
seq(from = 3,
    to = 10,
    length.out = 6)
rep(x = 11, times = 5)
rep(x = c(2, 3, 4), each = 5)

q = c(1, 2, 3)
p = c(1, 2)
p + q
l = 30:40
l[1:3]
l[-1:-3]

l[l < 35]
#burası bir vektördür l < 35
l < 35
l[l < 35 & l > 32]

subset(l, l > 34)
# küme, koşul bu kullanılmıyor. yukarıdaki kullanılıyo

which(l == 34)
# 34ün 1den başlayarak endeksini döndürür

which(l > 34)
# çoklu vektör döner

c(1:4, l, "sfs")
# combine ile vektörleri birleştirir

rev(l)
# tersten sıralar

rank(l)
# vektörün elemanlarının sıra numaralarından vektör döndürür

all(l > 29)
# bütün elemanlar için de doğru mu

any(l < 29)
# herhangi bir eleman koşulu sağlıyo mu

unique(l)
# tekrar edeneleri çıkarır

sort(l, decreasing = TRUE)
# sıralar

diff(l)
# farkların vekötürünü döndürür
diff(l, lag = 2)


setdiff(l, q)
# l'de var q'da yok. fark alma

l == q
# her elemanı karşılaştırıp vektör döner
all(l == q)
# tamamen aynı mı



x = rnorm(1000)
# normal dağılımla vektör oluşturur sd = 1 mean = 0 üzerinden


library(PerformanceAnalytics)
install.packages('PerformanceAnalytics')

skewness(x)
kurtosis(x)


# stop işareti çıktıysa arkada çalışan bir şey var devam edemez

######### LIST
x = list(FALSE, 3, "oho", 3.5)
x = c(x, 1:10)
# iki köşeli parantez listelerde kullanılır

str(x)


x = list(a = 1,
         b = "lkhj",
         c = 4.6,
         d = 1:5)
x$a
x
k = vector(mode = "list")
#boş liste oluşturma yöntemi, ama kullanmaya gerek yok

# liste ve vektörün aradaki elemanları atlayarak yeni girdi yapılabilir, uzunlukta boşları da sayar

names(x)
# keyleri döker
x = list(0:3)
names(x)
# keyler yok

x[["a"]]
# burada parametrik isim de girebilirim

x = list(a = 1:200,
         b = 2:400,
         c = c(TRUE, FALSE, FALSE))
x$2[4]
x$2
x$a[4]
x[2]
x[[2]]
x[2][4]
x[[2]][4]

x$z = 6
# z ile mapledik
names(x)
x = c(x, 5)
x
names(x)
# boş string ile keyledi

########## matrix
mat = matrix(nrow = 2, ncol = 3)
mat
d  = dim(mat)
mat(1, 1)
mat[1, 1]
d
d[2]
# kolon sayısını verir
nrow(mat)


mat[, 3]
# 3. kolonu verir

mat1 = matrix(seq(
  from = 3,
  to = 20,
  length.out = 16
),
nrow = 4,
ncol = 4)
mat1[2, ][2:3]
mat1[2, c(2, 4)]
mat[2,-c(1, 2)]
mat2 = matrix(seq(
  from = 2,
  by = 2,
  length.out = 9
),
nrow = 3,
ncol = 3)

mat3 = matrix(1:12, ncol = 4, nrow = 3)
mat3
mat3[, 4]
mat3[, 4] > 10
any(mat3[, 4] > 10)

cbind(1:3, 4:6) # kolon kolon ekler
rbind(1:3, 4:6) # satır satır ekler

rownames(mat1) = c("kjh", "lkj", "oiy", "0870")
colnames(mat1) = c("q", "e", 7, 5) # integer da alabiliyo ama almasa daha iyiymiş
t(mat1)
# traspoze eder

mat1 + mat3
mat1 * mat3 # skaler çarpar
mat1 %*% mat3 # matris çarpımı yapar
solve(mat1) # tersini verir ya da hata verir

options(scipen = 999)
# artık bilimsel gösterimden çıktık


###### data.frame
# matrixten esas farklı her eleman farklı tipte olabilir
# liste oluşturur gibi ama her vektör eşit elşemanlı olmalı

x = data.frame(isim = c("a", "b", "c"), puan = c(1, 1, 2))
x
nrow(x)
ncol(x)
dim(x)
str(x)

#ctrl+l ile konsol temizleniyor

# isimler kolonu için Factor tipi dönüyor
# nu yüzden isim ile regresyon kurmaya kalkar

x = data.frame(
  isim = c("a", "b", "c"),
  puan = c(1, 1, 2),
  stringsAsFactors = FALSE
)
str(x)
# düzeldi

options(stringsAsFactors = FALSE)
# session boyunca flase

x$isim[1]
x[[1]] # liste de olduğu için çalışıyor ama gerek yok
x[1, 1] # normal matris yöntemi
x[1, "isim"]

subset(x, select = isim)
subset(x, select = -isim)

rbind(x, c("d", 4))
# yeni kayıt

x = edit(x)
x

na.omit(x)
na.exclude(x)
# na olan satırları uçurur

y = data.frame(kilo = c(80, 70, 55), isim = c("a", "b", "c"))

merge(x, y)
# birleştirir

paste(x$isim, sep = "_")
# bu çalışmaz, elemanları tek tek virgülle girmek gerek

paste(x$isim, collapse = "_")
# bu çalışır

source("path/file.R") # ile başka bi R dosyasını import eder

nchar("ldlkf sflk sflkg")
# stringin uzunluğu döner

spl = strsplit("lksjdf0jadjfj0sdafadf0afwe0", split = "0")
str(spl)
spl[[1]][2] # bu bir liste olduğundan böyle bulunur
spl

strsplit(x = c("kljjhl-khgh", "llsfl-340"), split = "-")
# vektör girersen hepsi için yapar, o yüzden liste dönüyor

grep(1, 1:3)
grepl() # vektör içinde contains mantığüıyla olanları döndürür

outer() # çaprazlar iki vektörü?

str(capm)


write.csv(x) # dosya olarak kaydetti
install.packages("openxlsx")
library(openxlsx)
write.xlsx() # excel olarak kaydetti

library(foreign)
# bu farklı uzantılı dataları import ettirir

install.packages("quantmod")
library(quantmod)
# bu sayede yahoo google fln gibi açık kaynaklardan data çekebiliyoruz ve xts formatındadır

head(capm, 3)
tail(capm)
apple = getSymbols(
  "AAPL",
  src = "yahoo",
  auto.assign = FALSE,
  from = "2015-01-01",
  to = "2016-01-01"
)
str(apple)
# xts formatı yan yana kolon kolon eklenmiş time seriesler demek

apply() # her sütuna uygular

# veri girişi
install.packages('RODBC')
install.packages('RMySQL')
install.packages('odbc')
# odbc kütüphanesiyle örnekler yapıyoruz
# hem şifreyi bir daha istemediğinden güvenli, scriptimde şiifrem olmaz
# hem connections panelimde sürekli takip ederim
# başlattan odbc connections a yeni ekliyoruz
# bağalntı kaydediyoruz

library(odbc)

# iki yol
# 1) sqlleri yazarak data frame al

h4_results = dbSendQuery(con, paste0("select * from parity where asset_code = 'USD/TRY';"))
# paste0 ile kısayol oluşturuyoruz ?

h4 = dbFetch(h4_results)
# bunukullanmayı tercih etmiyormuş





# if ( x == y) {
#
# } else {
#
# }

x = 7
sonuc = 2 ^ x # üst böyle gösteriliyor

# exceldekine benzeyen bir fonksiyon var:
sonuc2 = ifelse(x > 10, x, 2 ^ x)
sonuc2

x = 1:3
for (i in 1:length(x)) {
  # i birden başlasın lenght e kadar
  print(x[i])
}

seq_along(x)
for (i in seq_along(x)) {
  print(x[i])
}
for (i in x) {
  print(i)
}




data = matrix(nrow = 100, ncol = 5)



i = 1:ncol(data)
for (x in 1:nrow(data)) {
  # x birden başlasın lenght e kadar
  data[, x] = rnorm(nrow(data))
}

sonuc = matrix(nrow = 5, ncol = 5)
j = 1:ncol(data)
j
for (x in 1:ncol(data)) {
  # x birden başlasın lenght e kadar
  for (j in 1:ncol(data)) {
    # j birden başlasın lenght e kadar
    sonuc[x, j] = cor(data[, x], data[, j])
  }
}
sonuc = cor(data)
sonuc

# foreach başka bişey multiprocessor için kullanılıyor

x = 1:3
i = 1
while (i < length(x) - 1) {
  print(x[i])
  i = i + 1
}

x = 8:12
sonuc = vector(mode = 'numeric', length = length(x))
for (i in 1:length(x)) {
  if (x[i] < 10) {
    row = x[i]
  } else {
    row = 2 ^ x[i]
  }
  sonuc[i] = row
}
sonuc

# repeat break

a = 0
repeat {
  a = a + 1
  print(a)
  if (a == 3) {
    break()
  }
}
f = function(x){
  result = sum(x) / length(x)
  return(result)
}

x = rnorm(1000)
mean = f(x)
mean

f2 = function(vec1){
  dat = data.frame(matrix(ncol = 1, nrow = 9))
  rownames(dat) = c('uzunluk', 'minimum', 'maximum', 'medyan', 'ortalama', 'sd', 'var', 'skewness', 'kurtosis')
  colnames(dat) = c('x')
  dat[1,1] = length(vec1)
  dat[2,1] = min(vec1)
  dat[3,1] = max(vec1)
  dat[4,1] = median(vec1)
  dat[5,1] = mean(vec1)
  dat[6,1] = sd(vec1)
  dat[7,1] = var(vec1)
  dat[8,1] = PerformanceAnalytics::skewness(vec1)
  dat[9,1] = PerformanceAnalytics::kurtosis(vec1)
  return(dat)
}
sonuc = f2(x)
sonuc

# next, koşulu sağlarsa bir methoduy bişr kere çalıştır

# apply ile hepsine uygulamak

x = data.frame(a = rnorm(6), b = rnorm(6))

sonuc = apply(x, 1, sum) # 1 çünkü satıra uygula, sütüna uygula ise 2
sonuc


dat = readr::read_csv(file.choose())

apply(dat, 2, function(dat) any(is.na(dat)))

f3 = function(dat){
  dat = data.frame(matrix(ncol = 1, nrow = 9))
  rownames(dat) = c('uzunluk', 'minimum', 'maximum', 'medyan', 'ortalama', 'sd', 'var', 'skewness', 'kurtosis')
  colnames(dat) = c(dat)
  dat[1,] = apply(dat, 2, length)
  dat[2,] = apply(dat, 2, min)
  dat[3,] = apply(dat, 2, max)
  dat[4,] = apply(dat, 2, median)
  dat[5,] = apply(dat, 2, mean)
  dat[6,] = apply(dat, 2, sd)
  dat[7,] = apply(dat, 2, var)
  dat[8,] = apply(dat, 2, PerformanceAnalytics::skewness)
  dat[9,] = apply(dat, 2, PerformanceAnalytics::kurtosis)
  return(dat)
}
sonuc = f3(dat)
sonuc

# tidyverse data manipulasyonları
install.packages("tidyverse")
library(tidyverse)

# pipeline %>% 
# dplyr deeplayer bigdata için lazım
tidyr
purr
tibble # data.frame  yerine bunu kullanıcaz
tbl_df # gibi fonksiyonlarımız var
ggplot2
string
forcast # machine learning için kullanılıyor
lubridate # zaman tarih için paket

# dplyr methodları
mutate() # kolonun kendisini değişken olarak tutup işleriz
select() # kolonlara göre seç
filter() # sqldeki where gibi,koşulu içeri yaz
summarise() # gruplandır pivot tablolar oluştur, her asset code dan kaçtane var
arrange() # sıralar


# gibi paketler

x = as.tibble(iris)
x

x %>% filter(Species == 'setosa')

starwars %>%  filter(species == 'Droid') %>% 
# starwars %>%  # pipleline satıor sopnunda olmalı!!
  select(name , ends_with('color')) %>% 
  head(.,4) # select edilmiş datayı temsil eder, son pipelinedaki datayı alır

starwars

starwars %>% 
  mutate(bmi = mass/((height/100)^2)) %>% 
  select(name:mass, bmi) %>%
  mutate(bmi_control = if_else(bmi>=30, TRUE, FALSE)) %>% 
  select(name:mass, contains('bmi')) %>% 
  filter(!bmi_control)
# mutate ile kolon ekledik

starwars %>% 
  select(name:mass, gender, species) %>% 
  mutate(
    type = case_when (
      height < 200 | mass > 200 ~ "large",
      species == "Droid"        ~ "robot",
      TRUE                      ~ "other"
    )
  ) %>% 
  mutate(bmi = mass/((height/100)^2)) %>% 
  mutate(
    category = case_when (
      bmi < 18.5 ~ "Underweight",
      bmi < 25  ~ "Normal",
      between(bmi, 25, 30) ~ "Overweight",
      TRUE                      ~ "Obes"
    )
  ) %>% 
  head

starwars %>%
  group_by(species)
starwars

starwars %>%
  group_by(species) %>% 
  summarise(
    n = n(),# count ediyor grubu
    mass = mean(mass, na.rm = TRUE)
  ) %>% 
  filter(n > 1) %>% 
  head(5)

left_join(a, b , by = c("tunc" = "cahit")) # ortak kolonda soldan birleştir..

band_instruments
band_members
band_members %>% 
  left_join(band_instruments, by = "name")

# gather - spread ile şekil değiştirir wide ya da long

dat = readr::read_csv(file.choose())
dat %>%  gather("asset", "price", -c(Date)) # dik hale çevirdi, date i bozmadan
dat %>% spread(key=IBM, value= MARKET, fill = NA) # key kısmını yayar, içini de valuelar ile doldurur

unite() # iki kolonu çaprazlar birleştiririz, unique kolon oluştur




library(odbc)

# iki yol
# 1) sqlleri yazarak data frame al

h4_results = dbSendQuery(con, paste0("select * from parity where asset_code = 'USD/TRY';"))
# paste0 ile kısayol oluşturuyoruz ?

h4 = dbFetch(h4_results)
# bunukullanmayı tercih etmiyormuş
tbl(con,"parity") %>% 
  filter(asset_code == "USD/TRY") %>% 
# datayı içeri almaz bağlantıyı kurar
  
  collect() # datayı içerialdık


parity = tbl(con,"parity") %>% 
  filter(asset_code == "USD/TRY") %>% 
  # datayı içeri almaz bağlantıyı kurar
  
  collect() # datayı içerialdık

%>% rename(boy = height) # kolon ismini değiştirdik

starwars %>% distinct(homeworld) %>% # unique olarak getirir
  pull(homeworld)

starwars %>% filter(species %in% c("Human", "Droid"))
starwars %>% filter(grepl("H", name)) # contains kolon seçerken var, where clauselarda grepl kullanılır

add_row() # yeni data satırı ekler

# lag lead fonksiyonları

do() # grup bazında apply gibi alışır

starwars %>% 
  group_by(homeworld) %>% 
  arrange((desc(height))) %>% 
  do(head(.,2))

setdiff() # fark kümesini bulur (data bazında kıyaslar, tibble içinde) takip edemediğin yeni insertleri tespitte kullanılabilir

install.packages("chron")
library(chron)

explanatory = dat %>% 
  filter(chron::is.weekend(Date)) %>%
  tidyr::fill(everything(), .direction = "down")


mutate_at() # bi hücreyi mutate eder mesela na leri ortalamalarla gibi