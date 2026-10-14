# =====================================================================
#  Seminer verileri: açık veri setlerini indirme ve ilk kullanım
#  R ve LLM ile Halk Sağlığında Veri Analizi
# ---------------------------------------------------------------------
#  Bu script İNTERNET BAĞLANTISI gerektirir (Bölüm 1 ve 2).
#  Bölüm 3 yalnızca ders klasöründeki veri/ dosyalarını kullanır.
# =====================================================================


# ---- Bölüm 1: NHANES (ABD Ulusal Sağlık ve Beslenme Araştırması) -----

# 2009-2010 ve 2011-2012 dönemleri; CRAN'daki NHANES paketiyle gelir.
#  - NHANES    : 10.000 kişi; fazla örneklemeyi gidermek için yeniden
#                örneklenmiş, basit rastgele örneklem gibi kullanılabilir.
#  - NHANESraw : 20.293 kişi; özgün örneklem, ağırlık ve tasarım
#                değişkenleriyle (WTMEC2YR, SDMVPSU, SDMVSTRA).
# Paket yazarlarının uyarısı: veri eğitim amaçlı uyarlanmıştır, araştırma
# veri tabanı olarak kullanılmamalıdır.

install.packages(c("NHANES", "survey"))   # bir kez
library(NHANES)
library(survey)
library(dplyr)

dim(NHANES)
glimpse(NHANES)
?NHANES                                   # değişken sözlüğü

# Seminer 1 için örnek: diyabete göre ortalama sistolik kan basıncı
NHANES |>
  filter(Age >= 18) |>
  group_by(Diabetes) |>
  summarise(n = n(), ort_sks = mean(BPSysAve, na.rm = TRUE))

# Seminer 4 için örnek: ağırlıklı yaygınlık (NHANESraw)
# İki 2 yıllık dönem birleştirildiği için ağırlık 2'ye bölünür.
nh <- NHANESraw |>
  filter(Age >= 18) |>
  mutate(agirlik_4yil = WTMEC2YR / 2)

nh_tasarim <- svydesign(ids = ~SDMVPSU, strata = ~SDMVSTRA,
                        weights = ~agirlik_4yil, nest = TRUE, data = nh)

mean(nh$Diabetes == "Yes", na.rm = TRUE)          # ağırlıksız
svymean(~Diabetes, nh_tasarim, na.rm = TRUE)       # ağırlıklı


# ---- Bölüm 2: The Epidemiologist R Handbook verileri ----------------

# Kurgusal bir Ebola salgınının vaka listesi (ham ve temizlenmiş sürüm).
# Türkçe kitaptaki tüm örnekler bu veriyle çalışır:
# https://epirhandbook.com/tr/
# Lisans: CC BY-NC-SA 4.0 (ticari olmayan eğitimde kaynak göstererek).

install.packages("pacman")                         # bir kez
pacman::p_install_gh("appliedepi/epirhandbook")    # bir kez (GitHub'dan)
library(epirhandbook)

# Açılan pencerede ders klasörünüzdeki veri/ klasörünü seçin:
get_data("linelist_raw.xlsx")
get_data("linelist_cleaned.rds")

linelist_ham   <- readxl::read_excel("veri/linelist_raw.xlsx")
linelist_temiz <- readRDS("veri/linelist_cleaned.rds")
dim(linelist_temiz)


# ---- Bölüm 3: TNSA benzeri veri (internet gerekmez) -----------------

# Sentetik; TNSA'ya benzer tabakalı küme örneklemi. Kuzey, Doğu ve kır
# yerleşimleri fazla örneklenmiştir: ağırlıksız sonuç yanıltıcı olur.
library(haven)

tnsa <- read_sav("veri/tnsa_benzeri_kadin.sav")

# SPSS'te tanımlı eksik kodları (98, 9996, 9998, 8) otomatik NA olur:
summary(as.numeric(tnsa$dogum_agirligi))
# Kodları görmek isterseniz:
tnsa_kodlu <- read_sav("veri/tnsa_benzeri_kadin.sav", user_na = TRUE)
table(tnsa_kodlu$dogum_agirligi >= 9996)

# Ağırlık DHS/TNSA geleneğindeki gibi 1.000.000 ile çarpılmış saklanır:
dogum <- tnsa |>
  filter(son5yil_dogum == 1) |>
  mutate(agirlik = agirlik / 1e6)

mean(dogum$sezaryen)                                 # ağırlıksız: %52,5

tnsa_tasarim <- svydesign(ids = ~kume, strata = ~tabaka, weights = ~agirlik,
                          nest = TRUE, data = dogum)
svymean(~sezaryen, tnsa_tasarim, deff = "replace")   # ağırlıklı: %58,5
confint(svymean(~sezaryen, tnsa_tasarim))
svyby(~sezaryen, ~as_factor(bolge), tnsa_tasarim, svymean)
