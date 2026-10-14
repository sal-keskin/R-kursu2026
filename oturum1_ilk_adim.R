# =====================================================================
#  R ve LLM ile Halk Sağlığında Veri Analizi
#  Oturum 1 (16 Ekim 2026): R ve RStudio'ya ilk adım
#  Hacettepe Üniversitesi Tıp Fakültesi Halk Sağlığı AD
#  Epidemiyoloji Bilim Dalı
# ---------------------------------------------------------------------
#  NASIL KULLANILIR?
#  - İmleç bir satırdayken Ctrl + Enter (Mac: Cmd + Enter) o satırı
#    çalıştırır; sonuç alttaki Console panelinde görünür.
#  - "#" ile başlayan satırlar yorumdur, R bunları çalıştırmaz.
#  - Bu dosyayı R_egitim.Rproj üzerinden açtığınızdan emin olun.
#  - Veri SENTETİKTİR (eğitim için üretilmiştir), gerçek kişilere ait değildir.
# =====================================================================


# ---- Bölüm 0: Proje ve çalışma klasörü ------------------------------

# R_egitim.Rproj dosyasına çift tıklayarak açtıysanız çalışma klasörünüz
# hazırdır. Kontrol edelim:
getwd()            # Şu an hangi klasördeyim?

# çalışma klasörünü değiştirmek için setwd() fonksiyonu kullanılabilir,
# bir örnek aşağıda, Win ve Mac için slash (/) - backslash (\) farkları vardır.

# setwd("/Users/kullanici/Documents/R_egitim")      # Mac
# setwd("C:/Users/kullanici/Documents/R_egitim")    # Windows: \ yerine / yazın


list.files()       # Bu klasörde hangi dosyalar var?
list.files("veri") # veri/ klasöründe hangi dosyalar var?


# ---- Bölüm 1: R bir hesap makinesidir ------------------------------

2 + 3
10 / 4
2^10
sqrt(144)

# Örnek: Bir düğünde 150 davetliden 73'ü hastalanmış. Atak hızı (%) nedir?
73 / 150 * 100


# ---- Bölüm 2: Nesneler ve atama ( <- ) -----------------------------

# Sonucu bir isme kaydedersek sonra tekrar kullanabiliriz.
hasta_sayisi <- 73
davetli      <- 150
atak_hizi    <- hasta_sayisi / davetli * 100

atak_hizi            # Nesnenin içine bakmak için adını yazıp çalıştırın
round(atak_hizi, 1)  # Bir ondalık basamağa yuvarla

# Kısayol: Alt + -  (Mac: Option + -) tuşları "<-" yazar.
# DİKKAT: R büyük-küçük harfe duyarlıdır. atak_hizi ile Atak_hizi farklıdır.
# Nesneler sağ üstteki Environment panelinde listelenir.


# ---- Bölüm 3: Vektörler: birden çok değer, tek nesne ----------------

# c() fonksiyonu ("combine") değerleri bir araya getirir.
yaslar <- c(34, 51, 67, 45, 29, 72, 58)

length(yaslar)   # Kaç değer var?
mean(yaslar)     # Ortalama
median(yaslar)   # Ortanca
sd(yaslar)       # Standart sapma
range(yaslar)    # En küçük ve en büyük

yaslar >= 65        # Her değer için TRUE (doğru) / FALSE (yanlış)
sum(yaslar >= 65)   # 65 yaş ve üstü kaç kişi? (TRUE = 1 sayılır)

# Metin (karakter) vektörü: değerler tırnak içinde yazılır
cinsiyet <- c("Kadın", "Erkek", "Kadın", "Kadın", "Erkek", "Erkek", "Kadın")
table(cinsiyet)     # Frekans tablosu


# ---- Bölüm 4: Fonksiyonlar ve yardım -------------------------------

# Yapı: fonksiyon_adi(argüman1, argüman2 = değer, ...)
round(3.14159, digits = 1)

# Yardım sayfası sağ alttaki Help sekmesinde açılır:
?mean

# NA = eksik değer (SPSS'teki "missing").
mean(c(5, 7, NA))                # Eksik varsa sonuç NA olur
mean(c(5, 7, NA), na.rm = TRUE)  # na.rm = TRUE: eksikleri dışarıda bırak


# ---- Bölüm 5: Paketler ---------------------------------------------

# Paket = R'a yeni fonksiyonlar ekleyen eklenti.
# KURULUM "genelde" bir kez yapılır. Gerekirse yeniden kurulur:
install.packages(c("tidyverse", "janitor", "gtsummary"))


# YÜKLEME her yeni R oturumunda yapılır:
library(readxl)   # Excel dosyası okuma
library(readr)    # CSV dosyası okuma
library(haven)    # SPSS (.sav), Stata, SAS dosyaları
library(dplyr)    # Veri yönetimi (Oturum 2'de ayrıntılı)


# ---- Bölüm 6: Veri okuma: aynı veri ama üç farklı format  ----------

# Veri: Kurgusal bir ilçede 18 yaş üstü 1.500 yetişkinde hipertansiyon taraması yapalım.
# Değişkenlerin anlamları: veri/veri_sozlugu.xlsx

# 6a. Excel dosyası
ht_excel <- read_excel("veri/ht_tarama.xlsx")

# 6b. CSV dosyası
ht_csv <- read_csv("veri/ht_tarama.csv")
# Not: Türkçe Excel'de "CSV (noktalı virgülle ayrılmış)" diye kaydedilen
# dosyalarda ayraç ";" ve ondalık "," olur. Türkçe ";" ayraç için read_csv2() kullanın.

# 6c. SPSS dosyası
ht_spss <- read_sav("veri/ht_tarama.sav")

# Üç nesne de Environment panelinde görünmeli. Boyutları aynı mı?
dim(ht_excel)   # satır sayısı, sütun sayısı
dim(ht_csv)
dim(ht_spss)


# ---- Bölüm 7: Veriye ilk bakış -------------------------------------

head(ht_excel)      # İlk 6 satır
names(ht_excel)     # Değişken adları
str(ht_excel)       # Yapı: her değişkenin türü (num = sayı, chr = metin)
glimpse(ht_excel)   # str()'nin daha okunaklı hali (dplyr paketi)
summary(ht_excel)   # Sayısal değişkenlerin özeti

View(ht_excel)      # SPSS'teki Data View gibi tablo görünümü açar

# Tek bir değişkene erişim: veri$degisken
mean(ht_excel$yas)
summary(ht_excel$kilo_kg)               # NA's satırı = eksik değer sayısı
table(ht_excel$cinsiyet)                # Frekans
table(ht_excel$egitim, useNA = "ifany") # Eksikleri de göster

# DİKKAT: Excel'den gelen metin kategoriler alfabetik sıralanır ("Lise",
# "Ortaokul"dan önce gelir). SPSS dosyasında sıra doğrudur. İleriki oturumlarda
# factor() ile sırayı kendimiz belirleyeceğiz.

round(prop.table(table(ht_excel$cinsiyet)) * 100, 1)  # Yüzde

# Çapraz tablo (SPSS: Analyze > Descriptive Statistics > Crosstabs)
table(ht_excel$cinsiyet, ht_excel$sigara) # X ve Y eksenlerine dikkat.


# ---- Bölüm 8: SPSS dosyalarında etiketler --------------------------

# SPSS dosyasında kategoriler sayı olarak gelir, etiketler (value) saklanır:
table(ht_spss$cinsiyet)        # 1 ve 2 görürsünüz
print_labels(ht_spss$cinsiyet) # 1 = Erkek, 2 = Kadın

# Etiketli sayıları R'ın kategorik veri türüne (factor) çevirelim:
ht_spss_f <- as_factor(ht_spss)
table(ht_spss_f$cinsiyet)      # Artık Erkek / Kadın

# Değişken etiketi (SPSS Variable View'daki "Label" sütunu) korunur:
attr(ht_spss_f$sks, "label")

# İlk hızlı grafikler (Oturum 3'te ggplot2 ile güzelleştireceğiz)
hist(ht_excel$yas,
     main = "Yaş dağılımı", xlab = "Yaş (yıl)", ylab = "Kişi sayısı",
     col = "lightblue")

?hist # fonksiyon hakkında merak ettiklerimizi her zaman R'a sorabiliriz.

boxplot(sks ~ cinsiyet, data = ht_excel,
        xlab = "Cinsiyet", ylab = "Sistolik kan basıncı (mmHg)")


# ---- Bölüm 9: Tek komutla veri özeti: modern paketler --------------

# Bölüm 7'de summary() ve table() ile değişkenlere tek tek baktık. Aşağıdaki
# paketler bütün veri setini tek komutla özetler. Yeni bir veriyle
# karşılaştığınızda ilk beş dakikada yapılacak iş bizce budur.
#

# Paketler kurulu değilse satırın başındaki "# " işaretini silip BİR KEZ
# çalıştırın
# install.packages(c("summarytools", "datawizard", "naniar", "gtExtras", "svglite", "DataExplorer"))


# 9a. summarytools: bütün veri tek sayfada
#     (SPSS: Analyze > Reports > Codebook ile Frequencies'in birleşimi gibi)
library(summarytools)
# Mac'te bu satır hata verirse önce XQuartz kurun: https://www.xquartz.org

st_options(lang = "tr")         # Tablo başlıkları Türkçe olsun
stview(dfSummary(ht_spss_f))    # Viewer panelinde bir HTML sayfası açılır
# Her değişken için tek satır: SPSS etiketi, değerler ve yüzdeleri, küçük bir
# grafik, geçerli (dolu) ve eksik gözlem sayısı.
# SORU: boy_cm satırında en küçük (min) ve en büyük (max) değere bakın. 1,68 cm
#       ya da 268 cm boyunda bir yetişkin olabilir mi? (Oturum 2'de düzelteceğiz.)
# SORU: Eksik sütununda hangi değişkenler öne çıkıyor?


# Gruplara göre özet (SPSS: Data > Split File): sistolik KB, cinsiyete göre
stby(ht_spss_f$sks, ht_spss_f$cinsiyet, descr, stats = "common")

# 9b. datawizard: SPSS çıktısına benzeyen tablolar
# Yazım: paket::fonksiyon(). Paketi library() ile yüklemeden tek bir
# fonksiyonunu kullanır. LLM'lerin yazdığı kodda bu yazımı sık görürsünüz.

# Frekans tablosu (SPSS: Analyze > Descriptive Statistics > Frequencies)
#   Raw %   = eksikler dahil yüzde      (SPSS: Percent)
#   Valid % = eksikler çıkarılarak yüzde (SPSS: Valid Percent)
datawizard::data_tabulate(ht_spss_f, "egitim")

# Çapraz tablo, satır yüzdeleriyle (SPSS: Crosstabs > Cells > Row)
datawizard::data_tabulate(ht_spss_f, "cinsiyet", by = "ht_tani",
                          proportions = "row", remove_na = TRUE)

# 9c. Eksik veri: kaç tane, kimlerde? (SPSS: Analyze > Missing Value Analysis)
library(naniar)    # Eksik veri özetleri ve grafikleri
library(ggplot2)   # Grafik paketi (Oturum 3'te ayrıntılı); naniar grafikleri bunu kullanır
# "The following object is masked" mesajı hata değildir: iki pakette aynı adlı
# bir fonksiyon var ve son yüklenen paketinki kullanılır.



miss_var_summary(ht_spss_f)   # Değişken başına eksik sayısı ve yüzdesi
vis_miss(ht_spss_f)           # Plots panelinde: her siyah çizgi bir eksik hücre
# SORU: sks, dks ve kilo_kg sütunlarındaki çizgiler aynı satırlarda mı?
#       Bu, eksiklerin nasıl oluştuğu hakkında ne söyler?



# Eksikler sağlık merkezlerine göre farklı mı? Renk = o merkezde % eksik
gg_miss_fct(ht_spss_f, fct = saglik_merkezi)

# SORU: Hangi ASM'de, hangi değişkende sorun var?

# Yaş ve kilo: eksik kilolar kırmızı noktalarla eksenin altında gösterilir,
# her ASM ayrı panelde
ggplot(ht_spss_f,
       aes(x = yas,
           y = kilo_kg)) +
  geom_miss_point() +
  facet_wrap(~saglik_merkezi)
# SORU: Eksik kilolar hangi yaşlarda yoğun? ASM-4'te farklı olan ne?



# 9d. Grafikli özet tablo (Viewer panelinde açılır)
gtExtras::gt_plt_summary(ht_spss_f)
# SORU: boy_cm'in grafiği neden tek bir çizgiye benziyor? (İpucu: 9a)

# 9e. Evde deneyin: tek satırla otomatik HTML rapor. Proje klasörüne
# ht_rapor.html dosyasını kaydeder ve tarayıcıda açar (10-20 saniye sürer).
# DataExplorer::create_report(ht_spss_f, output_file = "ht_rapor.html")




# ---- Bölüm 10: LLM köşesi: hata mesajlarını okumak -----------------

# Aşağıdaki satırların başındaki "# " işaretini silip TEK TEK çalıştırın.
# Her hata mesajını kopyalayıp bir LLM'e (ChatGPT, Claude, Gemini) sorun:
#
#   "R'da şu kodu çalıştırdım: [kod]. Şu hatayı aldım: [hata mesajı].
#    Ne anlama geldiğini yeni başlayan birine açıkla ve düzeltilmiş kodu ver."
#
# KURAL: Sohbete gerçek katılımcı verisi (satırlar, isimler, kimlik
# bilgileri) YAPIŞTIRMAYIN. Kod ve hata mesajı yeterlidir.

# Hata 1: Büyük-küçük harf
# Mean(ht_excel$yas)

# Hata 2: Paket adı yanlış yazılmış (ya da paket kurulmamış)
# library(gtsummry)

# Hata 3: Dosya yolu eksik (veri/ klasörü unutulmuş)
# read_excel("ht_tarama.xlsx")

# LLM'e verinizi anlatmanın güvenli yolu: içeriği değil YAPIYI paylaşın.
names(ht_excel)   # Bu çıktıyı paylaşabilirsiniz; veri satırlarını değil.

# Kodu açıklatma istemi (kopyalayıp LLM'e yapıştırın):
#   "Aşağıdaki R kodunu, istatistik bilen ama R bilmeyen birine satır satır
#    açıkla: round(prop.table(table(ht_excel$cinsiyet)) * 100, 1)"



# ---- Bölüm 11: Bonus: tek görselde veri öyküsü (patchwork) ---------

# Bu bölüm bir gösteridir. Kodun ayrıntısını Oturum 2 (dplyr) ve Oturum 3
# (ggplot2) sonunda rahatça okuyabileceksiniz; şimdilik çalıştırıp sonucu
# inceleyin. patchwork paketi birden çok ggplot grafiğini tek sayfada birleştirir:
#   p1 | p2  -> yan yana        p1 / p2  -> alt alta
# Kurulu değilse: install.packages("patchwork")

library(ggplot2)
library(naniar)
library(patchwork)

# Grafiklerde kullanacağımız yardımcı değişkenler
ht_g <- ht_spss_f |>
  mutate(yas_grubu = cut(yas, breaks = c(17, 29, 44, 59, 85),
                         labels = c("18-29", "30-44", "45-59", "60+")),
         pazartesi = format(tarama_tarihi, "%u") == "1",
         # Hatalı boylar (Bölüm 9a) BKİ hesabına girmesin
         bki = ifelse(boy_cm > 100 & boy_cm < 230, kilo_kg / (boy_cm / 100)^2, NA),
         bki_grubu = cut(bki, breaks = c(0, 25, 30, Inf), right = FALSE,
                         labels = c("<25", "25-29,9", "30+")))

# Bütün grafiklerde ortak görünüm
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        legend.position = "top")

# Eksik yüzdesini bir gruplama değişkenine göre hesaplayan küçük yardımcı
eksik_yuzde <- function(veri, grup) {
  veri |>
    group_by({{ grup }}) |>
    summarise(`Kan basıncı` = mean(is.na(sks)) * 100,
              Kilo          = mean(is.na(kilo_kg)) * 100,
              `Eğitim`      = mean(is.na(egitim)) * 100) |>
    tidyr::pivot_longer(c(`Kan basıncı`, Kilo, `Eğitim`),
                        names_to = "degisken", values_to = "yuzde") |>
    mutate(degisken = factor(degisken, levels = c("Kan basıncı", "Kilo", "Eğitim")))
}
virgul <- function(x) format(round(x, 1), nsmall = 1, decimal.mark = ",")

# A. Günlük kayıt sayısı; kesikli çizgiler resmi tatiller
tatil <- data.frame(tarih = as.Date(c("2026-03-20", "2026-04-23", "2026-05-01", "2026-05-19")),
                    ad    = c("Ramazan Bayramı", "23 Nisan", "1 Mayıs", "19 Mayıs"))
pA <- ht_g |>
  count(tarama_tarihi, pazartesi) |>
  ggplot(aes(x = tarama_tarihi, y = n, fill = pazartesi)) +
  geom_col(width = 0.75) +
  geom_vline(data = tatil, aes(xintercept = tarih), linetype = "dashed", colour = "grey60") +
  geom_text(data = tatil, aes(x = tarih, y = 53, label = ad), inherit.aes = FALSE,
            size = 3, colour = "grey35") +
  scale_fill_manual(values = c(`FALSE` = "#9ec5f4", `TRUE` = "#1c5cab"),
                    labels = c("Diğer günler", "Pazartesi"), name = NULL) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, 56), breaks = seq(0, 50, 10)) +
  labs(title = "Günlük kayıt sayısı", x = NULL, y = "Kayıt") +
  tema

pA

# B. Sağlık merkezine göre % eksik (ısı haritası, hücrede değer yazılı)
pB <- eksik_yuzde(ht_g, saglik_merkezi) |>
  ggplot(aes(x = saglik_merkezi, y = degisken, fill = yuzde)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = virgul(yuzde), colour = yuzde > 12), size = 3.5) +
  scale_fill_gradient(low = "#cde2fb", high = "#104281", guide = "none") +
  scale_colour_manual(values = c(`FALSE` = "#0b0b0b", `TRUE` = "white"), guide = "none") +
  scale_y_discrete(limits = rev) +
  labs(title = "Sağlık merkezine göre % eksik", x = NULL, y = NULL) +
  tema + theme(panel.grid = element_blank())

pB

# C. Yaş grubuna göre % eksik
pC <- eksik_yuzde(ht_g, yas_grubu) |>
  ggplot(aes(x = yas_grubu, y = yuzde, fill = degisken)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_text(aes(label = virgul(yuzde)), position = position_dodge(width = 0.85),
            vjust = -0.4, size = 2.7, colour = "grey25") +
  scale_fill_manual(values = c(`Kan basıncı` = "#2a78d6", Kilo = "#eb6834", `Eğitim` = "#1baf7a"),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Yaş grubuna göre % eksik", x = "Yaş grubu", y = "% eksik") +
  tema + theme(panel.grid.major.x = element_blank())

pC

# D. Yaş ve sistolik KB; eksik KB'ler eksenin altında turuncu
pD <- ggplot(ht_g, aes(x = yas, y = sks)) +
  geom_miss_point(size = 1.2, alpha = 0.7) +
  scale_colour_manual(values = c(Missing = "#eb6834", `Not Missing` = "#86b6ef"),
                      labels = c("Eksik", "Ölçülmüş"), name = NULL) +
  labs(title = "Yaş ve sistolik KB: eksikler nerede?", x = "Yaş (yıl)", y = "Sistolik KB (mmHg)") +
  tema

pD


# E. Daha önce HT tanısı alanlarda ve almayanlarda ölçülen sistolik KB
pE <- ht_g |>
  filter(!is.na(sks)) |>
  ggplot(aes(x = ht_tani, y = sks)) +
  geom_boxplot(fill = "#cde2fb", colour = "#1c5cab", width = 0.5, outlier.size = 1) +
  geom_hline(yintercept = 140, linetype = "dashed", colour = "grey45") +
  annotate("text", x = 0.45, y = 141, label = "140 mmHg", hjust = 0, vjust = 0,
           size = 3, colour = "grey30") +
  labs(title = "HT tanısına göre ölçülen sistolik KB",
       x = "Daha önce konmuş HT tanısı", y = "Sistolik KB (mmHg)") +
  tema

pE


# F. Diyabet ve HT tanısı sıklığı: yaş ve BKİ grubuna göre
oran <- ht_g |>
  filter(!is.na(bki_grubu)) |>
  group_by(yas_grubu, bki_grubu) |>
  summarise(Diyabet     = mean(diyabet == "Var") * 100,
            `HT tanısı` = mean(ht_tani == "Var") * 100, .groups = "drop") |>
  tidyr::pivot_longer(c(Diyabet, `HT tanısı`), names_to = "durum", values_to = "yuzde")
pF <- ggplot(oran, aes(x = yas_grubu, y = yuzde, colour = bki_grubu, group = bki_grubu)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_text(data = filter(oran, yas_grubu == "60+", durum == "Diyabet"), aes(label = bki_grubu),
            hjust = -0.3, size = 3, colour = "grey25") +
  facet_wrap(~durum) +
  scale_colour_manual(values = c("<25" = "#86b6ef", "25-29,9" = "#2a78d6", "30+" = "#104281"),
                      name = "BKİ") +
  scale_x_discrete(expand = expansion(add = c(0.3, 0.9))) +
  labs(title = "Diyabet ve HT tanısı: yaş ve BKİ grubuna göre", x = "Yaş grubu", y = "%") +
  tema

pF

# Şimdi altı paneli patchwork ile tek görselde birleştirelim:
# A en üstte, B-C ve D-E yan yana, F en altta
pA / (pB | pC) / (pD | pE) / pF +
  plot_annotation(title = "Hipertansiyon taraması: veriye tek bakış",
                  subtitle = "Sentetik eğitim verisi, 1.500 yetişkin",
                  tag_levels = "A")
# Plots panelinde küçük görünür: Zoom düğmesiyle büyütün. Dosyaya kaydetmek için:
# ggsave("ht_ozet.png", width = 12, height = 14, dpi = 150)
