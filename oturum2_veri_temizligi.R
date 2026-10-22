
#  ================================================= R ve LLM ile Halk Sağlığında Veri Analizi =================================================
#  Oturum 2 (23 Ekim 2026): Dağınık veriyi temizlemek
#  Hacettepe Üniversitesi Tıp Fakültesi Halk Sağlığı AD
#  Epidemiyoloji Bilim Dalı

#  BU OTURUMDA ------------------------------------------------------------
#  - Kod YAZMAYI değil, kodu TANIMAYI öğreniyoruz.
#  - Sorun çıktığında LLM'e doğru soruyu sormayı ve gelen kodu
#    KONTROL etmeyi öğreniyoruz.
#
#  NASIL KULLANILIR? ------------------------------------------------------
#  - Ctrl + Enter (Mac: Cmd + Enter) imlecin olduğu satırı çalıştırır.
#  - "LLM köşesi" bloklarında bir İSTEM ve bir ÖRNEK CEVAP var.
#    Örnek cevaplar eğitmenler tarafından hazırlanmış TEMSİLİ cevaplardır;
#    sizin kullandığınız LLM farklı bir kod yazabilir.
#  - Veri SENTETİKTİR (eğitim için üretilmiştir), gerçek kişilere ait değildir.


# ---- Bölüm 0: Geçen haftadan ---------------------------------------

# SORU: Çalıştırmadan önce tahmin edin. Bu satırlar ne yapar?
yaslar <- c(34, 51, NA, 45, 72)
mean(yaslar, na.rm = TRUE)
sum(yaslar >= 45, na.rm = TRUE)

# Bugünün sorusu: Sahadan gelen veride yaş böyle yazılmışsa R ne yapar?
# Tahmininizi sohbete yazın. Cevabı Bölüm 4'te birlikte göreceğiz.
# as.numeric(c("52", "45 yaş", "otuz sekiz"))

# Bugünkü paketler (hepsi geçen hafta tidyverse ve janitor ile kuruldu)
library(readxl)     # Excel okuma
library(readr)      # parse_number(), write_csv()
library(dplyr)      # veri yönetimi
library(stringr)    # metin işlemleri
library(lubridate)  # tarihler
library(janitor)    # temizlik yardımcıları
library(tidyr)      # pivot_longer(): Bölüm 1b'deki tanıtım kodu için
library(haven)      # SPSS (.sav) dosyaları
# "masked" mesajları hata değildir (Oturum 1). janitor'un chisq.test() ve
# fisher.test() fonksiyonları R'ınkilerle aynı sonucu verir.


# ---- Bölüm 0b: Farklı dosya türlerini R'a yüklemek ------------------

# Veri her zaman Excel'de gelmez. Dosya türüne göre okuma fonksiyonu
# değişir, sonuç aynıdır: R'da bir veri tablosu.
#
#   Dosya          Fonksiyon       Paket
#   .xlsx / .xls   read_excel()    readxl
#   .csv           read_csv()      readr   (ayraç ";" ise read_csv2())
#   .sav (SPSS)    read_sav()      haven   (Stata .dta için read_dta())

# Aynı taramanın düzenlenmiş halini üç farklı dosyadan okuyalım:
#ht_excel <- read_excel("veri/ht_tarama.xlsx")
#ht_csv   <- read_csv("veri/ht_tarama.csv")
#ht_spss  <- read_sav("veri/ht_tarama.sav")

# ---- CSV dosyasında sorun yaşarsanız: ayraç, ondalık, kodlama

# CSV düz bir metin dosyasıdır. Okumadan önce ilk satırlarına bakın:
readLines("veri/ht_tarama.csv", n = 3)
# Değerler arasında ne var: virgül (,) mı, noktalı virgül (;) mi?
# Ondalıklar nasıl yazılmış: 103.9 mu, 103,9 mu?
# Türkçe harfler (ş, ı, ğ) düzgün görünüyor mu?

# read_csv() bunları varsayar: ayraç ",", ondalık ".", kodlama UTF-8.
# Aynı ayarları açıkça yazan, her CSV'ye uyarlanabilen yazım:
ht_csv <- read_delim("veri/ht_tarama.csv",
                     delim  = ",",                       # ayraç
                     locale = locale(decimal_mark = ".", # ondalık işareti
                                     encoding = "UTF-8"))# karakter kodlaması
dim(ht_csv)

# Türkçe Excel'de "CSV (noktalı virgülle ayrılmış)" diye kaydedilen dosya
# genellikle böyle okunur (örnek; bu klasörde böyle bir dosya yok):
# read_delim("dosya.csv",
#            delim  = ";",
#            locale = locale(decimal_mark = ",", encoding = "windows-1254"))
#
#   Belirti                                  Olası neden        Çözüm
#   Bütün veri TEK sütunda                   ayraç yanlış       delim = ";"
#   103,9 kilo 1039 olarak okundu (uyarısız!) ondalık yanlış    decimal_mark = ","
#   "İçiyor" yerine "��iyor", "Ä°Ã§iyor"     kodlama yanlış     encoding = "windows-1254"
#                                                               ya da "UTF-8"
#
# UTF-8: Türkçe harfler dahil bütün dillerin harflerini saklayabilen,
# bugün en yaygın karakter kodlaması. Mümkünse Excel'de dosyayı
# "CSV UTF-8" seçeneğiyle kaydedin.

# read_csv() her sütunu hangi türde okuduğunu kendisi raporlar.

# Üçü de Environment panelinde. Boyutları aynı mı?
#dim(ht_excel)
#dim(ht_csv)
#dim(ht_spss)

# SPSS dosyası kategorileri sayı + etiket olarak saklar (geçen hafta görmüştük):
#head(ht_spss$cinsiyet)

# Bu üç dosya DÜZENLENMİŞ dosyalar. Sahadan veri genellikle böyle gelmez.
# Bugün aynı taramanın sahadan, ASM'lerden geldiği haliyle çalışacağız.


# ---- Bölüm 1: Ham veriyi okumak ------------------------------------

# Aynı tarama, ama sahadan geldiği haliyle: ASM'lerde elle girilmiş Excel.
# Önce hiçbir ayar yapmadan okuyalım:
ht_ilk <- read_excel("veri/ht_tarama_ham.xlsx")

dim(ht_ilk)     # satır, sütun
names(ht_ilk)   # Değişken adları nerede?

# Not: Excel dosyasını bir kez açıp bakın. Sorunu çoğu zaman gözünüzle
#      R'dan önce görürsünüz.

# ---- LLM köşesi 1: "Verim neden böyle okundu?" ----
#
# İSTEM:
#   "R'da read_excel() ile bir Excel dosyası okudum. names() çıktısı şöyle:
#    [1] "2026 Bahar Dönemi Hipertansiyon Taraması - ..." [2] "...2" [3] "...3"
#    ... [14] "...14"
#    Değişken adları neden böyle göründü? Nasıl düzeltirim? Yeni başlayan
#    birine açıkla; tidyverse kullan."
#
# ÖRNEK CEVAP (temsili):
#   "Excel dosyanızın ilk satırlarında başlık ya da açıklama var; R ilk
#    satırı değişken adı sanmış. Excel'i açıp gerçek değişken adlarının
#    KAÇINCI SATIRDA olduğuna bakın. Örneğin 3. satırdaysa üstteki 2 satırı
#    atlayın:
#      read_excel("dosya.xlsx", skip = 2)
#    Ardından janitor::clean_names() ile adları R'a uygun hale getirin."
#
# LLM dosyanızı göremez; eksik bilgi verirseniz tahmin eder. Başlıkların
# hangi satırda olduğunu yalnızca siz görebilirsiniz.
# Gizlilik: LLM'e veri satırlarını değil, names() / dim() çıktısını verdik.

ht_ham <- read_excel("veri/ht_tarama_ham.xlsx", skip = 2)
# Bir uyarı çıktı ("TOPLAM"). Şimdilik not edin, Bölüm 2'de döneceğiz.
names(ht_ham)   # "Sıra No", "Boy (cm)": boşluk, parantez, Türkçe harf

ht_ham <- clean_names(ht_ham)
names(ht_ham)   # sira_no, boy_cm: küçük harf, alt çizgi, Türkçe harf yok


# ---- Bölüm 1b: Veriyi göstermeden LLM'e tanıtmak --------------------

# Temizliğe başlamadan önce veride hangi sorunlar var, bilmek istiyoruz.
# LLM'den yardım alabiliriz, ama veriyi ona GÖSTEREMEYİZ.
# Çözüm: LLM'den veriyi tanıtacak KODU isteriz, kodu KENDİ bilgisayarımızda
# çalıştırırız, LLM'e yalnızca ÖZET çıktıyı veririz.

# ---- LLM köşesi 2: "Verimi göstermeden tanıtmak istiyorum" ----
#
# İSTEM:
#   "R'da ht_ham adında bir veri setim var (bir tarama verisi). Veriyi sana
#    gösteremem. Temizlikten önce veriyi tanımak için çalıştırabileceğim
#    R kodu yaz. Çıktıyı sana yapıştıracağım; bu yüzden çıktıda tek tek
#    satırlar ya da kişiyi tanımlayabilecek değerler OLMASIN.
#    Görmek istediklerim:
#    1) her değişkenin türü, eksik sayısı ve kaç farklı değeri olduğu,
#    2) birebir tekrar eden satır sayısı,
#    3) az kategorili metin sütunlarında (en fazla 15 farklı değer)
#       hangi değerlerin kaçar kez geçtiği,
#    4) sayı olması gereken ama metin olarak okunan sütunlarda sayıya
#       dönmeyen değerler,
#    5) sayısal sütunların en küçük ve en büyük değerleri.
#    tidyverse ve |> kullan, her bloğu kısaca açıkla."
#
# ÖRNEK CEVAP (temsili):
#   "Aşağıdaki kod hiçbir satırı tek tek göstermez; yalnızca özet üretir."

# 1. Her değişken: türü, eksik sayısı, kaç farklı değeri var
#    (değerlerin kendisini göstermez)
tibble(degisken     = names(ht_ham),
       tur          = sapply(ht_ham, function(x) class(x)[1]),
       eksik        = colSums(is.na(ht_ham)),
       farkli_deger = sapply(ht_ham, n_distinct))

# GÖRSEL KONTROL: Hangi değişkende kaç eksik var? (Plots panelinde açılır)
# Grafik yalnızca sizin ekranınızda; LLM'e gitmez.
dotchart(sort(colSums(is.na(ht_ham))),
         main = "Değişken başına eksik hücre sayısı", xlab = "Eksik hücre")

# SORU: En çok eksik hangi üç değişkende?
#       Neredeyse bütün değişkenlerde görünen 2 eksik nereden geliyor?

# 2. Birebir tekrar eden satır sayısı
sum(duplicated(ht_ham))

# 3. Az kategorili metin sütunları: değerler ve sıklıkları.
#    15'ten fazla farklı değeri olan sütunlar (ad, adres, kimlik no gibi)
#    bu sınır sayesinde kendiliğinden dışarıda kalır.
ht_ham |>
  select(where(function(x) is.character(x) && n_distinct(x) <= 15)) |>
  pivot_longer(everything(), names_to = "degisken", values_to = "deger") |>
  count(degisken, deger) |>
  print(n = Inf)

# 4. Sayı olması gereken ama metin okunan sütunlar: sayıya dönmeyen değerler.
#    "sayi_gibi": değerlerinin %90'ından fazlası sayıya dönebilen metin sütunu
sayi_gibi <- function(x) {
  is.character(x) && mean(!is.na(suppressWarnings(as.numeric(na.omit(x))))) > 0.9
}
ht_ham |>
  select(where(sayi_gibi)) |>
  pivot_longer(everything(), names_to = "degisken", values_to = "deger") |>
  filter(!is.na(deger), is.na(suppressWarnings(as.numeric(deger)))) |>
  count(degisken, deger)

# 5. Sayısal sütunların aralığı
ht_ham |> select(where(is.numeric)) |> summary()

# ÇIKTIYI LLM'E YAPIŞTIRMADAN ÖNCE KENDİNİZ OKUYUN:
#   - Bir kişinin adı, kimlik numarası, adresi, telefonu görünüyor mu?
#   - Çok nadir bir değer (1-2 kez geçen) birini tanıtabilir mi?
#   Gerekirse değeri yazmak yerine sorunu anlatın:
#   "yaş sütununda birimle yazılmış değerler var" gibi.

# SORU: Bu çıktıda hangi sorunları görüyorsunuz? Oturumun yol haritası:
#   - cinsiyet: 8 farklı yazım                       -> Bölüm 3
#   - egitim_durumu: alfabetik sıra                  -> Bölüm 3
#   - yas, kilo_kg, sistolik_kb metin olarak okunmuş -> Bölüm 4
#   - tarama_tarihi: sayı ve metin karışık           -> Bölüm 5
#   - boy_cm: en küçük 1.68, en büyük 268            -> Bölüm 6
#   - 7 tekrar eden satır (6 kişi + boş satırlar)    -> Bölüm 2 ve 7
# Profil her şeyi göstermez: sistolik KB metin olduğu için 1293 değeri
# burada görünmüyor. Kontroller her adımda sürecek.


# ---- Bölüm 2: Pipe ( |> ) ve dplyr fiilleri ------------------------

# |> işaretini "ve sonra" diye okuyun. Kısayol: Ctrl + Shift + M
#   (Mac: Cmd + Shift + M). %>% çıkıyorsa: Tools > Global Options > Code >
#   "Use native pipe operator" kutusunu işaretleyin. LLM'ler sık sık %>%
#   yazar; ikisi bu oturumdaki kodlarda aynı işi görür.
#   ht_ham |> count(asm)   =   "ht_ham'ı al, ve sonra asm'yi say"
#
#   Bugün sürekli kullanacağımız dört fiil:
#   Fiil        Ne yapar?                  SPSS karşılığı
#   count()     sayar                      Frequencies
#   filter()    satır seçer                Select Cases
#   select()    sütun seçer                Data View'da yalnızca istenen sütunlar
#   mutate()    yeni/değişen sütun         Compute / Recode
#   (arrange() sıralar: Sort Cases. rename() ve distinct() yeri gelince.)

# SORU: Her satırı çalıştırmadan önce ne çıkacağını tahmin edin.
ht_ham |> count(asm)              # NA 2: not edin, birazdan döneceğiz

ht_ham |> filter(asm == "ASM-4") |> nrow()

ht_ham |> select(sira_no, asm, yas, cinsiyet) |> head()

ht_ham |> mutate(boy_m = boy_cm / 100) |> select(boy_cm, boy_m) |> head(3)

ht_ham |> arrange(desc(boy_cm)) |> select(sira_no, boy_cm) |> head(3)
# En uzun boy 268 cm mi? Not edin, Bölüm 6'da döneceğiz.

# Hata örneği: başındaki "# " işaretini silip çalıştırın, mesajı okuyun.
#ht_ham |> filter(asm = "ASM-4")
# Fonksiyon içinde "=" bir argümana değer verir; "==" karşılaştırır ("eşit mi?").

# Pipe olmadan aynı iş: içten dışa okunur, zor.
head(select(filter(ht_ham, asm == "ASM-4"), sira_no, yas))
# Pipe ile: soldan sağa, yukarıdan aşağıya okunur.
ht_ham |>
  filter(asm == "ASM-4") |>
  select(sira_no, yas) |>
  head()

# Bölüm 1'deki uyarıya dönelim: "Expecting numeric in A1511: got 'TOPLAM'"
# R, sıra numarası sütununda sayı yerine "TOPLAM" yazısını görmüş.
tail(ht_ham, 3)   # Son iki satır tamamen boş (NA)

# KONTROL: 1.500 kişi bekliyorduk. Kaç satır var?
nrow(ht_ham)
# Fark nereden geliyor? Boş satır, TOPLAM satırı ve... (Bölüm 7'de göreceğiz)

# İlk temizlik adımı: boş satırları at, adları geçen haftaki verideki gibi yap.
# Yeni nesne: ht. Ham veri (ht_ham) olduğu gibi kalır, ona dokunmayız.
ht <- ht_ham |>
  filter(!is.na(sira_no)) |>          # sıra numarası boş olanlar: boş + TOPLAM
  # rename(): YENİ ad = ESKİ ad. SPSS'teki RENAME VARIABLES (eski = yeni)
  # sırasının tersidir.
  rename(id             = sira_no,
         saglik_merkezi = asm,
         egitim         = egitim_durumu,
         fiz_aktivite   = fiziksel_aktivite,
         sks            = sistolik_kb,
         dks            = diastolik_kb,
         ht_tani        = ht_tanisi)

nrow(ht)
glimpse(ht)   # <chr> = metin, <dbl> = sayı. yas, kilo_kg, sks neden metin?
# Not: kilo_kg'de "73.09999999999999" görürseniz şaşırmayın; Excel 73,1'i
#      bilgisayarın içinde böyle saklar. Sayıya çevirince 73.1 olur.


# ---- Bölüm 3: Kategorik değişkenler --------------------------------

# Bugünün çalışma kalıbı:  DENE -> KONTROL ET -> UYGULA
#   1. DENE        : değişikliği yeni bir sütunda, "deneme" nesnesinde yapın
#   2. KONTROL ET  : eski ve yeni sütunu YAN YANA sayın
#   3. UYGULA      : sonuç doğruysa aynı değişikliği ht'ye uygulayın

ht |> count(cinsiyet)   # Kaç farklı yazım var?

# ---- LLM köşesi 3: kategorileri birleştirmek ----
#
# İSTEM:
#   "R'da bir veri setinde cinsiyet sütununu count() ile saydım:
#      E 25, ERKEK 22, Erkek 664, K 17, KADIN 17, Kadın 728, erkek 13,
#      kadın 20
#    Bunları 'Erkek' ve 'Kadın' olarak birleştirmek istiyorum. dplyr ve |>
#    kullanarak kod yaz, her satırı açıkla."
#
# ÖRNEK CEVAP (temsili):
#   "Önce hepsini Baş Harfi Büyük yapalım, sonra tek harfleri çevirelim:"

# 1. DENE
deneme <- ht |>
  mutate(cinsiyet_yeni = str_to_title(cinsiyet),
         cinsiyet_yeni = case_when(cinsiyet_yeni == "E" ~ "Erkek",
                                   cinsiyet_yeni == "K" ~ "Kadın",
                                   .default = cinsiyet_yeni))

# 2. KONTROL ET: Her eski yazım nereye gitti?
deneme |> count(cinsiyet, cinsiyet_yeni)

# GÖRSEL KONTROL: Kaç çubuk bekliyoruz? Kaç tane var?
barplot(table(deneme$cinsiyet_yeni), main = "LLM'in kodundan sonra cinsiyet", col = "orange")
# SORU: "KADIN" neden "Kadin" oldu? (İpucu: büyük I harfinin küçüğü
#       İngilizcede i, Türkçede ı'dır.)
# Kod hata vermedi ama sonuç yanlış. Hatayı count() yakaladı, LLM değil.

# Daha açık bir yol: hangi yazımın neye dönüşeceğini tek tek yazmak.
# %in% = "bu listedekilerden biri mi?"
# 1. DENE
deneme <- ht |>
  mutate(cinsiyet_yeni = case_when(
    cinsiyet %in% c("Erkek", "ERKEK", "erkek", "E") ~ "Erkek",
    cinsiyet %in% c("Kadın", "KADIN", "kadın", "K") ~ "Kadın"))

# 2. KONTROL ET
deneme |> count(cinsiyet, cinsiyet_yeni)
# Listede olmayan yeni bir yazım çıksaydı NA olurdu ve burada görünürdü.

# GÖRSEL KONTROL: önce ve sonra yan yana
par(mfrow = c(1, 2))     # Plots panelini ikiye böl: iki grafik yan yana
barplot(table(deneme$cinsiyet), las = 2, main = "Önce: 8 yazım", col = "thistle")
barplot(table(deneme$cinsiyet_yeni), main = "Sonra: 2 kategori", col = "burlywood")
par(mfrow = c(1, 1))     # tek grafiğe geri dön

# 3. UYGULA
ht <- ht |>
  mutate(cinsiyet = case_when(
    cinsiyet %in% c("Erkek", "ERKEK", "erkek", "E") ~ "Erkek",
    cinsiyet %in% c("Kadın", "KADIN", "kadın", "K") ~ "Kadın"))
ht |> count(cinsiyet)   # Yalnızca Erkek ve Kadın

# Not: Türkçe metinde büyük/küçük harf çevirirken locale = "tr" ekleyin:
#      str_to_title("KADIN", locale = "tr")  ->  "Kadın"

# Eğitim: alfabetik sıra mantıklı değil. SPSS te 1234 olarak kodlu olmasına rağmen R da alfabetik (geçen hafta "factor() ile
# düzelteceğiz" demiştik)

ht |> count(egitim)

# 1. DENE (bilerek hatalı: "İlkokul" yerine "Ilkokul" yazdık)
deneme <- ht |>
  mutate(egitim_yeni = factor(egitim,
                              levels = c("Ilkokul ve altı", "Ortaokul",
                                         "Lise", "Üniversite")))

# 2. KONTROL ET
deneme |> count(egitim, egitim_yeni)
# 403 kişi sessizce NA oldu! factor() listede bulamadığı değeri NA yapar,
# uyarı da vermez.

# 3. UYGULA (doğru yazımla)
ht <- ht |>
  mutate(egitim = factor(egitim,
                         levels = c("İlkokul ve altı", "Ortaokul",
                                    "Lise", "Üniversite")))
ht |> count(egitim)   # Sıra doğru; NA hâlâ 30 (önce de 30'du)

# GÖRSEL KONTROL: Grafikteki sıra da değişti mi?
par(mfrow = c(1, 2))
barplot(table(ht_ham$egitim_durumu), las = 2, cex.names = 0.7, col = "thistle",
        main = "Önce: bilgisayarın sırası")
barplot(table(ht$egitim), las = 2, cex.names = 0.7, col ="burlywood",
        main = "Sonra: eğitim sırası")
par(mfrow = c(1, 1))

colors()
# ---- Bölüm 4: Sayı olması gereken sütunda metin ---------------------

# Bölüm 0'daki sorunun cevabı. Önce üç değerle deneyelim:
as.numeric(c("52", "45 yaş", "otuz sekiz"))     # 52 NA NA
# Uyarı: "NAs introduced by coercion" = "Sayıya çeviremediklerimi NA yaptım."
parse_number(c("52", "45 yaş", "otuz sekiz"))   # 52 45 NA
# parse_number() metnin içindeki sayıyı bulur; "otuz sekiz"i o da okuyamaz.

# DİKKAT: parse_number() bulduğu İLK sayıyı alır, uyarı vermez:
parse_number(c("45-50", "1,68 m"))              # 45 ve 168!
# Bu yüzden önce hangi değerlerin sayıya dönmediğine bakın, sonra kullanın.

# Sayısal sütunlarda kalıp: ÖNCE "ne kaybolacak?", SONRA summary() ile NA.

# yas, kilo_kg ve sks metin (<chr>) olarak okunmuştu (Bölüm 2, glimpse).
# ÖNCE: Sayıya dönmeyecek yaş değerleri hangileri?
ht |>
  filter(is.na(as.numeric(yas)), !is.na(yas)) |>
  select(id, yas)
# Uyarı hata değildir ama BİLGİDİR. Burada bize 3 kaydı gösterdi.

# Karar: "otuz sekiz" = 38. Kalan ikisini parse_number() güvenle okur.
ht <- ht |>
  mutate(yas = if_else(yas == "otuz sekiz", "38", yas),
         yas = parse_number(yas))
# SONRA:
summary(ht$yas)   # NA yok, 18-85 arası

# GÖRSEL KONTROL: Yaş dağılımı akla yatkın mı?
hist(ht$yas, main = "Yaş dağılımı", xlab = "Yaş (yıl)", ylab = "Kişi sayısı",
     col = "sienna3")

# ÖNCE: Kilo ve sistolik KB'de sayıya dönmeyecek değerler neler?
ht |> filter(is.na(as.numeric(kilo_kg)), !is.na(kilo_kg)) |> count(kilo_kg)
ht |> filter(is.na(as.numeric(sks)),     !is.na(sks))     |> count(sks)

# İkisi de "ölçülmemiş" demek. Bunları BİLEREK NA yapalım; uyarı kalmasın:
ht <- ht |>
  mutate(kilo_kg = na_if(kilo_kg, "-"),
         kilo_kg = as.numeric(kilo_kg),
         sks     = na_if(sks, "ölçülmedi"),
         sks     = as.numeric(sks))
# SONRA:
glimpse(ht)                            # Hepsi artık <dbl>
summary(ht |> select(kilo_kg, sks))    # NA'lar: boş hücreler + 8 "-" / 4 "ölçülmedi"


# ---- Bölüm 5: Tarihler ---------------------------------------------

ht |> select(id, tarama_tarihi) |> head(3)   # "46083" bir tarih mi?

# Excel tarihi aslında bir SAYIDIR: 30.12.1899'dan bu yana geçen gün sayısı.
excel_numeric_to_date(46083)

# Ama 3 hücreye tarih elle, metin olarak yazılmış. Bir sütunda tek bir metin
# bile olsa R bütün sütunu metin okur:
ht |> filter(str_detect(tarama_tarihi, "\\.")) |> select(id, tarama_tarihi)

# ---- LLM köşesi 4: karışık tarih sütunu ----
#
# İSTEM:
#   "R'da Excel'den okuduğum tarama_tarihi sütunu karakter. Değerlerin çoğu
#    '46083' gibi Excel seri numarası, birkaçı '05.03.2026' (gün.ay.yıl)
#    biçiminde metin. Hepsini Date türüne çevirmek istiyorum. dplyr ve |>
#    kullan."
#
# ÖRNEK CEVAP (temsili):
#   "Sayıları Excel tarihine çevirin, çevrilemeyenleri dmy() ile okuyun,
#    coalesce() ile ikisini birleştirin:"
deneme <- ht |>
  mutate(tarih_deneme = coalesce(excel_numeric_to_date(as.numeric(tarama_tarihi)),
                                 dmy(tarama_tarihi)))

# KONTROL: Boş tarih kaldı mı?
sum(is.na(deneme$tarih_deneme))   # 3! Kod çalıştı ama 3 tarih kayboldu.
# Nedeni: dmy() uzun bir sütunda biçimi bir örneklemden tahmin eder; örnekleme
# metin tarihler düşmedi. Aynı kod 10 satırlık bir denemede doğru çalışır.
# Kontrolü her zaman bütün veride yapın.

# Doğru yol: convert_to_date() sayıları ve metinleri ayrı ayrı çevirir.
ht <- ht |>
  mutate(tarama_tarihi = convert_to_date(tarama_tarihi, character_fun = dmy))
sum(is.na(ht$tarama_tarihi))   # 0

# KONTROL: Tarama 2 Mart - 22 Mayıs 2026 arasında yapıldı.
range(ht$tarama_tarihi)

# GÖRSEL KONTROL: Ay ay kaç kişi tarandı?
barplot(table(format(ht$tarama_tarihi, "%Y-%m")), las = 2, col = "steelblue",
        ylab = "Kişi sayısı", main = "Ay ay taranan kişi sayısı")
# SORU: En sağdaki etiket ne? Neden üstünde çubuk görünmüyor?
ht |> filter(tarama_tarihi > as.Date("2026-05-22")) |> select(id, tarama_tarihi)

# Karar: 2062, 2026'nın rakamları yer değiştirmiş hali. 15.05.2026 tarama
# dönemi içinde; başka akla yatkın yorum yok, düzeltiyoruz.
ht <- ht |>
  mutate(tarama_tarihi = if_else(tarama_tarihi == as.Date("2062-05-15"),
                                 as.Date("2026-05-15"), tarama_tarihi))
range(ht$tarama_tarihi)   # "2026-03-02" "2026-05-22"

# GÖRSEL KONTROL: Düzeltmeden sonra
barplot(table(format(ht$tarama_tarihi, "%Y-%m")), las = 2, col = "steelblue",
        ylab = "Kişi sayısı", main = "Düzeltmeden sonra")

# ---- Bölüm 6: Akla yatkın olmayan değerler -------------------------

ht |> select(yas, boy_cm, kilo_kg, sks, dks) |> summary()
# SORU: Hangi Min ya da Max değeri bir insan için mümkün değil?

# GÖRSEL KONTROL: Kutu grafiğinde uç değerler nokta olarak görünür
par(mfrow = c(1, 2))
boxplot(ht$boy_cm, main = "Boy (cm)", col="blue")
boxplot(ht$sks, main = "Sistolik KB (mmHg)", col="red")
par(mfrow = c(1, 1))
# SORU: Kutular neden ezilmiş, çizgi gibi görünüyor?

ht |>
  filter(boy_cm < 100 | boy_cm > 230 | sks > 300) |>
  select(id, saglik_merkezi, boy_cm, kilo_kg, sks, dks)

# ---- LLM köşesi 5: "Bu değerleri nasıl düzelteyim?" ----
#
# İSTEM:
#   "Bir hipertansiyon taramasında boy (cm) değerleri arasında 1.68, 17.2 ve
#    268; sistolik kan basıncı (mmHg) değerleri arasında 1293 ve 1348 var.
#    Bunları R'da nasıl düzeltirim?"
#
# ÖRNEK CEVAP (temsili):
#   "Bunlar veri giriş hataları. Şöyle düzeltebilirsiniz:
#    1.68 -> 168 (metre girilmiş), 17.2 -> 172 (virgül kaymış),
#    268 -> 168 (2 yerine 1 yazılmak istenmiş), 1293 -> 129.3, 1348 -> 134.8"
#
# SORU: Bu önerilerin hangisine güvenirsiniz?
#   - 1.68 ve 17.2: Tek bir akla yatkın yorum var.
#   - 268: 168 mi, 186 mı, 178 mi? Bilemeyiz.
#   - 1293: 129 mu, 130 mu, 93 mü? Kan basıncı ondalıklı ölçülmez; 129.3 olamaz.
# Bu karar LLM'in değil, sizin kararınız. Gerçek hayatta en doğrusu:
# ASM'ye sormak, kayıt defterine bakmak.
#
# Kuralımız: Tek bir yorum varsa düzelt; yoksa NA yap. Kararı yaz.

ht <- ht |>
  mutate(boy_cm = case_when(boy_cm < 3   ~ boy_cm * 100,   # metre girilmiş
                            boy_cm < 30  ~ boy_cm * 10,    # virgül kaymış
                            boy_cm > 230 ~ NA,             # yorumlanamaz
                            .default = boy_cm),
         sks    = if_else(sks > 300, NA, sks))             # yorumlanamaz

ht |> select(boy_cm, sks) |> summary()

# GÖRSEL KONTROL: Düzeltmeden sonra
par(mfrow = c(1, 2))
boxplot(ht$boy_cm, main = "Boy (cm)" , col="blue")
boxplot(ht$sks, main = "Sistolik KB (mmHg)", col= "red")
par(mfrow = c(1, 1))
# DİKKAT: Hâlâ noktalar var (ör. sks 180'ler). Kutu grafiğindeki nokta
# "aykırı" demektir, "hatalı" değil. 185 mmHg bir insanda ölçülebilir;
# 1293 ölçülemez. Hangisinin hata olduğuna grafik değil, siz karar verirsiniz.


# ---- Bölüm 7: Tekrar eden kayıtlar ve kaydetmek ---------------------

ht |> get_dupes(id)    # Aynı sıra numarası iki kez girilmiş mi?
# SPSS: Data > Identify Duplicate Cases

# Satırların her sütunu aynı mı? Aynıysa biri fazladan girilmiş demektir.
ht |> get_dupes()

ht <- ht |> distinct()
nrow(ht)                     # 1.500
n_distinct(ht$id) == nrow(ht)  # TRUE: her kişi bir kez

# Kararlarımızın kaydı. Altı ay sonra siz de bunu arayacaksınız.
kararlar <- tribble(
  ~degisken,       ~sorun,                         ~karar,
  "satırlar",      "boş satır ve TOPLAM satırı",   "silindi",
  "satırlar",      "6 kayıt iki kez girilmiş",     "tekrarlar silindi",
  "cinsiyet",      "8 farklı yazım",               "Erkek / Kadın",
  "egitim",        "alfabetik sıra",               "factor, eğitim sırası",
  "yas",           "'45 yaş', '52 y', 'otuz sekiz'", "45, 52, 38",
  "kilo_kg / sks", "'-' ve 'ölçülmedi'",           "NA",
  "tarama_tarihi", "Excel sayısı + metin tarih",   "Date türüne çevrildi",
  "tarama_tarihi", "id 1368: 15.05.2062",          "15.05.2026",
  "boy_cm",        "1.68 ve 17.2",                 "168 ve 172",
  "boy_cm",        "268",                          "NA",
  "sks",           "1293 ve 1348",                 "NA"
)
kararlar

# Kaydetme: ham dosyanın ÜZERİNE YAZMAYIN.
saveRDS(ht, "veri/ht_tarama_oturum2.rds")     # R için: factor ve tarih korunur
write_csv(ht, "veri/ht_tarama_oturum2.csv")   # Başka programlar için
write_csv(kararlar, "veri/ht_temizlik_kararlari.csv")   # Karar kaydı, verinin yanında


# ---- Bölüm 8: Hepsi bir arada --------------------------------------

# Bugün adım adım yaptığımız her şey, tek bir blokta aşağıda.
# Her satırı artık tanıyorsunuz.
# Kod yeniden kullanılır; KONTROLLER her seferinde yeniden yapılır.

ht_temiz <- read_excel("veri/ht_tarama_ham.xlsx", skip = 2) |>
  clean_names() |>
  filter(!is.na(sira_no)) |>
  rename(id = sira_no, saglik_merkezi = asm, egitim = egitim_durumu,
         fiz_aktivite = fiziksel_aktivite, sks = sistolik_kb,
         dks = diastolik_kb, ht_tani = ht_tanisi) |>
  mutate(
    cinsiyet = case_when(
      cinsiyet %in% c("Erkek", "ERKEK", "erkek", "E") ~ "Erkek",
      cinsiyet %in% c("Kadın", "KADIN", "kadın", "K") ~ "Kadın"),
    egitim   = factor(egitim, levels = c("İlkokul ve altı", "Ortaokul",
                                         "Lise", "Üniversite")),
    yas      = parse_number(if_else(yas == "otuz sekiz", "38", yas)),
    kilo_kg  = as.numeric(na_if(kilo_kg, "-")),
    sks      = as.numeric(na_if(sks, "ölçülmedi")),
    tarama_tarihi = convert_to_date(tarama_tarihi, character_fun = dmy),
    tarama_tarihi = if_else(tarama_tarihi == as.Date("2062-05-15"),
                            as.Date("2026-05-15"), tarama_tarihi),
    boy_cm   = case_when(boy_cm < 3   ~ boy_cm * 100,
                         boy_cm < 30  ~ boy_cm * 10,
                         boy_cm > 230 ~ NA,
                         .default = boy_cm),
    sks      = if_else(sks > 300, NA, sks)
  ) |>
  distinct()

# KONTROL: Adım adım yaptığımızla aynı sonuç mu?
all.equal(ht_temiz, ht)

# Temiz veriyle ilk grafik: eğitim sırası grafikte de doğru.
# (Oturum 3'te ggplot2 ile güzelleştireceğiz.)
boxplot(sks ~ egitim, data = ht_temiz, las = 1, cex.axis = 0.8, col="lightblue",
        xlab = "Eğitim", ylab = "Sistolik KB (mmHg)",
        main = "Eğitime göre sistolik kan basıncı")
# DİKKAT: Bu bir ilişki gösterir, neden değil. Eğitimi düşük olanlar daha
# yaşlı olabilir; yaş hem eğitimle hem kan basıncıyla ilişkili (karıştırıcı).


# ---- LLM'e soru sormak için şablon ---------------------------------
#
#   1. BAĞLAM  : "R'a yeni başladım. Halk sağlığı verisiyle çalışıyorum."
#   2. YAPI    : names(), glimpse() ya da count() çıktısı.
#                Veri satırlarını, isimleri, kimlik bilgilerini YAPIŞTIRMAYIN.
#   3. SORUN   : Ne yaptım, ne bekliyordum, ne oldu? Hata mesajı aynen.
#   4. İSTEK   : "dplyr ve |> kullan, her satırı açıkla."
#   5. SINIR   : "Emin olmadığın değerleri tahminle düzeltme, NA yap ve listele."
#
#   Cevabı aldıktan sonra: ÇALIŞTIR, sonra SAY (count, summary, nrow).
#   Kod hata vermediyse, bu doğru çalıştığı anlamına gelmez.
