# Quarto derlemesinden önce çalışır (_quarto.yml: pre-render).
# Ders klasöründeki dosyaları siteye kopyalar; asıl kaynak her zaman repo kökündeki
# dosyalardır, buradaki kopyalar derleme sırasında yeniden üretilir (.gitignore'da).

kok <- normalizePath("..")
kurs_dosyalari <- c("R_egitim.Rproj", "OKUBENI.txt", "oturum1_ilk_adim.R", "seminer_verileri.R")

yenile <- function(klasor) {
  unlink(klasor, recursive = TRUE)
  dir.create(klasor, recursive = TRUE, showWarnings = FALSE)
}

# 1. veri/: sayfalar ve tarayıcıdaki R (webR) bu kopyayı okur
yenile("veri")
file.copy(list.files(file.path(kok, "veri"), full.names = TRUE), "veri")

# 2. indir/: sitedeki indirme bağlantıları (tek tek dosyalar ve klasörün zip'i)
yenile("indir")
file.copy(file.path(kok, c("oturum1_ilk_adim.R", "seminer_verileri.R")), "indir")
gecici <- file.path(tempdir(), "R_egitim")
unlink(gecici, recursive = TRUE)
dir.create(gecici)
file.copy(file.path(kok, kurs_dosyalari), gecici)
file.copy(file.path(kok, "veri"), gecici, recursive = TRUE)
eski <- setwd(dirname(gecici))
utils::zip(file.path(eski, "indir", "R_egitim.zip"), "R_egitim", flags = "-rq9X")
setwd(eski)

# 3. "Kod ve çıktılar" sayfası: sabit sayfa başı + script'ten dönüştürülen gövde.
#    Sayfa tamamen burada üretilir; böylece bölüm başlıkları içindekiler menüsüne girer.
source("_donustur.R")
writeLines(c(readLines("_kod_sayfasi_basi.qmd", encoding = "UTF-8"), "",
             script_to_rmd(file.path(kok, "oturum1_ilk_adim.R"))),
           "oturum1-kod.qmd", useBytes = TRUE)

# 4. "Tarayıcıda deneyin" sayfası: sabit sayfa başı + script'in webR'lı hali + "Sıra sizde" alıştırmaları
source("_dene_alistirmalar.R")
writeLines(c(readLines("_dene_basi.qmd", encoding = "UTF-8"), "",
             script_to_live(file.path(kok, "oturum1_ilk_adim.R"), alistirmalar_md())),
           "dene.qmd", useBytes = TRUE)

cat("Site için dosyalar hazırlandı:", length(list.files("veri")), "veri dosyası, R_egitim.zip\n")
