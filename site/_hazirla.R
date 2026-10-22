# Quarto derlemesinden önce çalışır (_quarto.yml: pre-render).
# Ders klasöründeki dosyaları siteye kopyalar; asıl kaynak her zaman repo kökündeki
# dosyalardır, buradaki kopyalar derleme sırasında yeniden üretilir (.gitignore'da).

kok <- normalizePath("..")
kurs_dosyalari <- c("R_egitim.Rproj", "OKUBENI.txt", "oturum1_ilk_adim.R",
                    "oturum2_veri_temizligi.R", "seminer_verileri.R")
# Script'ler çalıştırılınca veri/ klasörüne kaydedilen dosyalar: siteye ve zip'e girmez
uretilen_veri <- c("ht_tarama_oturum2.rds", "ht_tarama_oturum2.csv", "ht_temizlik_kararlari.csv")

# Her oturumun iki sayfası script'ten üretilir: "Kodlar ve çıktıları" ve "Tarayıcıda deneyin"
oturumlar <- list(
  list(script = "oturum1_ilk_adim.R", kod_basi = "_oturum1_kod_basi.qmd", kod = "oturum1-kod.qmd",
       dene_basi = "_oturum1_dene_basi.qmd", dene = "dene.qmd", alistirma = "_oturum1_alistirmalar.R"),
  list(script = "oturum2_veri_temizligi.R", kod_basi = "_oturum2_kod_basi.qmd", kod = "oturum2-kod.qmd",
       dene_basi = "_oturum2_dene_basi.qmd", dene = "oturum2-dene.qmd", alistirma = "_oturum2_alistirmalar.R")
)

yenile <- function(klasor) {
  unlink(klasor, recursive = TRUE)
  dir.create(klasor, recursive = TRUE, showWarnings = FALSE)
}
veri_dosyalari <- function() {
  d <- list.files(file.path(kok, "veri"), full.names = TRUE)
  d[!basename(d) %in% uretilen_veri]
}

# 1. veri/: sayfalar ve tarayıcıdaki R (webR) bu kopyayı okur
yenile("veri")
invisible(file.copy(veri_dosyalari(), "veri"))

# 2. indir/: sitedeki indirme bağlantıları (tek tek script'ler ve klasörün zip'i)
yenile("indir")
invisible(file.copy(file.path(kok, grep("\\.R$", kurs_dosyalari, value = TRUE)), "indir"))
gecici <- file.path(tempdir(), "R_egitim")
unlink(gecici, recursive = TRUE)
dir.create(file.path(gecici, "veri"), recursive = TRUE)
invisible(file.copy(file.path(kok, kurs_dosyalari), gecici))
invisible(file.copy(veri_dosyalari(), file.path(gecici, "veri")))
eski <- setwd(dirname(gecici))
utils::zip(file.path(eski, "indir", "R_egitim.zip"), "R_egitim", flags = "-rq9X")
setwd(eski)

# 3. Oturum sayfaları: sabit sayfa başı + script'ten dönüştürülen gövde.
#    Sayfalar tamamen burada üretilir; böylece bölüm başlıkları içindekiler menüsüne girer.
source("_donustur.R")
source("_alistirma.R")
for (o in oturumlar) {
  script <- file.path(kok, o$script)
  writeLines(c(readLines(o$kod_basi, encoding = "UTF-8"), "", script_to_rmd(script)),
             o$kod, useBytes = TRUE)
  source(o$alistirma)   # alistirma_listesi
  writeLines(c(readLines(o$dene_basi, encoding = "UTF-8"), "",
               script_to_live(script, alistirmalar_md(alistirma_listesi), kod_sayfasi = o$kod)),
             o$dene, useBytes = TRUE)
}

cat("Site için dosyalar hazırlandı:", length(list.files("veri")), "veri dosyası, R_egitim.zip,",
    length(oturumlar), "oturumun sayfaları\n")
