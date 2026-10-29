# Oturum 3 "Kodlar ve çıktıları" sayfasına eklenenler (bkz. _donustur.R: script_to_rmd(ekler = ...))
# veriye_bakis() raporunu PDF dosyasına yazar; sayfada görünmez. Raporun sayfaları, çağrının
# hemen altına sekmeler halinde resim olarak eklenir (aynı fonksiyonlar, aynı veri).

rapor_sayfasi <- function(baslik, kod) {
  c(paste("##", baslik), "",
    "```{r}", "#| echo: false", "#| fig-width: 8.27", "#| fig-height: 11.69",
    kod, "```", "")
}

rapor_sekmeleri <- function(aciklama, sayfalar) {
  c("::: {.callout-tip appearance=\"simple\"}", aciklama, ":::", "",
    "::: {.panel-tabset}", "",
    unlist(lapply(sayfalar, function(s) rapor_sayfasi(s[1], s[2]))),
    ":::")
}

ekler_listesi <- list(
  list(desen = "veriye_bakis\\(dusme, grup = dusme_12ay",
       md = rapor_sekmeleri(
         "Bu satır beş sayfalık raporu PDF dosyasına yazar. Sayfalar aşağıdaki sekmelerdedir.",
         list(c("1. Veri kimlik kartı", "invisible(veri_karti(dusme))"),
              c("2. Kategorik değişkenler", "invisible(kategorik_sayfasi(dusme))"),
              c("3. Sayısal değişkenler", "invisible(normallik_sayfasi(dusme))"),
              c("4. Sonuca göre profil", "invisible(grup_sayfasi(dusme, dusme_12ay))"),
              c("5. Sayısal değişkenler, sonuca göre", "invisible(grup_sayisal_sayfasi(dusme, dusme_12ay))")))),
  list(desen = "veriye_bakis\\(dusme, grup = yurume_hizi[,)]",
       md = rapor_sekmeleri(
         "Sonuç sürekli olduğu için yalnız 4. ve 5. sayfa değişir; ilk üç sayfa yukarıdakiyle aynıdır.",
         list(c("4. Sonuca göre profil", "invisible(grup_sayfasi(dusme, yurume_hizi, parametrik = TRUE))"),
              c("5. Sayısal değişkenler, sonuca göre", "invisible(grup_sayisal_sayfasi(dusme, yurume_hizi))"))))
)
