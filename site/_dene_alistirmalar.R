# "Tarayıcıda deneyin" sayfasındaki "Sıra sizde" alıştırmaları.
# Kod içinde {{cevap}} ya da {{cevap1|cevap2}} yazılan yerler sayfada giriş kutusu olur.
# Katılımcı kutuya yazar, Enter'a ya da Çalıştır'a basar; kod webR'da çalışır ve cevap kontrol edilir.
# Alıştırmalar, adı verilen bölümün sonuna eklenir (bolum = script'teki bölüm numarası).

alistirma_listesi <- list(
  list(bolum = 1, id = "atak", baslik = "Atak hızı",
       metin = "Başka bir düğünde 120 davetliden 30'u hastalanmış. Atak hızını (%) hesaplamak için boşlukları doldurun.",
       kod = "{{30}} / {{120}} * 100",
       ipucu = "Atak hızı = hasta sayısı / davetli sayısı × 100. Sonuç 25 olmalı."),
  list(bolum = 3, id = "vektor", baslik = "Vektörle sayma",
       metin = "`yaslar` vektöründe 40 yaşından küçük kaç kişi var? Boşluğa doğru karşılaştırma işaretini yazın.",
       kod = "yaslar <- c(34, 51, 67, 45, 29, 72, 58)\nsum(yaslar {{<}} 40)",
       ipucu = "\"Küçüktür\" işareti < ile yazılır. yaslar < 40 her kişi için TRUE ya da FALSE verir; sum() TRUE'ları sayar."),
  list(bolum = 4, id = "eksik", baslik = "Eksik değerle ortalama",
       metin = "Üç kişiden birinin kilosu eksik. Ortalamayı eksik değeri dışarıda bırakarak hesaplayın.",
       kod = "mean(c(70, 82, NA), {{na.rm}} = {{TRUE|T}})",
       ipucu = "Bölüm 4'teki örneğe bakın: eksikleri dışarıda bırakan argüman. Sonuç 76 olmalı."),
  list(bolum = 7, id = "frekans", baslik = "Frekans ve çapraz tablo",
       metin = "Önce sigara kullanımının frekans tablosunu, sonra cinsiyete göre diyabet çapraz tablosunu çıkarın.",
       kod = "table(ht_excel${{sigara}})\ntable(ht_excel$cinsiyet, ht_excel${{diyabet}})",
       ipucu = "Değişken adları küçük harfle yazılır. Bütün adları names(ht_excel) ile görebilirsiniz."),
  list(bolum = 8, id = "etiket", baslik = "SPSS etiketleri",
       metin = "SPSS dosyasında eğitim düzeyi sayı olarak gelir. Sayıları etiketli kategorilere çevirip tablolayın.",
       kod = "table({{as_factor|haven::as_factor}}(ht_spss$egitim))",
       ipucu = "Bölüm 8'de etiketli sayıları factor'e çeviren fonksiyonu kullandık."),
  list(bolum = 9, id = "eksiksay", baslik = "Eksik değerleri sayma",
       metin = "Kilo değişkeninde kaç eksik değer var?",
       kod = "sum(is.na(ht_excel${{kilo_kg}}))",
       ipucu = "is.na() her değer için \"eksik mi?\" diye sorar; sum() eksikleri sayar. Değişkenin adı kilo_kg."),
  list(bolum = 10, id = "hata", baslik = "Hata avı",
       metin = "İki satırdaki hataları düzeltin. R büyük-küçük harfe duyarlıdır; veri dosyaları `veri/` klasörünün içindedir.",
       kod = "{{mean}}(ht_excel$yas)\nread_excel(\"{{veri/}}ht_tarama.xlsx\")",
       ipucu = "Fonksiyonun adı küçük harfle yazılır. Dosya yolunun başına klasörün adı gelir.")
)

html_kacis <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# {{...}} işaretli kodu, giriş kutulu HTML'e ve çözümün düz koduna çevirir
alistirma_kodu <- function(kod) {
  parcalar <- regmatches(kod, gregexpr("\\{\\{[^}]*\\}\\}", kod), invert = NA)[[1]]
  html <- character(); cozum <- character(); n <- 0
  for (p in parcalar) {
    if (grepl("^\\{\\{.*\\}\\}$", p)) {
      n <- n + 1
      cevaplar <- strsplit(substr(p, 3, nchar(p) - 2), "|", fixed = TRUE)[[1]]
      genislik <- max(4, nchar(cevaplar[1]) + 2)
      json <- paste0("[", paste0("&quot;", html_kacis(gsub("\\", "\\\\", cevaplar, fixed = TRUE)), "&quot;", collapse = ","), "]")
      html <- c(html, sprintf(
        '<input type="text" class="kod-girdi" data-cevap="%s" style="width:%dch" spellcheck="false" autocomplete="off" autocapitalize="off" aria-label="Boşluk %d">',
        json, genislik, n))
      cozum <- c(cozum, cevaplar[1])
    } else {
      html <- c(html, html_kacis(p))
      cozum <- c(cozum, p)
    }
  }
  list(html = paste(html, collapse = ""), cozum = paste(cozum, collapse = ""))
}

alistirma_md <- function(a, sira) {
  k <- alistirma_kodu(a$kod)
  c(
    sprintf("::: {#dolgu-%s .dolgu}", a$id),
    sprintf("[Sıra sizde %d]{.dolgu-etiket}", sira),
    "",
    sprintf("**%s.** %s", a$baslik, a$metin),
    "",
    "```{=html}",
    sprintf('<pre class="dolgu-kod"><code>%s</code></pre>', k$html),
    '<div class="dolgu-dugmeler">',
    '  <button type="button" class="dolgu-calistir">▷ Çalıştır</button>',
    '  <button type="button" class="dolgu-cozum">Çözümü göster</button>',
    '</div>',
    '<div class="dolgu-geri" role="status" aria-live="polite"></div>',
    sprintf('<details class="dolgu-ipucu"><summary>İpucu</summary><p>%s</p></details>', html_kacis(a$ipucu)),
    "```",
    ":::",
    "",
    sprintf("::: {#calistirici-%s .dolgu-calistirici}", a$id),
    "```{webr}",
    k$cozum,
    "```",
    ":::"
  )
}

# Bölüm numarasına göre adlandırılmış liste: her öğe o bölümün sonuna eklenecek Markdown
alistirmalar_md <- function() {
  sonuc <- list()
  for (i in seq_along(alistirma_listesi)) {
    a <- alistirma_listesi[[i]]
    anahtar <- as.character(a$bolum)
    sonuc[[anahtar]] <- c(sonuc[[anahtar]], "", alistirma_md(a, i))
  }
  sonuc
}
