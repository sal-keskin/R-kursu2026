# "Tarayıcıda deneyin" sayfalarındaki "Sıra sizde" alıştırmalarını Markdown'a çevirir.
# Her oturumun alıştırmaları kendi dosyasındadır (_oturum1_alistirmalar.R, _oturum2_alistirmalar.R).
# Kod içinde {{cevap}} ya da {{cevap1|cevap2}} yazılan yerler sayfada giriş kutusu olur.
# Katılımcı kutuya yazar, Enter'a ya da Çalıştır'a basar; kod webR'da çalışır ve cevap kontrol edilir.
# Alıştırmalar, adı verilen bölümün sonuna eklenir (bolum = script'teki bölüm kimliği: 1, "1b" gibi).
# Cevaplarda tırnak işareti (") kullanmayın; giriş kutusunun cevap listesi bozulur.

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

# Bölüm kimliğine göre adlandırılmış liste: her öğe o bölümün sonuna eklenecek Markdown
alistirmalar_md <- function(liste) {
  sonuc <- list()
  for (i in seq_along(liste)) {
    a <- liste[[i]]
    anahtar <- as.character(a$bolum)
    sonuc[[anahtar]] <- c(sonuc[[anahtar]], "", alistirma_md(a, i))
  }
  sonuc
}
