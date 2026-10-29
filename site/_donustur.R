# Ders script'ini (.R) site sayfalarına dönüştürür.
#   script_to_rmd()  -> "Kodlar ve çıktıları" sayfası (knitr ile çalıştırılır)
#   script_to_live() -> "Tarayıcıda deneyin" sayfası (webR ile tarayıcıda çalışır)
# Ortak kurallar:
#   - "# ---- Bölüm N: Başlık ----" satırları sayfa başlığı (##) olur (N: 0, 1, 1b gibi)
#   - Diğer "# ---- Başlık" satırları: sonunda uzun çizgi varsa ##, yoksa ara başlık (###)
#     (ör. "# ---- LLM köşesi 1: ... ----")
#   - Boş satırlar kod bloklarını ayırır; yorumlar kodun içinde aynen kalır
#   - Bir blok tek başına R kodu olarak ayrıştırılamıyorsa sonraki blokla birleştirilir

baslik_re <- "^# ---- \\S"
bolum_re  <- "^Bölüm ([0-9]+[a-z]?):.*$"

# Başlık satırının metni ve düzeyi (2 = bölüm, 3 = ara başlık)
baslik_bilgisi <- function(s) {
  metin <- sub("\\s*-*\\s*$", "", sub("^# ---- ", "", s))
  son_cizgi <- nchar(sub("^.*?(-*)\\s*$", "\\1", s, perl = TRUE))
  list(metin = metin, duzey = if (grepl(bolum_re, metin) || son_cizgi >= 10) 2 else 3)
}

# Script'i başlık ve kod paragraflarına ayırır
script_parcalari <- function(yol) {
  satir <- readLines(yol, encoding = "UTF-8", warn = FALSE)
  ilk <- grep(baslik_re, satir)[1]
  satir <- satir[ilk:length(satir)]          # dosya başındaki "NASIL KULLANILIR" kutusu atlanır

  parcalar <- list(); tampon <- character()
  bosalt <- function() {
    if (length(tampon)) parcalar[[length(parcalar) + 1]] <<- list(tur = "kod", satir = tampon)
    tampon <<- character()
  }
  for (s in satir) {
    if (grepl(baslik_re, s)) {
      bosalt()
      parcalar[[length(parcalar) + 1]] <- c(list(tur = "baslik"), baslik_bilgisi(s))
    } else if (!nzchar(trimws(s))) {
      bosalt()
    } else {
      tampon <- c(tampon, s)
    }
  }
  bosalt()

  birlesik <- list(); bekleyen <- NULL
  ayristir <- function(x) !inherits(try(parse(text = x), silent = TRUE), "try-error")
  for (p in parcalar) {
    if (p$tur == "baslik") {
      if (!is.null(bekleyen)) { birlesik[[length(birlesik) + 1]] <- bekleyen; bekleyen <- NULL }
      birlesik[[length(birlesik) + 1]] <- p
      next
    }
    bekleyen <- if (is.null(bekleyen)) p else list(tur = "kod", satir = c(bekleyen$satir, "", p$satir))
    if (ayristir(bekleyen$satir)) { birlesik[[length(birlesik) + 1]] <- bekleyen; bekleyen <- NULL }
  }
  if (!is.null(bekleyen)) birlesik[[length(birlesik) + 1]] <- bekleyen
  birlesik
}

# Grafik içeren bloklar için boyut (inç)
grafik_boyutu <- function(kod) {
  tek <- trimws(paste(kod, collapse = " "))
  if (any(grepl("plot_annotation\\(", kod))) c(12, 14)
  else if (tek %in% c("pA", "pF")) c(10, 4.4)
  else if (tek %in% c("pB", "pC", "pD", "pE")) c(7.5, 4.6)
  else if (any(grepl("^grafik_kor", kod))) c(8.5, 7)               # Oturum 3 korelasyon ısı haritaları
  else if (any(grepl("ggcoef_table", kod))) c(9, 4.5)               # Oturum 3 orman grafiği
  else if (any(grepl("facet_wrap|vis_miss", kod))) c(9, 5.5)
  else if (any(grepl("par\\(mfrow = c\\(1, 2\\)\\)", kod))) c(9, 4.8)
  else NULL
}

# Bölüm kimliği ("1", "1b"); bölüm olmayan başlıklarda NA
bolum_no <- function(baslik) if (grepl(bolum_re, baslik)) sub(bolum_re, "\\1", baslik) else NA_character_

baslik_satiri <- function(p) paste(strrep("#", p$duzey), p$metin)

# Blok "# |>" gibi bir satırla başlıyorsa Quarto onu blok ayarı (#|) sanar.
# Yalnız yorumdan oluşan böyle bloklar düz kod olarak gösterilir; kod içerenlerin başına boş bir yorum satırı eklenir.
ayar_gibi <- function(kod) grepl("^#\\s*\\|", kod[1])

# Yorum satırlarıyla çizilmiş ASCII resim (ör. Oturum 3'teki MOLA ve "OTURUMUN SONU"): düz metin olarak,
# satırları kaydırılmadan ve renklendirilmeden gösterilir
ascii_sanat <- function(kod) {
  all(grepl("^\\s*#", kod)) && length(kod) >= 5 && any(grepl("={20,}|-{5,}\\+|#{2,}\\s+#{2,}", kod))
}

# "# Hata N:" ya da "# Hata örneği:" bloğundaki, yorum işareti silinince çalışacak kod satırları
hatali_satirlar <- function(kod) {
  bas <- grepl("^# Hata( [0-9]+| örneği)?:", kod)
  if (!any(bas)) return(character())
  aday <- sub("^#\\s?", "", kod[!bas & grepl("^#", kod)])
  kod_mu <- vapply(aday, function(x) grepl("(", x, fixed = TRUE) &&
                     !inherits(try(parse(text = x), silent = TRUE), "try-error"), logical(1))
  aday[kod_mu]
}

# ---- "Kodlar ve çıktıları" ----
#   - Yardım satırları (?mean) gösterilir ama çalıştırılmaz
#   - "# Hata N" örneklerinin altına, yorum işareti silinince R'ın verdiği hata mesajı eklenir
#   - ekler: list(list(desen = "...", md = "...")); kodu desene uyan bloğun altına md eklenir
#     (ör. Oturum 3'te PDF'e yazılan rapor sayfalarını sayfada göstermek için)
script_to_rmd <- function(yol, ekler = list()) {
  cikti <- character()
  for (p in script_parcalari(yol)) {
    if (p$tur == "baslik") {
      cikti <- c(cikti, "", baslik_satiri(p), "")
      next
    }
    kod <- p$satir
    if (ascii_sanat(kod)) { cikti <- c(cikti, "```{.text .ascii-sanat}", kod, "```", ""); next }
    if (ayar_gibi(kod)) {
      if (all(grepl("^\\s*#", kod))) { cikti <- c(cikti, "```r", kod, "```", ""); next }
      kod <- c("#", kod)
    }
    secenek <- character()
    if (any(grepl("^\\?", kod))) secenek <- c(secenek, "#| eval: false")
    boyut <- grafik_boyutu(kod)
    if (!is.null(boyut)) secenek <- c(secenek, sprintf("#| fig-width: %s", boyut[1]), sprintf("#| fig-height: %s", boyut[2]))
    cikti <- c(cikti, "```{r}", secenek, kod, "```", "")

    hatali <- hatali_satirlar(kod)
    if (length(hatali)) {
      cikti <- c(cikti, "::: {.hata-ornegi}",
                 "Satırın başındaki `#` silinip çalıştırılınca R şu mesajı verir:", "",
                 "```{r}", "#| error: true", "#| echo: false", hatali, "```", ":::", "")
    }
    for (ek in ekler) if (any(grepl(ek$desen, kod))) cikti <- c(cikti, ek$md, "")
  }
  paste(cikti, collapse = "\n")
}

# ---- "Tarayıcıda deneyin" ----
#   - Çalıştırılabilir satır içeren her blok webR kod kutusu olur; yalnız yorumdan oluşan bloklar
#     ve yardım satırları (?mean) düz kod olarak gösterilir
#   - Tarayıcıda çalışmayan satırlar (paket kurma, summarytools, gtExtras) yorum satırına çevrilir
#   - alistirmalar: bölüm kimliğine göre ("1", "1b"), o bölümün sonuna eklenecek "Sıra sizde" metinleri
#   - kod_sayfasi: tarayıcıda çalışmayan satırlar için yönlendirilecek "Kodlar ve çıktıları" sayfası
tarayicida_calismaz <- "library\\(summarytools\\)|st_options\\(|stview\\(|dfSummary\\(|stby\\(|gtExtras::"

# Desene uyan bütün ifadeleri (birden çok satıra yayılanlar dahil) yorum satırına çevirir.
# Oturuma özgü tarayıcıda çalışmayan satırlar için (ör. Oturum 3: ggsave, Word, PDF raporu).
ifadeleri_yorumla <- function(kod, desen) {
  ifade <- tryCatch(parse(text = kod, keep.source = TRUE), error = function(e) NULL)
  if (is.null(ifade) || !length(ifade)) return(list(kod = kod, degisti = FALSE))
  sr <- attr(ifade, "srcref")
  degisti <- FALSE
  for (i in seq_along(ifade)) {
    satirlar <- sr[[i]][1]:sr[[i]][3]
    if (any(grepl(desen, kod[satirlar]))) {
      yorum <- grepl("^\\s*#", kod[satirlar])
      kod[satirlar[!yorum]] <- paste0("# ", kod[satirlar[!yorum]])
      degisti <- TRUE
    }
  }
  list(kod = kod, degisti = degisti)
}

#   - calismaz: oturuma özgü desen; uyan ifadeler bütünüyle yorum satırına çevrilir
#   - ekler: list(list(desen, md)); kodu desene uyan bloğun altına md eklenir (ör. ek bir webR kutusu)
script_to_live <- function(yol, alistirmalar = list(), kod_sayfasi = "oturum1-kod.qmd",
                           calismaz = NULL, ekler = list()) {
  cikti <- character()
  simdiki <- NA_character_
  ekle_alistirma <- function(no) {
    if (!is.na(no) && !is.null(alistirmalar[[no]])) cikti <<- c(cikti, alistirmalar[[no]], "")
  }
  for (p in script_parcalari(yol)) {
    if (p$tur == "baslik") {
      if (p$duzey == 2) {
        ekle_alistirma(simdiki)
        simdiki <- bolum_no(p$metin)
      }
      cikti <- c(cikti, "", baslik_satiri(p), "")
      next
    }
    kod <- p$satir
    if (ascii_sanat(kod)) { cikti <- c(cikti, "```{.text .ascii-sanat}", kod, "```", ""); next }
    yorum_mu <- grepl("^\\s*#", kod)
    not <- FALSE
    kurulum <- !yorum_mu & grepl("^install\\.packages\\(", kod)
    kod[kurulum] <- paste0("# ", kod[kurulum], "   # tarayıcıda paket kurmaya gerek yok")
    satir_calismaz <- !yorum_mu & grepl(tarayicida_calismaz, kod)
    if (any(satir_calismaz)) { kod[satir_calismaz] <- paste0("# ", kod[satir_calismaz]); not <- TRUE }
    if (!is.null(calismaz)) {
      y <- ifadeleri_yorumla(kod, calismaz)
      kod <- y$kod
      if (y$degisti) not <- TRUE
    }

    calisir <- !grepl("^\\s*#", kod) & nzchar(trimws(kod))
    if (!any(calisir) || any(grepl("^\\?", kod))) {
      cikti <- c(cikti, "```r", kod, "```", "")
    } else {
      if (ayar_gibi(kod)) kod <- c("#", kod)
      secenek <- character()
      boyut <- grafik_boyutu(kod)
      if (!is.null(boyut)) secenek <- c(sprintf("#| fig-width: %s", boyut[1]), sprintf("#| fig-height: %s", boyut[2]))
      cikti <- c(cikti, "```{webr}", secenek, kod, "```", "")
    }
    if (not) {
      cikti <- c(cikti, "::: {.tarayici-notu}",
                 sprintf("Yorum satırına çevrilen satırlar tarayıcıda çalışmaz. Sonuçlarını [Kodlar ve çıktıları](%s) sayfasında görebilir, RStudio'da kendiniz deneyebilirsiniz.", kod_sayfasi),
                 ":::", "")
    }
    for (ek in ekler) if (any(grepl(ek$desen, p$satir))) cikti <- c(cikti, ek$md, "")
  }
  ekle_alistirma(simdiki)
  paste(cikti, collapse = "\n")
}
