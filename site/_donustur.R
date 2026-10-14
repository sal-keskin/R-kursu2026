# Ders script'ini (.R) site sayfalarına dönüştürür.
#   script_to_rmd()  -> "Kodlar ve çıktıları" sayfası (knitr ile çalıştırılır)
#   script_to_live() -> "Tarayıcıda deneyin" sayfası (webR ile tarayıcıda çalışır)
# Ortak kurallar:
#   - "# ---- Bölüm N: Başlık ----" satırları sayfa başlığı (##) olur
#   - Boş satırlar kod bloklarını ayırır; yorumlar kodun içinde aynen kalır
#   - Bir blok tek başına R kodu olarak ayrıştırılamıyorsa sonraki blokla birleştirilir

baslik_re <- "^# ---- (Bölüm [0-9]+: .*?) -*\\s*$"

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
      parcalar[[length(parcalar) + 1]] <- list(tur = "baslik", metin = sub(baslik_re, "\\1", s))
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
  else if (any(grepl("facet_wrap|vis_miss", kod))) c(9, 5.5)
  else NULL
}

bolum_no <- function(baslik) as.integer(sub("^Bölüm ([0-9]+):.*$", "\\1", baslik))

# ---- "Kodlar ve çıktıları" ----
#   - Yardım satırları (?mean) gösterilir ama çalıştırılmaz
#   - "# Hata N" örneklerinin altına, yorum işareti silinince R'ın verdiği hata mesajı eklenir
script_to_rmd <- function(yol) {
  cikti <- character()
  for (p in script_parcalari(yol)) {
    if (p$tur == "baslik") {
      cikti <- c(cikti, "", paste("##", p$metin), "")
      next
    }
    kod <- p$satir
    secenek <- character()
    if (any(grepl("^\\?", kod))) secenek <- c(secenek, "#| eval: false")
    boyut <- grafik_boyutu(kod)
    if (!is.null(boyut)) secenek <- c(secenek, sprintf("#| fig-width: %s", boyut[1]), sprintf("#| fig-height: %s", boyut[2]))
    cikti <- c(cikti, "```{r}", secenek, kod, "```", "")

    if (any(grepl("^# Hata [0-9]+:", kod))) {
      hatali <- sub("^#\\s?", "", kod[!grepl("^# Hata [0-9]+:", kod) & grepl("^#\\s?[A-Za-z_.][A-Za-z0-9_.:]*\\(", kod)])
      if (length(hatali)) {
        cikti <- c(cikti, "::: {.hata-ornegi}",
                   "Satırın başındaki `#` silinip çalıştırılınca R şu mesajı verir:", "",
                   "```{r}", "#| error: true", "#| echo: false", hatali, "```", ":::", "")
      }
    }
  }
  paste(cikti, collapse = "\n")
}

# ---- "Tarayıcıda deneyin" ----
#   - Çalıştırılabilir satır içeren her blok webR kod kutusu olur; yalnız yorumdan oluşan bloklar
#     ve yardım satırları (?mean) düz kod olarak gösterilir
#   - Tarayıcıda çalışmayan satırlar (paket kurma, summarytools, gtExtras) yorum satırına çevrilir
#   - alistirmalar: bölüm numarasına göre, o bölümün sonuna eklenecek "Sıra sizde" metinleri
tarayicida_calismaz <- "library\\(summarytools\\)|st_options\\(|stview\\(|dfSummary\\(|stby\\(|gtExtras::"

script_to_live <- function(yol, alistirmalar = list()) {
  cikti <- character()
  simdiki <- NA_integer_
  ekle_alistirma <- function(no) {
    if (!is.na(no) && !is.null(alistirmalar[[as.character(no)]])) cikti <<- c(cikti, alistirmalar[[as.character(no)]], "")
  }
  for (p in script_parcalari(yol)) {
    if (p$tur == "baslik") {
      ekle_alistirma(simdiki)
      simdiki <- bolum_no(p$metin)
      cikti <- c(cikti, "", paste("##", p$metin), "")
      next
    }
    kod <- p$satir
    yorum_mu <- grepl("^\\s*#", kod)
    not <- FALSE
    kurulum <- !yorum_mu & grepl("^install\\.packages\\(", kod)
    kod[kurulum] <- paste0("# ", kod[kurulum], "   # tarayıcıda paket kurmaya gerek yok")
    calismaz <- !yorum_mu & grepl(tarayicida_calismaz, kod)
    if (any(calismaz)) { kod[calismaz] <- paste0("# ", kod[calismaz]); not <- TRUE }

    calisir <- !grepl("^\\s*#", kod) & nzchar(trimws(kod))
    if (!any(calisir) || any(grepl("^\\?", kod))) {
      cikti <- c(cikti, "```r", kod, "```", "")
    } else {
      secenek <- character()
      boyut <- grafik_boyutu(kod)
      if (!is.null(boyut)) secenek <- c(sprintf("#| fig-width: %s", boyut[1]), sprintf("#| fig-height: %s", boyut[2]))
      cikti <- c(cikti, "```{webr}", secenek, kod, "```", "")
    }
    if (not) {
      cikti <- c(cikti, "::: {.tarayici-notu}",
                 "Yorum satırına çevrilen satırlar tarayıcıda çalışmaz. Sonuçlarını [Kodlar ve çıktıları](oturum1-kod.qmd) sayfasında görebilir, RStudio'da kendiniz deneyebilirsiniz.",
                 ":::", "")
    }
  }
  ekle_alistirma(simdiki)
  paste(cikti, collapse = "\n")
}
